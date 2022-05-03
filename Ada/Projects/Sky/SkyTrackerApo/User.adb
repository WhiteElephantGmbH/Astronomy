-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Application;
with Data;
with Earth;
with Gui.Enumeration_Menu_Of;
with Gui.Registered;
with Lexicon;
with Objects;
with Os;
with Persistent;
with Site;
with Space;
with Strings;
with Targets;
with Time;
with Traces;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Version;

  Control_Page   : Gui.Page;
  Goto_Button    : Gui.Button;
  Parking_Button : Gui.Button;
  Target         : Gui.Plain_Edit_Box;
  Description    : Gui.Plain_Edit_Box;
  Display        : Gui.List_View;

  Display_Page : Gui.Page;
  Target_Ra    : Gui.Plain_Edit_Box;
  Target_Dec   : Gui.Plain_Edit_Box;
  Actual_Ra    : Gui.Plain_Edit_Box;
  Actual_Dec   : Gui.Plain_Edit_Box;
  Actual_Alt   : Gui.Plain_Edit_Box;
  Actual_Az    : Gui.Plain_Edit_Box;
  Lmst         : Gui.Plain_Edit_Box;
  Local_Time   : Gui.Plain_Edit_Box;

  type Page is (Is_Control, Is_Display);

  The_Page : Page := Is_Control;

  type Target_Selection is (No_Target, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection := Target_Object;

  The_Status : Telescope.State := Telescope.Disconnected;

  Action_Routine   : Action_Handler;
  The_Last_Action  : Action := Action'pred (Button_Action'first);
  Last_Action_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;


  procedure Signal_Action (The_Action : Action) is
    Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
    use type Ada.Real_Time.Time;
  begin
    if Action_Routine /= null then
      if The_Action in Button_Action and The_Last_Action = The_Action then
        if Now < Last_Action_Time + Ada.Real_Time.Milliseconds (500) then
          Log.Write ("Double Click on " & The_Action'img);
          return;
        end if;
      end if;
      Log.Write ("Signal: " & The_Action'img);
      Action_Routine.all (The_Action);
      Last_Action_Time := Now;
      The_Last_Action := The_Action;
    end if;
  end Signal_Action;


  procedure Set_Target_Name (Item : String) is
  begin
    if Item = "" then
      The_Target_Selection := No_Target;
    else
      Log.Write ("Target: " & Item);
      The_Target_Selection := Target_Object;
    end if;
    Gui.Set_Text (Target, Item);
    Gui.Set_Text (Description, "");
  end Set_Target_Name;


  use all type Targets.Selection;
  subtype Selection is Targets.Selection range All_Objects .. Multiple_Stars;


  function Image_Of (The_Selection : Selection) return String is

    type Names is array (Selection) of Lexicon.Word;

    Name_Of : constant Names := (All_Objects    => Lexicon.All_Objects,
                                 Solar_System   => Lexicon.Solar_System,
                                 Clusters       => Lexicon.Clusters,
                                 Open_Clusters  => Lexicon.Open_Clusters,
                                 Nebulas        => Lexicon.Nebulas,
                                 Galaxies       => Lexicon.Galaxies,
                                 Stars          => Lexicon.Stars,
                                 Multiple_Stars => Lexicon.Multiple_Stars);
  begin
    return Lexicon.Image_Of (Name_Of(The_Selection));
  end Image_Of;

  package Selection_Menu is new Gui.Enumeration_Menu_Of (Selection, Gui.Radio, Image_Of);


  procedure Selection_Handler (The_Filter : Selection) is
  begin
    Log.Write ("Filter: " & The_Filter'img);
    Targets.Set (The_Selection => The_Filter);
    Signal_Action (Update);
  exception
  when others =>
    Log.Error ("Selection_Handler");
  end Selection_Handler;


  function Image_Of (The_Selection : Data.Sky_Object) return String is
    use all type Data.Sky_Object;
  begin
    case The_Selection is
    when Favorites =>
      return Lexicon.Image_Of (Lexicon.Favorites);
    when Caldwell =>
      null;
    when Hip =>
      return "HIP";
    when Hr =>
      return "HR";
    when Messier =>
      null;
    when Ngc =>
      return "NGC";
    when Ocl =>
      return "OCl";
    when Quasars =>
      return Lexicon.Image_Of (Lexicon.Quasars);
    end case;
    return Strings.Legible_Of (The_Selection'img);
  end Image_Of;

  package Catalog_Menu is new Gui.Enumeration_Menu_Of (Data.Sky_Object, Gui.Radio, Image_Of);

  procedure Catalog_Handler (The_Catalog : Data.Sky_Object) is
  begin
    Log.Write ("Catalog: " & The_Catalog'img);
    Name.Define (The_Catalog);
    Signal_Action (Define_Catalog);
  exception
  when others =>
    Log.Error ("Catalog_Handler");
  end Catalog_Handler;


  procedure Perform_Park is
  begin
    Signal_Action (Park);
  end Perform_Park;


  procedure Perform_Unpark is
  begin
    Signal_Action (Unpark);
  end Perform_Unpark;


  Perform_Parking : not null access procedure := Perform_Unpark'access;

  The_Actual_Direction : Earth.Direction; -- used for Skyline


  procedure Show (Information : Telescope.Data) is

    procedure Set_Goto_Button is
    begin
      if The_Target_Selection = No_Target then
        Gui.Disable (Goto_Button);
      else
        Gui.Enable (Goto_Button);
      end if;
    end Set_Goto_Button;

    procedure Define_Parking_Button is
    begin
      case Information.Status is
      when Telescope.Parked =>
        Gui.Set_Text (Parking_Button, "Unpark");
        Perform_Parking := Perform_Unpark'access;
      when others =>
        Gui.Set_Text (Parking_Button, "Park");
        Perform_Parking := Perform_Park'access;
      end case;
    end Define_Parking_Button;

    procedure Clear_Actual_Values is
    begin
      Gui.Set_Text (Actual_Dec, "");
      Gui.Set_Text (Actual_Ra, "");
      Gui.Set_Text (Actual_Alt, "");
      Gui.Set_Text (Actual_Az, "");
      The_Actual_Direction := Earth.Unknown_Direction;
    end Clear_Actual_Values;

    use type Telescope.State;

  begin -- Show
    if (The_Status /= Information.Status)
      or (Last_Target_Selection /= The_Target_Selection)
    then
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      Define_Parking_Button;
      case The_Status is
      when Telescope.Disconnected
         | Telescope.Failure
         | Telescope.Homing
         | Telescope.Inconsistent
         | Telescope.Inhibited
         | Telescope.Parking
         | Telescope.Unknown
         | Telescope.Unparking
      =>
        Gui.Disable (Goto_Button);
        Gui.Disable (Parking_Button);
      when Telescope.Parked =>
        Gui.Enable (Parking_Button);
        Gui.Disable (Goto_Button);
      when Telescope.Tracking
         | Telescope.Stopped
         | Telescope.Slewing
         | Telescope.Halted
         | Telescope.Outside
         | Telescope.Following
      =>
        Set_Goto_Button;
        Gui.Enable (Parking_Button);
      end case;
      Gui.Set_Status_Line (Information.Status'img);
    end if;
    if Space.Direction_Is_Known (Information.Actual_Direction) then
      if Site.Is_Defined then
        The_Actual_Direction := Objects.Direction_Of (Information.Actual_Direction, Time.Lmst);
      else
        The_Actual_Direction := Earth.Unknown_Direction;
      end if;
    end if;
    case The_Page is
    when Is_Control =>
      null;
    when Is_Display =>
      if Space.Direction_Is_Known (Information.Target_Direction) then
        Gui.Set_Text (Target_Dec, Space.Dec_Image_Of (Information.Target_Direction));
        Gui.Set_Text (Target_Ra, Space.Ra_Image_Of (Information.Target_Direction));
      else
        Gui.Set_Text (Target_Dec, "");
        Gui.Set_Text (Target_Ra, "");
      end if;
      if Space.Direction_Is_Known (Information.Actual_Direction) then
        Gui.Set_Text (Actual_Dec, Space.Dec_Image_Of (Information.Actual_Direction));
        Gui.Set_Text (Actual_Ra, Space.Ra_Image_Of (Information.Actual_Direction));
        Gui.Set_Text (Actual_Alt, Earth.Alt_Image_Of (The_Actual_Direction));
        Gui.Set_Text (Actual_Az, Earth.Az_Image_Of (The_Actual_Direction));
      else
        Clear_Actual_Values;
      end if;
      if Information.Universal_Time = Time.In_The_Past then
        Gui.Set_Text (Lmst, "");
        Gui.Set_Text (Local_Time, "");
      else
        if Site.Is_Defined then
          Gui.Set_Text (Lmst, Time.Image_Of (Time.Lmst_Of (Information.Universal_Time)));
        else
          Gui.Set_Text (Lmst, "");
        end if;
        Gui.Set_Text (Local_Time, Time.Image_Of (Information.Universal_Time, Time_Only => True));
      end if;
    end case;
  end Show;


  procedure Set (The_Target : Name.Id) is
  begin
    Set_Target_Name (Name.Image_Of (The_Target));
    Signal_Action (Define_Target);
    Perform_Goto;
  end Set;


  procedure Clear_Target is
  begin
    Set_Target_Name ("");
  end Clear_Target;


  procedure Perform_Goto is
  begin
    Signal_Action (Go_To);
  end Perform_Goto;


  procedure Handle_Goto is
  begin
    case The_Target_Selection is
    when Target_Object =>
      Perform_Goto;
    when No_Target =>
      raise Program_Error;
    end case;
  end Handle_Goto;


  procedure Handle_Parking is
  begin
    Perform_Parking.all;
  end Handle_Parking;


  function Target_Name return String is
  begin
    return Gui.Contents_Of (Target);
  end Target_Name;


  procedure Show_Description (Image : String) is
  begin
    Gui.Set_Text (Description, Image);
  end Show_Description;

  procedure Show_Error (Image : String) is
  begin
    Gui.Message_Box (Image);
    Gui.Beep;
  end Show_Error;


  The_Targets : Name.Id_List_Access;

  procedure Enter_Control_Page is
  begin
    The_Page := Is_Control;
    Catalog_Menu.Enable;
    Selection_Menu.Enable;
    Gui.Enable_Key_Handler;
    Gui.Clear_Focus;
  end Enter_Control_Page;


  procedure Enter_Display_Page is
  begin
    The_Page := Is_Display;
    Catalog_Menu.Disable;
    Selection_Menu.Disable;
    Gui.Enable_Key_Handler;
    Gui.Clear_Focus;
  exception
  when others =>
    Log.Error ("Enter_Display_Page failed");
  end Enter_Display_Page;


  function Convertion is new Ada.Unchecked_Conversion (Gui.Information, Name.Id_Access);

  function Display_Text_Handler (For_Column       : Natural;
                                 With_Information : Gui.Information) return String is
    Name_Id : constant Name.Id_Access := Convertion (With_Information);
    use type Name.Id_Access;
  begin
    if For_Column = 0 then
      if Name_Id = null then
        return "";
      else
        return Name.Prefix_Of (Name_Id.all) & Name.Image_Of (Name_Id.all);
      end if;
    else
      raise Program_Error;
    end if;
  exception
  when others =>
    Log.Error ("Display_Text_Handler failed");
    return "";
  end Display_Text_Handler;


  procedure Select_Target (Item : Gui.Information) is
    Name_Id : constant Name.Id_Access := Convertion (Item);
    use type Name.Id_Access;
  begin
    if Name_Id /= null then
      Set_Target_Name (Name.Image_Of (Name_Id.all));
    end if;
    Signal_Action (Define_Target);
  exception
  when others =>
    Log.Error ("Select_Target failed");
  end Select_Target;


  type Display_Data is record
    Width : Natural;
  end record;

  package Persistent_Display is new Persistent (Display_Data, "Display");

  The_Persistent_Display_Data : Persistent_Display.Data;

  The_Display_Data : Display_Data renames The_Persistent_Display_Data.Storage;

  The_Targets_Column : Gui.Column;


  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Action_Handler      : Action_Handler;
                     The_Termination_Handler : not null access procedure) is

    Windows_Width  : constant Natural := 260;
    Windows_Height : constant Natural := 800;
    Minimum_Width  : constant Natural := 200;
    Separation     : constant Natural := (if Os.Is_Osx then 8 else 6);

    procedure Create_Interface is

      procedure Define_Control_Page is
        Title      : constant String := Lexicon.Image_Of (Lexicon.Target);
        Title_Size : constant Natural := Gui.Text_Size_Of (Title) + Separation;
      begin
        Control_Page := Gui.Add_Page (The_Title  => "Control",
                                      The_Action => Enter_Control_Page'access,
                                      The_Style  => (Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False));

        Goto_Button := Gui.Create (Control_Page, "Goto", Handle_Goto'access);
        Gui.Disable (Goto_Button);
        Parking_Button := Gui.Create (Control_Page, "Unpark", Handle_Parking'access);
        Gui.Disable (Parking_Button);

        Target := Gui.Create (Control_Page, Title, "", The_Title_Size => Title_Size, Is_Modifiable  => False);
        Description := Gui.Create (Control_Page, "", "", Is_Modifiable  => False);

        Display := Gui.Create (Parent_Page           => Control_Page,
                               The_Text_Handler      => Display_Text_Handler'access,
                               The_Click_Routine     => Select_Target'access,
                               The_Click_Kind        => Gui.Single_Click,
                               Use_Proportional_Font => True);
        if Persistent_Display.Storage_Is_Empty then
          The_Display_Data := (Width => Windows_Width - 35);
        end if;
        if The_Display_Data.Width < Minimum_Width then
          The_Display_Data.Width := Minimum_Width;
        end if;
        The_Targets_Column := Gui.Add_Column (The_List_View     => Display,
                                              The_Title         => "",
                                              The_Width         => The_Display_Data.Width,
                                              The_Justification => Gui.Left);
      end Define_Control_Page;


      procedure Define_Display_Page is
        -- largest texts
        Moving_Rate_Text : constant String := "Moving Rate";

        Title_Size : constant Natural := Gui.Text_Size_Of (Moving_Rate_Text) + Separation;
        Text_Size  : constant Natural := Natural'max (Gui.Text_Size_Of ("+360d00'00.0"""),
                                                      Gui.Text_Size_Of ("20h58m58.58s")) + Separation;
      begin
        Display_Page := Gui.Add_Page (The_Title  => "Display",
                                      The_Action => Enter_Display_Page'access,
                                      The_Style  => (Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False));

        Target_Ra := Gui.Create (Display_Page, "Target RA", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Target_Dec := Gui.Create (Display_Page, "Target DEC", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);

        Actual_Ra := Gui.Create (Display_Page, "Actual RA", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Actual_Dec := Gui.Create (Display_Page, "Actual DEC", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);

        Actual_Alt := Gui.Create (Display_Page, "Actual Alt", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
        Actual_Az := Gui.Create (Display_Page, "Actual Az", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);

        Lmst := Gui.Create (Display_Page, "LMST", "",
                            Is_Modifiable  => False,
                            The_Size       => Text_Size,
                            The_Title_Size => Title_Size);

        Local_Time := Gui.Create (Display_Page, "Local Time", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
      end Define_Display_Page;

    begin -- Create_Interface
      Selection_Menu.Create (Lexicon.Image_Of (Lexicon.Selection), Selection_Handler'access);
      Targets.Set (The_Selection => All_Objects);
      Catalog_Menu.Create (Lexicon.Image_Of (Lexicon.Catalog), Catalog_Handler'access);
      Catalog_Handler (Data.Favorites);
      Define_Control_Page;
      Define_Display_Page;
      Gui.Enable_Key_Handler;
      Gui.Show;
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;


    function Title return String is
    begin
      return Application_Name;
    end Title;


    procedure Termination is
    begin
      The_Display_Data.Width := Gui.Width_Of (The_Targets_Column);
      Signal_Action (Close);
      Log.Write ("Teminating");
      The_Termination_Handler.all;
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Action_Routine := The_Action_Handler;
    Gui.Registered.Execute (The_Application_Name    => Title,
                            The_Version             => Version,
                            The_Startup_Routine     => Create_Interface'access,
                            The_Termination_Routine => Termination'access,
                            Initial_Metrics         => (Width  => Windows_Width,
                                                        Height => Windows_Height,
                                                        others => <>));
    Log.Write ("Teminated");
  end Execute;


  procedure Clear_Targets is
  begin
    Gui.Hide (Display);
    Gui.Remove_All_From (Display);
  end Clear_Targets;


  procedure Define (New_Targets : Name.Id_List_Access) is
  begin
    The_Targets := New_Targets;
    Gui.Set_Title (The_Targets_Column, Image_Of (The_Targets.Kind));
    Update_Targets;
    Gui.Show (Display);
  end Define;


  procedure Update_Targets is

    procedure Remove_Target (Index : Natural) is
    begin
      Gui.Remove_From (Display, Index);
    end Remove_Target;

    procedure Insert_Target (Item  : Name.Id_Access;
                             Index : Natural) is
      function Convert is new Ada.Unchecked_Conversion (Name.Id_Access, Gui.Information);
    begin
      Gui.Add_To (Display, Index, Convert(Item));
    end Insert_Target;

  begin -- Update
    Name.Update (The_Targets, Remove_Target'access, Insert_Target'access);
  end Update_Targets;

end User;
