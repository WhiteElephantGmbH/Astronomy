-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Cwe;
with Data;
with Device;
with Gui.Enumeration_Menu_Of;
with Gui.Registered;
with Keys;
with Lexicon;
with Parameter;
with Persistent;
with Remote;
with SkyTracker;
with Strings;
with Targets;
with Traces;
with User.Input;

package body User is

  package Log is new Traces ("User");

  package Fans renames Device.Fans;
  package M3   renames Device.M3;

  Page         : Gui.Page;
  Left_Button  : Gui.Button;
  Right_Button : Gui.Button;
  Progress_Bar : Gui.Progress_Bar;
  Target       : Gui.Plain_Edit_Box;
  Description  : Gui.Plain_Edit_Box;
  Display      : Gui.List_View;

  type Target_Selection is (No_Target, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection := Target_Object;

  The_Status : Telescope.State := Telescope.Unknown;

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
  subtype Selection is Targets.Selection;


  function Image_Of (The_Selection : Selection) return String is

    type Names is array (Selection) of Lexicon.Word;

    Name_Of : constant Names := [All_Objects        => Lexicon.All_Objects,
                                 Solar_System       => Lexicon.Solar_System,
                                 Clusters           => Lexicon.Clusters,
                                 Open_Clusters      => Lexicon.Open_Clusters,
                                 Nebulas            => Lexicon.Nebulas,
                                 Galaxies           => Lexicon.Galaxies,
                                 Stars              => Lexicon.Stars,
                                 Multiple_Stars     => Lexicon.Multiple_Stars,
                                 Near_Earth_Objects => Lexicon.Neos];
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


  function Image_Of (The_Selection : Data.Kind) return String is
    use all type Data.Kind;
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
    when Neo =>
      return "NEO";
    when Ngc =>
      return "NGC";
    when Ocl =>
      return "OCl";
    when Quasars =>
      return Lexicon.Image_Of (Lexicon.Quasars);
    end case;
    return Strings.Legible_Of (The_Selection'img);
  end Image_Of;

  package Catalog_Menu is new Gui.Enumeration_Menu_Of (Data.Kind, Gui.Radio, Image_Of);

  procedure Catalog_Handler (The_Catalog : Data.Kind) is
  begin
    Log.Write ("Catalog: " & The_Catalog'img);
    Name.Define (The_Catalog);
    Signal_Action (Define_Catalog);
  exception
  when others =>
    Log.Error ("Catalog_Handler");
  end Catalog_Handler;


  function Image_Of (The_State : Fans.State) return String is
  begin
    case The_State is
    when Fans.Off =>
      return Lexicon.Image_Of (Lexicon.Off);
    when Fans.On =>
      return Lexicon.Image_Of (Lexicon.On);
    end case;
  end Image_Of;

  Is_From_Set : Boolean := False;

  package Fans_Menu is new Gui.Enumeration_Menu_Of (Fans.State, Gui.Radio, Image_Of);

  procedure Fans_Handler (The_State : Fans.State) is
  begin
    if not Is_From_Set then
      Log.Write ("Fans: " & The_State'img);
      Fans.Turn (To => The_State);
    end if;
  exception
  when others =>
    Log.Error ("Fans_Handler");
  end Fans_Handler;


  function Image_Of (The_Place : M3.Place) return String is
  begin
    case The_Place is
    when M3.Camera =>
      return Lexicon.Image_Of (Lexicon.Camera);
    when M3.Ocular =>
      return Lexicon.Image_Of (Lexicon.Ocular);
    end case;
  end Image_Of;

  package M3_Menu is new Gui.Enumeration_Menu_Of (M3.Place, Gui.Radio, Image_Of);

  procedure M3_Handler (The_Place : M3.Place) is
  begin
    if not Is_From_Set then
      Log.Write ("M3: " & The_Place'img);
      M3.Turn (To => The_Place);
    end if;
  exception
  when others =>
    Log.Error ("M3_Handler");
  end M3_Handler;


  function Image_Of (The_Mode : Cwe.Mode) return String is
  begin
    case The_Mode is
    when Cwe.Off =>
      return Lexicon.Image_Of (Lexicon.Off);
    when Cwe.On =>
      return Lexicon.Image_Of (Lexicon.On);
    end case;
  end Image_Of;

  package Cwe_Menu is new Gui.Enumeration_Menu_Of (Cwe.Mode, Gui.Radio, Image_Of);

  procedure Cwe_Handler (The_Mode : Cwe.Mode) is
  begin
    Log.Write ("CWE: " & The_Mode'img);
    Cwe.Set (To => The_Mode);
  exception
  when others =>
    Log.Error ("Cwe_Handler");
  end Cwe_Handler;


  function Image_Of (The_Command : Remote.Command) return String is
  begin
    case The_Command is
    when Remote.Start_Session =>
      return Lexicon.Image_Of (Lexicon.Start);
    when Remote.Generate_Qr_Code =>
      return Lexicon.Image_Of (Lexicon.Qr_Code);
    when Remote.End_Session =>
      return Lexicon.Image_Of (Lexicon.Finish);
    end case;
  end Image_Of;

  package Demo_21_Menu is new Gui.Enumeration_Menu_Of (Remote.Command, Gui.Plain, Image_Of);

  procedure Demo_21_Handler (The_Command : Remote.Command) is
  begin
    Remote.Execute (The_Command);
  exception
  when others =>
    Log.Error ("Demo_21_Handler");
  end Demo_21_Handler;


  procedure Show_Error (The_Text : String := Error.Message) is
  begin
    Gui.Beep;
    Gui.Message_Box (The_Text);
    Log.Warning ("Show Error: " & The_Text);
  end Show_Error;


  procedure Show (The_Progress : Percent) is
  begin
    Gui.Report_Progress (Progress_Bar, Natural(The_Progress));
  end Show;


  procedure Show (Visible_In : Duration) is
  begin
    Show_Description (Targets.Text_Of (Visible_In));
  end Show;


  Perform_Left_Handler : access procedure := null;

  procedure Perform_Left is
  begin
    Cwe.New_Offset;
    Perform_Left_Handler.all;
  end Perform_Left;


  Perform_Right_Handler : access procedure := null;

  procedure Perform_Right is
  begin
    Perform_Right_Handler.all;
  end Perform_Right;


  procedure Perform_Startup is
  begin
    Signal_Action (Startup);
  end Perform_Startup;


  procedure Perform_Shutdown is
  begin
    Signal_Action (Shutdown);
  end Perform_Shutdown;


  procedure Set_Startup_Text is
  begin
    Gui.Set_Text (Left_Button, "Startup");
  end Set_Startup_Text;


  procedure Set_Goto_Text is
  begin
    Gui.Set_Text (Left_Button, "Goto");
  end Set_Goto_Text;


  procedure Set_Stop_Text is
  begin
    Gui.Set_Text (Right_Button, "Stop");
  end Set_Stop_Text;


  procedure Set_Shutdown_Text is
  begin
    if Telescope.Park_Position_Defined then
      Gui.Set_Text (Right_Button, "Park");
    else
      Gui.Set_Text (Right_Button, "Shutdown");
    end if;
  end Set_Shutdown_Text;


  procedure Disable_Startup_Button is
  begin
    Perform_Left_Handler := null;
    Set_Startup_Text;
    Gui.Disable (Left_Button);
  end Disable_Startup_Button;


  procedure Enable_Startup_Button is
  begin
    Perform_Left_Handler := Perform_Startup'access;
    Set_Startup_Text;
    Gui.Enable (Left_Button);
  end Enable_Startup_Button;


  procedure Disable_Stop_Button is
  begin
    Perform_Right_Handler := null;
    Set_Stop_Text;
    Gui.Disable (Right_Button);
  end Disable_Stop_Button;


  procedure Enable_Stop_Button is
  begin
    Perform_Right_Handler := Perform_Stop'access;
    Set_Stop_Text;
    Gui.Enable (Right_Button);
  end Enable_Stop_Button;


  procedure Disable_Goto_Button is
  begin
    Perform_Left_Handler := null;
    Set_Goto_Text;
    Gui.Disable (Left_Button);
  end Disable_Goto_Button;


  procedure Enable_Goto_Button is
  begin
    Set_Goto_Text;
    if The_Target_Selection = Target_Object then
      Perform_Left_Handler := Perform_Goto'access;
      Gui.Enable (Left_Button);
    else
      Perform_Left_Handler := null;
      Disable_Goto_Button;
    end if;
  end Enable_Goto_Button;


  procedure Enable_Shutdown_Button is
  begin
    Perform_Right_Handler := Perform_Shutdown'access;
    Set_Shutdown_Text;
    Gui.Enable (Right_Button);
  end Enable_Shutdown_Button;


  procedure Disable_Shutdown_Button is
  begin
    Perform_Right_Handler := null;
    Set_Shutdown_Text;
    Gui.Disable (Right_Button);
  end Disable_Shutdown_Button;


  procedure Menu_Disable is
  begin
    Fans_Menu.Disable;
    M3_Menu.Disable;
  end Menu_Disable;


  procedure Menu_Enable is
  begin
    Fans_Menu.Enable;
    M3_Menu.Enable;
  end Menu_Enable;


  procedure Show (Information : Telescope.Data) is
    use type Telescope.State;
  begin
    if (The_Status /= Information.Status) or (Last_Target_Selection /= The_Target_Selection) then
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      case The_Status is
      when Telescope.Unknown | Telescope.Restarting =>
        Disable_Startup_Button;
        Disable_Stop_Button;
        Menu_Disable;
      when Telescope.Disconnected =>
        Enable_Startup_Button;
        Disable_Shutdown_Button;
        Menu_Disable;
      when Telescope.Mount_Error =>
        Disable_Startup_Button;
        Disable_Stop_Button;
        Menu_Disable;
        Enable_Shutdown_Button;
      when Telescope.Connected | Telescope.Enabled =>
        Enable_Startup_Button;
        Enable_Shutdown_Button;
        Menu_Disable;
      when Telescope.Disconnecting | Telescope.Disabling | Telescope.Parking =>
        Disable_Startup_Button;
        Enable_Stop_Button;
        Menu_Disable;
      when Telescope.Connecting | Telescope.Enabling | Telescope.Homing =>
        Disable_Startup_Button;
        Enable_Stop_Button;
        Menu_Disable;
      when Telescope.Stopped =>
        Enable_Goto_Button;
        Enable_Shutdown_Button;
        Menu_Enable;
      when Telescope.Positioned | Telescope.Waiting =>
        Enable_Stop_Button;
        Enable_Goto_Button;
      when Telescope.Positioning | Telescope.Preparing | Telescope.Approaching | Telescope.Is_Tracking =>
        Enable_Goto_Button;
        Enable_Stop_Button;
        Menu_Disable;
      when Telescope.Stopping =>
        Disable_Goto_Button;
        Disable_Stop_Button;
        Menu_Disable;
      end case;
    end if;
    Gui.Set_Status_Line (Information.Status'img);
    Is_From_Set := True;
    Fans_Menu.Set (Information.Fans_State);
    if Information.M3_Position in M3.Place then
      M3_Menu.Set (Information.M3_Position);
    end if;
    Is_From_Set := False;
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


  function Image_Orientation return Telescope.Orientation is
  begin
    return Telescope.Correct;
  end Image_Orientation;


  procedure Perform_Goto is
  begin
    Signal_Action (Go_To);
  end Perform_Goto;


  procedure Perform_Stop is
  begin
    Signal_Action (Stop);
  end Perform_Stop;


  function Target_Name return String is
  begin
    return Gui.Contents_Of (Target);
  end Target_Name;


  procedure Show_Description (Image : String) is
  begin
    Gui.Set_Text (Description, Image);
  end Show_Description;


  The_Targets : Name.Id_List_Access;


  procedure Enter_Handling is
  begin
    Signal_Action (Back);
  end Enter_Handling;


  procedure Put (The_Command : Device.Command) is
  begin
    Input.Put (The_Command, From => Input.Keypad);
  end Put;


  procedure Put_Key (The_Key : Keys.Command) is
    use all type Keys.Command;
    use all type Device.Command;
  begin
    case The_Key is
    when Move_Left =>
      Put (Move_Left);
    when Move_Right=>
      Put (Move_Right);
    when Move_Up=>
      Put (Move_Up);
    when Move_Down=>
      Put (Move_Down);
    when Increase_Time=>
      Put (Increase_Time);
    when Decrease_Time=>
      Put (Decrease_Time);
    when Keys.End_Command =>
      Put (No_Command);
    when Increase_Speed=>
      Put (Increase_Speed);
    when Decrease_Speed=>
      Put (Decrease_Speed);
    when Enter=>
      Put (Enter);
    end case;
  end Put_Key;

  procedure Key_Handler is new Keys.Handler (Put_Key);


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

    Windows_Width  : constant := 310;
    Windows_Height : constant := 800;
    Minimum_Width  : constant := 200;
    Separation     : constant := 10;

    procedure Create_Interface is

      procedure Define_Page is
        Target_Image : constant String  := Lexicon.Image_Of (Lexicon.Target);
        Target_Size  : constant Natural := Gui.Text_Size_Of (Target_Image) + Separation;
      begin
        Page := Gui.Add_Page (The_Title  => Lexicon.Image_Of (Lexicon.Ocular),
                              The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                             Gui.Buttons_Fill_Vertically   => False]);
        Left_Button := Gui.Create (Page, "", Perform_Left'access);
        Gui.Disable (Left_Button);
        Right_Button := Gui.Create (Page, "", Perform_Right'access);
        Gui.Disable (Right_Button);

        Progress_Bar := Gui.Create (Page);
        Gui.Define_Range (Progress_Bar, Natural(Percent'last));

        Target := Gui.Create (Page, Target_Image, "", The_Title_Size => Target_Size, Is_Modifiable  => False);
        Description := Gui.Create (Page, "", "", Is_Modifiable  => False);

        Display := Gui.Create (Parent_Page           => Page,
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
      end Define_Page;

    begin -- Create_Interface
      Selection_Menu.Create (Lexicon.Image_Of (Lexicon.Selection), Selection_Handler'access);
      Targets.Set (The_Selection => All_Objects);
      Catalog_Menu.Create (Lexicon.Image_Of (Lexicon.Catalog), Catalog_Handler'access);
      Catalog_Handler (Data.Favorites);
      Fans_Menu.Create (Lexicon.Image_Of (Lexicon.Fans), Fans_Handler'access);
      M3_Menu.Create (Lexicon.Image_Of (Lexicon.Optic), M3_Handler'access);
      Menu_Disable;
      Cwe_Menu.Create ("CWE", Cwe_Handler'access);
      if Parameter.Remote_Configured then
        Demo_21_Menu.Create ("Demo 21", Demo_21_Handler'access);
      end if;
      Define_Page;
      Catalog_Menu.Enable;
      Selection_Menu.Enable;
      Gui.Install_Key_Handler (Key_Handler'access);
      Gui.Enable_Key_Handler;
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;


    procedure Termination is
    begin
      Input.Close;
      The_Display_Data.Width := Gui.Width_Of (The_Targets_Column);
      Signal_Action (Close);
      Log.Write ("Teminating");
      The_Termination_Handler.all;
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Input.Open;
    Action_Routine := The_Action_Handler;
    Gui.Registered.Execute (The_Application_Name    => SkyTracker.Title,
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
