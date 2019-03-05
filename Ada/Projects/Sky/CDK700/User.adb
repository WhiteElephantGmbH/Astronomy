-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Alignment;
with Application;
with Data;
with Device;
with Earth;
with Gui.Enumeration_Menu_Of;
with Gui.Registered;
with Gui.Key_Codes;
with Lexicon;
with Parameter;
with Persistent;
with Refraction;
with Space;
with Strings;
with Time;
with Traces;
with User.Input;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Version;

  Orientation_Key : constant String := "Orientation";

  Control_Page : Gui.Page;
  Goto_Button  : Gui.Button;
  Stop_Button  : Gui.Button;
  Progress_Bar : Gui.Progress_Bar;
  Target       : Gui.Plain_Edit_Box;
  Description  : Gui.Plain_Edit_Box;
  Display      : Gui.List_View;

  Display_Page      : Gui.Page;
  Target_Ra         : Gui.Plain_Edit_Box;
  Target_Dec        : Gui.Plain_Edit_Box;
  Actual_Ra         : Gui.Plain_Edit_Box;
  Actual_Dec        : Gui.Plain_Edit_Box;
  Actual_Az         : Gui.Plain_Edit_Box;
  Actual_Alt        : Gui.Plain_Edit_Box;
  Lmst              : Gui.Plain_Edit_Box;
  Local_Time        : Gui.Plain_Edit_Box;

  Setup_Page      : Gui.Page;
  Orientation_Box : Gui.Plain_Combo_Box;
  Air_Pressure    : Gui.Plain_Edit_Box;
  Temperature     : Gui.Plain_Edit_Box;

  type Setup_Data_Storage is record
    Image_Orientation : Telescope.Orientation;
    Air_Pressure      : Refraction.Hectopascal;
    Temperature       : Refraction.Celsius;
  end record;

  package Persistent_Setup is new Persistent (Setup_Data_Storage, "Setup");

  The_Setup_Data : Persistent_Setup.Data;

  The_Image_Orientation : Telescope.Orientation   renames The_Setup_Data.Storage.Image_Orientation;
  The_Air_Pressure      : Refraction.Hectopascal  renames The_Setup_Data.Storage.Air_Pressure;
  The_Temperature       : Refraction.Celsius      renames The_Setup_Data.Storage.Temperature;

  type Page is (Is_Control, Is_Display, Is_Setup);

  Is_Setup_Mode : Boolean := False;
  The_Page      : Page := Is_Control;

  type Target_Selection is (No_Target, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection;

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
    if Item /= "" then
      Log.Write ("Target: " & Item);
    end if;
    The_Target_Selection := Target_Object;
    Gui.Set_Text (Target, Item);
    Gui.Set_Text (Description, "");
  end Set_Target_Name;


  function Image_Of (The_Selection : Selection) return String is

    type Names is array (Selection) of Lexicon.Word;

    Name_Of : constant Names := (All_Objects        => Lexicon.All_Objects,
                                 Solar_System       => Lexicon.Solar_System,
                                 Clusters           => Lexicon.Clusters,
                                 Open_Clusters      => Lexicon.Open_Clusters,
                                 Nebulas            => Lexicon.Nebulas,
                                 Galaxies           => Lexicon.Galaxies,
                                 Stars              => Lexicon.Stars,
                                 Multiple_Stars     => Lexicon.Multiple_Stars,
                                 Near_Earth_Objects => Lexicon.Neos);
  begin
    return Lexicon.Image_Of (Name_Of(The_Selection));
  end Image_Of;

  package Selection_Menu is new Gui.Enumeration_Menu_Of (Selection, Gui.Radio, Image_Of);

  Actual_Selection : Selection;

  procedure Selection_Handler (The_Filter : Selection) is
  begin
    Log.Write ("Filter: " & The_Filter'img);
    Actual_Selection := The_Filter;
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


  procedure Show_Error (The_Text : String := Error.Message) is
  begin
    Gui.Beep;
    Gui.Message_Box (The_Text);
  end Show_Error;


  procedure Show (The_Progress : Percent) is
  begin
    Gui.Report_Progress (Progress_Bar, Natural(The_Progress));
  end Show;


  procedure Show (Visible_In : Duration) is

    function Image_Of (Value : Natural;
                       Unit  : String) return String is
      Image : constant String := Value'img & " " & Unit;
    begin
      if Value = 0 then
        return "";
      elsif Value = 1 then
        return Image;
      else
        return Image & "s";
      end if;
    end Image_Of;

    Header : constant String := "Visible in";
    Second : constant String := "second";
    Minute : constant String := "minute";
    Hour   : constant String := "hour";

    procedure Show_Duration (Value      : Natural;
                             Upper_Unit : String;
                             Lower_Unit : String) is
    begin
      Show_Description (Header & Image_Of (Value / 60, Upper_Unit) & Image_Of (Value mod 60, Lower_Unit));
    end Show_Duration;

    Delta_Time : constant Natural := Natural(Visible_In);

  begin -- Show
    if Delta_Time = 0 then
      Show_Description ("");
    elsif Delta_Time < 3600 then
      Show_Duration (Delta_Time, Minute, Second);
    else
      Show_Duration ((Delta_Time + 59) / 60, Hour, Minute);
    end if;
  end Show;


  procedure Show (Information : Telescope.Data) is
    use type Telescope.State;
    use type Device.Time_Synch_State;
  begin
    if The_Target_Selection = No_Target then
      return;
    end if;
    if (The_Status /= Information.Status) or (Last_Target_Selection /= The_Target_Selection) then
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      case The_Status is
      when Telescope.Startup | Telescope.Disconnected | Telescope.Directing | Telescope.Stopping =>
        Gui.Disable (Stop_Button);
        Gui.Disable (Goto_Button);
      when Telescope.Ready | Telescope.Stopped =>
        Gui.Enable (Goto_Button);
      when Telescope.Adjusting =>
        Gui.Disable (Goto_Button);
      when Telescope.Positioning | Telescope.Approaching =>
        Gui.Enable (Stop_Button);
        Gui.Enable (Goto_Button);
      when Telescope.Tracking =>
        Gui.Enable (Stop_Button);
        Gui.Enable (Goto_Button);
      end case;
    end if;
    Gui.Set_Status_Line (Information.Status'img);
    if Earth.Direction_Is_Known (Information.Adjustment) then
      Alignment.Set (Direction => Information.Local_Direction,
                     Offset    => Information.Adjustment);
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
      else
        Gui.Set_Text (Actual_Dec, "");
        Gui.Set_Text (Actual_Ra, "");
      end if;
      if Earth.Direction_Is_Known (Information.Local_Direction) then
        Gui.Set_Text (Actual_Alt, Earth.Alt_Image_Of (Information.Local_Direction));
        Gui.Set_Text (Actual_Az, Earth.Az_Image_Of (Information.Local_Direction));
      else
        Gui.Set_Text (Actual_Alt, "");
        Gui.Set_Text (Actual_Az, "");
      end if;
      if Information.Universal_Time = Time.In_The_Past then
        Gui.Set_Text (Lmst, "");
        Gui.Set_Text (Local_Time, "");
      else
        Gui.Set_Text (Lmst, Time.Image_Of (Time.Lmst_Of (Information.Universal_Time)));
        Gui.Set_Text (Local_Time, Time.Image_Of (Information.Universal_Time, Time_Only => True));
      end if;
    when Is_Setup =>
      null;
    end case;
  end Show;


  procedure Clear_Target is
  begin
    Set_Target_Name ("");
  end Clear_Target;


  function Is_Selected (The_Object : Object) return Boolean is
  begin
    case Actual_Selection is
    when All_Objects =>
      return True;
    when Stars =>
      case The_Object is
      when Stars | Multiple_Stars  =>
        return True;
      when others =>
        return False;
      end case;
    when others =>
      return The_Object = Actual_Selection;
    end case;
  end Is_Selected;


  function Identifier_Of (Item : String) return String is
    The_Image : String := Strings.Trimmed (Item);
  begin
    for Index in The_Image'range loop
      if The_Image(Index) = ' ' then
        The_Image(Index) := '_';
      end if;
    end loop;
    return The_Image;
  end Identifier_Of;


  function Image_Orientation return Telescope.Orientation is
  begin
    return The_Image_Orientation;
  end Image_Orientation;


  procedure Set_Orientation is
  begin
    begin
      The_Image_Orientation := Telescope.Orientation'value(Identifier_Of (Gui.Contents_Of (Orientation_Box)));
    exception
    when others =>
      The_Image_Orientation := Telescope.Correct;
    end;
    Signal_Action (Set_Orientation);
  exception
  when others =>
    Log.Error ("Set_Orientation");
  end Set_Orientation;


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


  Is_Entering_Number : Boolean := False;
  The_Number         : Natural;
  The_Number_Id      : Name.Selector;
  The_Targets        : Name.Id_List_Access;


  function Image_Of (The_Value : Refraction.Hectopascal) return String is
  begin
    return Strings.Trimmed (The_Value'img) & "hPa";
  end Image_Of;


  procedure Define_Air_Pressure is
  begin
    declare
      Value : constant String := Strings.Trimmed (Gui.Contents_Of (Air_Pressure));
      Last  : Natural := Value'last;
    begin
      loop
        exit when Value(Last) in '0'..'9';
        Last := Last - 1;
      end loop;
      The_Air_Pressure := Refraction.Hectopascal'value(Value(Value'first .. Last));
      Refraction.Set (The_Air_Pressure);
    exception
    when others =>
      Show_Error ("Incorrect Air Pressure: " & Value);
    end;
    Gui.Set_Text (Air_Pressure, Image_Of (The_Air_Pressure));
  exception
  when others =>
    Log.Error ("Define_Air_Pressure");
  end Define_Air_Pressure;


  function Image_Of (The_Value : Refraction.Celsius) return String is
  begin
    return Strings.Trimmed (The_Value'img) & "°C";
  end Image_Of;


  procedure Define_Temperature is
  begin
    declare
      Value : constant String := Strings.Trimmed (Gui.Contents_Of (Temperature));
      Last  : Natural := Value'last;
    begin
      loop
        exit when Value(Last) in '0'..'9';
        Last := Last - 1;
      end loop;
      The_Temperature := Refraction.Celsius'value(Value(Value'first .. Last));
      Refraction.Set (The_Temperature);
    exception
    when others =>
      Show_Error ("Incorrect Temperature: " & Value);
    end;
    Gui.Set_Text (Temperature, Image_Of (The_Temperature));
  end Define_Temperature;


  procedure Put (The_Command : Device.Command) is
  begin
    if not Is_Entering_Number then
      Input.Put (The_Command, From => Input.Keypad);
    end if;
  end Put;


  procedure Start_New_Number (Number_Id : Name.Selector) is
  begin
    if not Input.Is_Active then
      The_Number_Id := Number_Id;
      Is_Entering_Number := True;
      The_Number := 0;
    end if;
  end Start_New_Number;


  procedure Not_A_Number is
  begin
    Is_Entering_Number := False;
  end Not_A_Number;


  procedure Handle_Number (Value : Natural) is
  begin
    if Is_Entering_Number then
      The_Number := The_Number * 10 + Value;
    end if;
  exception
  when others =>
    Not_A_Number;
  end Handle_Number;


  procedure Enter_Number is
  begin
    if Is_Entering_Number then
      if The_Number = 0 then
        null; -- no park position
      else
        for Index in The_Targets.Ids'first .. The_Targets.Last loop
          declare
            Item : constant Name.Id := The_Targets.Ids(Index);
          begin
            if Name.Matches (Item, The_Number_Id, The_Number) then
              Set_Target_Name (Name.Image_Of (Item));
              Signal_Action (Define_Target);
              Signal_Action (Go_To);
              exit;
            end if;
          end;
        end loop;
      end if;
      Is_Entering_Number := False;
    elsif The_Target_Selection = Target_Object and then Gui.Is_Enabled (Goto_Button) then
      Signal_Action (Go_To);
    end if;
  end Enter_Number;


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


  procedure Enter_Setup_Page is
  begin
    The_Page := Is_Setup;
    Catalog_Menu.Disable;
    Selection_Menu.Disable;
    Gui.Disable_Key_Handler;
  exception
  when others =>
    Log.Error ("Enter_Setup_Page");
  end Enter_Setup_Page;


  procedure Enter_Handling is
  begin
    Enter_Number;
  end Enter_Handling;


  Ignore_Next : Boolean := False;

  procedure Key_Handler (The_Event    : Gui.Key_Event;
                         The_Key_Code : Gui.Key_Code) is
  begin
    case The_Page is
    when Is_Setup =>
      return;
    when Is_Control | Is_Display =>
      null;
    end case;
    case The_Event is
    when Gui.Key_Pressed =>
      if Ignore_Next then
        return;
      end if;
      Log.Write ("Key pressed: " & The_Key_Code'img);
      case The_Key_Code is
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up | Gui.Key_Codes.K_Up =>
        Put (Device.Move_Up);
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down | Gui.Key_Codes.K_Down =>
        Put (Device.Move_Down);
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left | Gui.Key_Codes.K_Left =>
        Put (Device.Move_Left);
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right | Gui.Key_Codes.K_Right =>
        Put (Device.Move_Right);
      when Gui.Key_Codes.KP_Add | Gui.Key_Codes.K_Page_Up =>
        Put (Device.Increase);
      when Gui.Key_Codes.KP_Subtract | Gui.Key_Codes.K_Page_Down =>
        Put (Device.Decrease);
      when Gui.Key_Codes.K_Back =>
        Put (Device.Stop);
      when Gui.Key_Codes.KP_Enter | Gui.Key_Codes.K_Return =>
        Input.Put (Device.Enter, From => Input.Keypad);
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := True;
      when others =>
        null;
      end case;
    when  Gui.Key_Released =>
      Log.Write ("Key released: " & The_Key_Code'img);
      case The_Key_Code is
      when Gui.Key_Codes.KP_Divide =>
        Start_New_Number (Name.Messier);
      when Gui.Key_Codes.KP_Multiply =>
        Start_New_Number (Name.Enumerated);
      when Gui.Key_Codes.K_Tab | Gui.Key_Codes.K_Delete =>
        Start_New_Number (Name.Caldwell);
      when Gui.Key_Codes.KP_0 =>
        Handle_Number (0);
      when Gui.Key_Codes.KP_1 =>
        Handle_Number (1);
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down | Gui.Key_Codes.K_Down =>
        Put (Device.No_Command);
        Handle_Number (2);
      when Gui.Key_Codes.KP_3 =>
        Handle_Number (3);
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left | Gui.Key_Codes.K_Left =>
        Put (Device.No_Command);
        Handle_Number (4);
      when Gui.Key_Codes.KP_5 =>
        Handle_Number (5);
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right | Gui.Key_Codes.K_Right =>
        Put (Device.No_Command);
        Handle_Number (6);
      when Gui.Key_Codes.KP_7 =>
        Handle_Number (7);
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up | Gui.Key_Codes.K_Up =>
        Put (Device.No_Command);
        Handle_Number (8);
      when Gui.Key_Codes.KP_9 =>
        Handle_Number (9);
      when Gui.Key_Codes.KP_Add | Gui.Key_Codes.K_Page_Up =>
        Put (Device.No_Command);
      when Gui.Key_Codes.KP_Subtract | Gui.Key_Codes.K_Page_Down =>
        Put (Device.No_Command);
      when Gui.Key_Codes.KP_Enter | Gui.Key_Codes.K_Return =>
        Put (Device.No_Command);
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := False;
      when others =>
        Not_A_Number;
      end case;
    end case;
  exception
  when others =>
    Log.Error ("Key_Handler failed");
  end Key_Handler;


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
    if Name_Id /= null then -- not park position
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
    Separation     : constant := 5;

    procedure Create_Interface is

      procedure Define_Control_Page is
        Title      : constant String := Lexicon.Image_Of (Lexicon.Target);
        Title_Size : constant Natural := Gui.Text_Size_Of (Title) + Separation;
      begin
        Control_Page := Gui.Add_Page (The_Title  => "Control",
                                      The_Action => Enter_Control_Page'access,
                                      The_Style  => (Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False));

        Goto_Button := Gui.Create (Control_Page, "Goto", Perform_Goto'access);
        Gui.Disable (Goto_Button);
        Stop_Button := Gui.Create (Control_Page, "Stop", Perform_Stop'access);
        Gui.Disable (Stop_Button);
        Progress_Bar := Gui.Create (Control_Page);
        Gui.Define_Range (Progress_Bar, Natural(Percent'last));

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
        Gui.Install_Key_Handler (Key_Handler'access);
      end Define_Control_Page;


      procedure Define_Display_Page is
        -- largest texts
        Alt_Adjustment_Text : constant String := "Alt Adjustment";

        Title_Size : constant Natural := Gui.Text_Size_Of (Alt_Adjustment_Text) + Separation;
        Text_Size  : constant Natural := Natural'max (Gui.Text_Size_Of ("+360d00'00.000"""),
                                                      Gui.Text_Size_Of ("99 Revolutions")) + Separation;
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

        Actual_Az := Gui.Create (Display_Page, "Azimuth", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Actual_Alt := Gui.Create (Display_Page, "Altitude", "",
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


      procedure Define_Setup_Page is

        -- largest texts
        Title_Size : constant Natural := Gui.Text_Size_Of (Orientation_Key) + Separation;
        Text_Size  : constant Natural := Gui.Text_Size_Of ("Upside Down sb") + Separation;
      begin
        Setup_Page := Gui.Add_Page (The_Title  => "Setup",
                                    The_Action => Enter_Setup_Page'access,
                                    The_Style  => (Gui.Buttons_Fill_Horizontally => True,
                                                   Gui.Buttons_Fill_Vertically   => False));

        Orientation_Box := Gui.Create (Setup_Page, Orientation_Key,
                                       The_Action_Routine => Set_Orientation'access,
                                       The_Size           => Text_Size,
                                       The_Title_Size     => Title_Size);
        for Value in Telescope.Orientation'range loop
          Gui.Add_Text (Orientation_Box, Strings.Legible_Of (Value'img));
        end loop;
        Gui.Select_Text (Orientation_Box, Strings.Legible_Of (The_Image_Orientation'img));
        Air_Pressure := Gui.Create (Setup_Page, "Air Pressure", Image_Of (The_Air_Pressure),
                                    The_Action_Routine => Define_Air_Pressure'access,
                                    The_Size           => Text_Size,
                                    The_Title_Size     => Title_Size);
        Temperature := Gui.Create (Setup_Page, "Temperature", Image_Of (The_Temperature),
                                   The_Action_Routine => Define_Temperature'access,
                                   The_Size           => Text_Size,
                                   The_Title_Size     => Title_Size);

      end Define_Setup_Page;

    begin -- Create_Interface
      Selection_Menu.Create (Lexicon.Image_Of (Lexicon.Selection), Selection_Handler'access);
      Actual_Selection := All_Objects;
      Catalog_Menu.Create (Lexicon.Image_Of (Lexicon.Catalog), Catalog_Handler'access);
      Catalog_Handler (Data.Favorites);
      Define_Control_Page;
      if Persistent_Setup.Storage_Is_Empty then
        The_Image_Orientation := Telescope.Correct;
        The_Air_Pressure := 0;
        The_Temperature := 10;
      end if;
      Signal_Action (Set_Orientation);
      Refraction.Set (The_Air_Pressure);
      Refraction.Set (The_Temperature);
      Is_Setup_Mode := Parameter.Is_Setup_Mode;
      if Is_Setup_Mode then
        Define_Display_Page;
        Define_Setup_Page;
      end if;
      Gui.Enable_Key_Handler;
      Gui.Show;
      The_Startup_Handler.all;
    exception
    when others =>
      Log.Error ("Create_Interface failed");
    end Create_Interface;


    function Title return String is
      Titel_Name : constant String := Parameter.Telescope_Name;
    begin
      if Titel_Name = "" then
        return Application_Name;
      else
        return Titel_Name;
      end if;
    end Title;


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


  procedure Define (Targets : Name.Id_List_Access) is
  begin
    The_Targets := Targets;
    Gui.Set_Title (The_Targets_Column, Image_Of (The_Targets.Kind));
    Gui.Add_To (Display, 1, Gui.No_Information);
    Update_Targets;
    Gui.Show (Display);
  end Define;


  procedure Update_Targets is

    procedure Remove_Target (Index : Natural) is
    begin
      Gui.Remove_From (Display, Index + 1);
    end Remove_Target;

    procedure Insert_Target (Item  : Name.Id_Access;
                             Index : Natural) is
      function Convert is new Ada.Unchecked_Conversion (Name.Id_Access, Gui.Information);
    begin
      Gui.Add_To (Display, Index + 1, Convert(Item));
    end Insert_Target;

  begin -- Update
    Name.Update (The_Targets, Remove_Target'access, Insert_Target'access);
  end Update_Targets;

end User;
