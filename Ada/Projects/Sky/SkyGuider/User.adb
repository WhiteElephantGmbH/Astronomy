-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Angle;
with Application;
with Data;
with Earth;
with Gui.Enumeration_Menu_Of;
with Gui.Key_Codes;
with Gui.Registered;
with Lexicon;
with Objects;
with Os;
with Persistent;
with Picture;
with Pole_Axis;
with Refraction;
with Sky_Line;
with Site;
with Space;
with Strings;
with Targets;
with Text;
with Time;
with Traces;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Main_Version;

  Setup_Object_Key : constant String := "Setup Object";

  Control_Page         : Gui.Page;
  First_Control_Button : Gui.Button;
  Stop_Or_Synch_Button : Gui.Button;
  Target               : Gui.Plain_Edit_Box;
  Description          : Gui.Plain_Edit_Box;
  Display              : Gui.List_View;

  Display_Page : Gui.Page;
  Target_Ra    : Gui.Plain_Edit_Box;
  Target_Dec   : Gui.Plain_Edit_Box;
  Actual_Ra    : Gui.Plain_Edit_Box;
  Actual_Dec   : Gui.Plain_Edit_Box;
  Ra_Offset    : Gui.Plain_Edit_Box;
  Dec_Offset   : Gui.Plain_Edit_Box;
  Picture_Ra   : Gui.Plain_Edit_Box;
  Picture_Dec  : Gui.Plain_Edit_Box;
  Actual_Alt   : Gui.Plain_Edit_Box;
  Actual_Az    : Gui.Plain_Edit_Box;
  Moving_Rate  : Gui.Plain_Edit_Box;
  Lmst         : Gui.Plain_Edit_Box;
  Local_Time   : Gui.Plain_Edit_Box;

  Setup_Page         : Gui.Page;
  First_Setup_Button : Gui.Button;
  Clear_Button       : Gui.Button;
  Longitude          : Gui.Plain_Edit_Box;
  Latitude           : Gui.Plain_Edit_Box;
  Elevation          : Gui.Plain_Edit_Box;
  Air_Pressure       : Gui.Plain_Edit_Box;
  Temperature        : Gui.Plain_Edit_Box;
  Setup_Control      : Gui.Plain_Combo_Box;
  Cone_Error         : Gui.Plain_Edit_Box;
  Az_Offset          : Gui.Plain_Edit_Box;
  Alt_Offset         : Gui.Plain_Edit_Box;

  type Setup_Data_Storage is record
    Air_Pressure : Refraction.Hectopascal;
    Temperature  : Refraction.Celsius;
  end record;

  The_Error_Text : Text.String;

  package Persistent_Setup is new Persistent (Setup_Data_Storage, "Setup");

  The_Setup_Data : Persistent_Setup.Data;

  The_Air_Pressure : Refraction.Hectopascal renames The_Setup_Data.Storage.Air_Pressure;
  The_Temperature  : Refraction.Celsius     renames The_Setup_Data.Storage.Temperature;

  type Page is (Is_Control, Is_Display, Is_Setup);

  The_Page : Page := Is_Control;

  type Target_Selection is (No_Target, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection := Target_Object;

  Align_On_Picture_Change     : Boolean := False;
  Align_On_Picture_Is_Enabled : Boolean := False;

  The_Status : Telescope.State := Telescope.Disconnected;

  subtype Get_Pole is Setup_Object range Pole_Top .. Pole_Right;

  The_Setup_Object : Setup_Object := Skyline;

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

    Name_Of : constant Names := [All_Objects    => Lexicon.All_Objects,
                                 Solar_System   => Lexicon.Solar_System,
                                 Clusters       => Lexicon.Clusters,
                                 Open_Clusters  => Lexicon.Open_Clusters,
                                 Nebulas        => Lexicon.Nebulas,
                                 Galaxies       => Lexicon.Galaxies,
                                 Stars          => Lexicon.Stars,
                                 Multiple_Stars => Lexicon.Multiple_Stars];
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


  procedure Show_Error (The_Text : String := Error.Message) is
  begin
    The_Error_Text := Text.String_Of (The_Text);
    Gui.Beep;
  end Show_Error;


  procedure Clear_Error is
  begin
    Text.Clear (The_Error_Text);
  end Clear_Error;


  The_Actual_Direction : Earth.Direction; -- used for Skyline

  procedure Define_Setup_Buttons is
  begin
    case The_Status is
    when Telescope.Disconnected | Telescope.Connected | Telescope.Initialized =>
      Gui.Disable (Setup_Control);
      Gui.Set_Text (Clear_Button, "Clear");
      if Site.Is_Defined then
        Gui.Enable (Clear_Button);
      else
        Gui.Disable (Clear_Button);
      end if;
    when Telescope.Stopped | Telescope.Tracking =>
      Gui.Enable (Setup_Control);
      Gui.Set_Text (Clear_Button, "Delete");
      case The_Setup_Object is
      when Skyline =>
        Gui.Set_Text (First_Setup_Button, "Add");
        if Earth.Direction_Is_Known (The_Actual_Direction) then
          Gui.Enable (First_Setup_Button);
        else
          Gui.Disable (First_Setup_Button);
        end if;
        if Sky_Line.Is_Defined then
          Gui.Enable (Clear_Button);
        else
          Gui.Disable (Clear_Button);
        end if;
      when Get_Pole =>
        Gui.Set_Text (First_Setup_Button, "Get");
        if Pole_Axis.Has_Values then
          Gui.Enable (Clear_Button);
        else
          Gui.Disable (Clear_Button);
        end if;
      end case;
      Gui.Enable (First_Setup_Button);
    when Telescope.Approaching =>
      Gui.Disable (Setup_Control);
      Gui.Set_Text (First_Setup_Button, "Stop");
      Gui.Enable (First_Setup_Button);
      Gui.Disable (Clear_Button);
    when Telescope.Error =>
      Gui.Disable (Setup_Control);
      Gui.Set_Text (Clear_Button, "Reset");
      Gui.Enable (Clear_Button);
      Gui.Disable (First_Setup_Button);
    when Telescope.Solving | Telescope.Moving =>
      Gui.Disable (Setup_Control);
      Gui.Set_Text (First_Setup_Button, "");
      Gui.Disable (First_Setup_Button);
      Gui.Disable (Clear_Button);
    end case;
  end Define_Setup_Buttons;


  procedure Show (Information : Telescope.Data) is

    procedure Set_Tracking_Buttons is
    begin
      Gui.Set_Text (First_Control_Button, "Goto");
      Gui.Set_Text (Stop_Or_Synch_Button, "Synch");
      if The_Target_Selection = No_Target then
        Gui.Disable (First_Control_Button);
        Gui.Disable (Stop_Or_Synch_Button);
      else
        Gui.Enable (First_Control_Button);
        Gui.Enable (Stop_Or_Synch_Button);
      end if;
    end Set_Tracking_Buttons;

    procedure Clear_Actual_Values is
    begin
      Gui.Set_Text (Actual_Dec, "");
      Gui.Set_Text (Actual_Ra, "");
      Gui.Set_Text (Dec_Offset, "");
      Gui.Set_Text (Ra_Offset, "");
      Gui.Set_Text (Actual_Alt, "");
      Gui.Set_Text (Actual_Az, "");
      The_Actual_Direction := Earth.Unknown_Direction;
    end Clear_Actual_Values;

    The_Offset : Space.Direction;

    use type Angle.Value;
    use type Space.Direction;
    use type Telescope.State;

  begin -- Show
    if not Text.Is_Null (The_Error_Text) then
      Gui.Set_Text (First_Control_Button, "Reset");
      Gui.Set_Text (Clear_Button, "Reset");
      Gui.Set_Status_Line ("ERROR - " & Text.String_Of (The_Error_Text));
      Gui.Enable (First_Control_Button);
      Gui.Enable (Clear_Button);
      return;
    end if;
    if (The_Status /= Information.Status)
      or (Last_Target_Selection /= The_Target_Selection)
      or Align_On_Picture_Change
    then
      Align_On_Picture_Change := False;
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      case The_Status is
      when Telescope.Error =>
        Gui.Set_Text (First_Control_Button, "Reset");
        Gui.Enable (First_Control_Button);
        Show_Error (Telescope.Error_Message);
      when Telescope.Disconnected =>
        Gui.Set_Text (First_Control_Button, "Start");
        Gui.Enable (First_Control_Button);
        Gui.Disable (Stop_Or_Synch_Button);
        Align_On_Picture_Is_Enabled := False;
      when Telescope.Connected =>
        Gui.Set_Text (First_Control_Button, "Initialize");
        Gui.Enable (First_Control_Button);
        Gui.Disable (Stop_Or_Synch_Button);
        Align_On_Picture_Is_Enabled := False;
      when Telescope.Initialized =>
        Set_Tracking_Buttons;
      when Telescope.Solving =>
        Gui.Disable (First_Control_Button);
        Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
      when Telescope.Approaching =>
        Gui.Enable (First_Control_Button);
        Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
        Gui.Enable (Stop_Or_Synch_Button);
      when Telescope.Stopped =>
        Set_Tracking_Buttons;
      when Telescope.Tracking =>
        Set_Tracking_Buttons;
        if Align_On_Picture_Is_Enabled then
          Gui.Set_Text (First_Control_Button, "Align");
        end if;
      when Telescope.Moving =>
        Gui.Disable (First_Control_Button);
        Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
        Gui.Enable (Stop_Or_Synch_Button);
        Align_On_Picture_Is_Enabled := False;
      end case;
      Define_Setup_Buttons;
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
        if Space.Direction_Is_Known (Information.Actual_Direction) then
          Gui.Set_Text (Actual_Dec, Space.Dec_Image_Of (Information.Actual_Direction));
          Gui.Set_Text (Actual_Ra, Space.Ra_Image_Of (Information.Actual_Direction));
          The_Offset := Information.Actual_Direction - Information.Target_Direction;
          Gui.Set_Text (Dec_Offset, Space.Dec_Offset_Image_Of (The_Offset));
          Gui.Set_Text (Ra_Offset, Space.Ra_Offset_Image_Of (The_Offset));
          Gui.Set_Text (Actual_Alt, Earth.Alt_Image_Of (The_Actual_Direction));
          Gui.Set_Text (Actual_Az, Earth.Az_Image_Of (The_Actual_Direction));
        else
          Clear_Actual_Values;
        end if;
      else
        Gui.Set_Text (Target_Dec, "");
        Gui.Set_Text (Target_Ra, "");
        Clear_Actual_Values;
      end if;
      if Space.Direction_Is_Known (Information.Picture_Direction) then
        Gui.Set_Text (Picture_Ra, Space.Ra_Image_Of (Information.Picture_Direction));
        Gui.Set_Text (Picture_Dec, Space.Dec_Image_Of (Information.Picture_Direction));
      else
        Gui.Set_Text (Picture_Ra, "");
        Gui.Set_Text (Picture_Dec, "");
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
      Gui.Set_Text (Moving_Rate, Strings.Legible_Of (Information.Actual_Moving_Rate'image));
    when Is_Setup =>
      if Site.Is_Defined then
        Gui.Set_Text (Longitude, Angle.Image_Of (Site.Longitude));
        Gui.Set_Text (Latitude, Angle.Image_Of (Site.Latitude, Show_Signed => True));
        Gui.Set_Text (Elevation, Strings.Trimmed (Site.Elevation'img) & 'm');
      else
        Gui.Set_Text (Longitude, "");
        Gui.Set_Text (Latitude, "");
        Gui.Set_Text (Elevation, "");
      end if;
      if Information.Cone_Error = Angle.Zero then
        Gui.Set_Text (Cone_Error, "");
      else
        Gui.Set_Text (Cone_Error, Angle.Image_Of (Information.Cone_Error, Show_Signed => True));
      end if;
      if Earth.Direction_Is_Known (Information.Pole_Offsets) then
        if Earth.Az_Of (Information.Pole_Offsets) = Angle.Zero then
          Gui.Set_Text (Az_Offset, "");
        else
          Gui.Set_Text (Az_Offset, Earth.Az_Offset_Image_Of (Information.Pole_Offsets));
        end if;
        if Earth.Alt_Of (Information.Pole_Offsets) = Angle.Zero then
          Gui.Set_Text (Alt_Offset, "");
        else
          Gui.Set_Text (Alt_Offset, Earth.Alt_Offset_Image_Of (Information.Pole_Offsets));
        end if;
      else
        Gui.Set_Text (Az_Offset, "");
        Gui.Set_Text (Alt_Offset, "");
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


  procedure Perform_Goto is
  begin
    Signal_Action (Go_To);
  end Perform_Goto;


  procedure Perform_Stop is
  begin
    Signal_Action (Stop);
  end Perform_Stop;


  procedure Perform_Synch is
  begin
    Signal_Action (Synch);
  end Perform_Synch;


  procedure Perform_Stop_Or_Synch is
  begin
    case The_Status is
    when Telescope.Initialized | Telescope.Stopped | Telescope.Tracking =>
      case The_Target_Selection is
      when Target_Object =>
        Perform_Synch;
        Align_On_Picture_Is_Enabled := False;
        Align_On_Picture_Change := True;
      when No_Target =>
        raise Program_Error;
      end case;
    when Telescope.Moving | Telescope.Approaching | Telescope.Solving =>
      Perform_Stop;
    when others =>
      null;
    end case;
  exception
  when others =>
    Log.Error ("Perform_Stop_Or_Synch");
  end Perform_Stop_Or_Synch;


  procedure Perform_First_Control is
  begin
    case The_Status is
    when Telescope.Error =>
      Clear_Error;
    when Telescope.Disconnected =>
      Signal_Action (Start);
    when Telescope.Connected =>
      Signal_Action (Initialize);
    when Telescope.Initialized | Telescope.Stopped | Telescope.Tracking =>
      case The_Target_Selection is
      when Target_Object =>
        if Align_On_Picture_Is_Enabled then
          Align_On_Picture_Is_Enabled := False;
          Align_On_Picture_Change := True;
          Signal_Action (Align);
        else
          Perform_Goto;
        end if;
      when No_Target =>
        raise Program_Error;
      end case;
    when Telescope.Approaching =>
      Perform_Goto;
    when Telescope.Solving | Telescope.Moving =>
      null;
    end case;
  exception
  when others =>
    Log.Error ("Perform_First_Control");
  end Perform_First_Control;


  function Target_Name return String is
  begin
    return Gui.Contents_Of (Target);
  end Target_Name;


  procedure Show_Description (Image : String) is
  begin
    Gui.Set_Text (Description, Image);
  end Show_Description;


  The_Targets : Name.Id_List_Access;


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
      Telescope.Set_Error ("Incorrect Air Pressure: " & Value);
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


  procedure Goto_Pole (Item : Get_Pole) is
  begin
    The_Target_Selection := No_Target;
    Gui.Set_Text (Target, Strings.Legible_Of (Item'image));
    Signal_Action (Define_Target);
    Perform_Goto;
  end Goto_Pole;


  procedure Define_Setup_Object is

    Setup_Object_Image : constant String := Identifier_Of (Gui.Contents_Of (Setup_Control));

  begin -- Define_Operation
    The_Setup_Object := Setup_Object'value(Setup_Object_Image);
    Log.Write ("Setup Object - " & Setup_Object_Image);
    Define_Setup_Buttons;
    case The_Setup_Object is
    when Skyline =>
      Gui.Disable (First_Setup_Button);
      Set_Target_Name ("");
    when Get_Pole =>
      Goto_Pole (The_Setup_Object);
    end case;
  exception
  when others =>
    Log.Error ("Define_Setup_Object");
  end Define_Setup_Object;


  procedure Handle_Clear_Button is
    use type Telescope.State;
  begin
    case The_Status is
    when Telescope.Error =>
      Clear_Error;
    when Telescope.Disconnected | Telescope.Connected | Telescope.Initialized =>
      Site.Clear;
      Targets.Update_List;
      Gui.Disable (Clear_Button);
    when others =>
      case The_Setup_Object is
      when Skyline =>
        Sky_Line.Clear;
        Gui.Disable (Clear_Button);
      when Get_Pole =>
        Pole_Axis.Clear;
        Gui.Disable (Clear_Button);
      end case;
    end case;
  exception
  when others =>
    Log.Error ("Handle_Clear_Button");
  end Handle_Clear_Button;


  procedure Handle_First_Setup_Button is
  begin
    case The_Status is
    when Telescope.Stopped | Telescope.Tracking =>
      case The_Setup_Object is
      when Pole_Top =>
        Pole_Axis.Evaluate_Pole_Top;
      when Pole_Left =>
        Pole_Axis.Evaluate_Pole_Left;
      when Pole_Right =>
        Pole_Axis.Evaluate_Pole_Right;
      when Skyline =>
        Sky_Line.Add (The_Actual_Direction);
      end case;
      Define_Setup_Buttons;
    when Telescope.Approaching =>
      Perform_Stop;
    when others =>
      null;
    end case;
  exception
  when Pole_Axis.Picture_Not_Found =>
    Telescope.Set_Error ("Picture " & Picture.Filename & " not found");
  when Pole_Axis.Picture_Not_Solved =>
    Telescope.Set_Error ("Picture not solved");
  when Item: others =>
    Log.Termination (Item);
  end Handle_First_Setup_Button;


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
        Signal_Action (Move_Up);
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down | Gui.Key_Codes.K_Down =>
        Signal_Action (Move_Down);
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left | Gui.Key_Codes.K_Left =>
        Signal_Action (Move_Left);
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right | Gui.Key_Codes.K_Right =>
        Signal_Action (Move_Right);
      when Gui.Key_Codes.KP_Add | Gui.Key_Codes.K_Page_Up =>
        Signal_Action (Increase_Moving_Rate);
      when Gui.Key_Codes.KP_Subtract | Gui.Key_Codes.K_Page_Down =>
        Signal_Action (Decrease_Moving_Rate);
      when Gui.Key_Codes.K_Back =>
        Perform_Stop;
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := True;
      when others =>
        null;
      end case;
    when  Gui.Key_Released =>
      Log.Write ("Key released: " & The_Key_Code'img);
      case The_Key_Code is
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up | Gui.Key_Codes.K_Up =>
        Signal_Action (End_Move_Up);
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down | Gui.Key_Codes.K_Down =>
        Signal_Action (End_Move_Down);
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left | Gui.Key_Codes.K_Left =>
        Signal_Action (End_Move_Left);
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right | Gui.Key_Codes.K_Right =>
        Signal_Action (End_Move_Right);
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := False;
      when others =>
        null;
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
                                      The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False]);

        First_Control_Button := Gui.Create (Control_Page, "Start", Perform_First_Control'access);
        Gui.Disable (First_Control_Button);
        Stop_Or_Synch_Button := Gui.Create (Control_Page, "Stop", Perform_Stop_Or_Synch'access);
        Gui.Disable (Stop_Or_Synch_Button);

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
        Moving_Rate_Text : constant String := "Moving Rate";

        Title_Size : constant Natural := Gui.Text_Size_Of (Moving_Rate_Text) + Separation;
        Text_Size  : constant Natural := Natural'max (Gui.Text_Size_Of ("+360d00'00.0"""),
                                                      Gui.Text_Size_Of ("20h58m58.58s")) + Separation;
      begin
        Display_Page := Gui.Add_Page (The_Title  => "Display",
                                      The_Action => Enter_Display_Page'access,
                                      The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False]);

        Target_Ra := Gui.Create (Display_Page, "Target RA", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Target_Dec := Gui.Create (Display_Page, "Target DEC", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);

        Ra_Offset := Gui.Create (Display_Page, "RA Offset", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Dec_Offset := Gui.Create (Display_Page, "DEC Offset", "",
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

        Picture_Ra := Gui.Create (Display_Page, "Picture RA", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
        Picture_Dec := Gui.Create (Display_Page, "Picture DEC", "",
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

        Moving_Rate := Gui.Create (Display_Page, Moving_Rate_Text, "",
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
        Air_Pressure_Text : constant String := "Air Pressure";

        Title_Size : constant Natural := Gui.Text_Size_Of ((if Os.Is_Osx then Setup_Object_Key
                                                                         else Air_Pressure_Text)) + Separation;
        Text_Size  : constant Natural := Gui.Text_Size_Of ("20h58m58.58s") + Separation;
      begin
        Setup_Page := Gui.Add_Page (The_Title  => "Setup",
                                    The_Action => Enter_Setup_Page'access,
                                    The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                   Gui.Buttons_Fill_Vertically   => False]);

        First_Setup_Button := Gui.Create (Setup_Page, "", Handle_First_Setup_Button'access);
        Clear_Button := Gui.Create (Setup_Page, "", Handle_Clear_Button'access);
        Gui.Disable (Clear_Button);

        Longitude := Gui.Create (Setup_Page, "Longitude", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);

        Latitude := Gui.Create (Setup_Page, "Latitude", "",
                                Is_Modifiable  => False,
                                The_Size       => Text_Size,
                                The_Title_Size => Title_Size);

        Elevation := Gui.Create (Setup_Page, "Elevation", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);

        Air_Pressure := Gui.Create (Setup_Page, Air_Pressure_Text, Image_Of (The_Air_Pressure),
                                    The_Action_Routine => Define_Air_Pressure'access,
                                    The_Size           => Text_Size,
                                    The_Title_Size     => Title_Size);
        Temperature := Gui.Create (Setup_Page, "Temperature", Image_Of (The_Temperature),
                                   The_Action_Routine => Define_Temperature'access,
                                   The_Size           => Text_Size,
                                   The_Title_Size     => Title_Size);

        Setup_Control := Gui.Create (Setup_Page, Setup_Object_Key,
                                     The_Action_Routine => Define_Setup_Object'access,
                                     The_Size           => Text_Size,
                                     The_Title_Size     => Title_Size);
        for Value in Setup_Object'range loop
          Gui.Add_Text (Setup_Control, Strings.Legible_Of (Value'img));
        end loop;
        Gui.Select_Text (Setup_Control, Strings.Legible_Of (The_Setup_Object'img));
        Cone_Error := Gui.Create (Setup_Page, "Cone Error", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
        Az_Offset := Gui.Create (Setup_Page, "Az Offset", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Alt_Offset := Gui.Create (Setup_Page, "Alt Offset", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
      end Define_Setup_Page;

    begin -- Create_Interface
      Selection_Menu.Create (Lexicon.Image_Of (Lexicon.Selection), Selection_Handler'access);
      Targets.Set (The_Selection => All_Objects);
      Catalog_Menu.Create (Lexicon.Image_Of (Lexicon.Catalog), Catalog_Handler'access);
      Catalog_Handler (Data.Favorites);
      Define_Control_Page;
      if Persistent_Setup.Storage_Is_Empty then
        The_Air_Pressure := 0;
        The_Temperature := 10;
      end if;
      Refraction.Set (The_Air_Pressure);
      Refraction.Set (The_Temperature);
      Define_Display_Page;
      Define_Setup_Page;
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


  procedure Enable_Align_On_Picture is
  begin
    Align_On_Picture_Is_Enabled := True;
    Align_On_Picture_Change := True;
  end Enable_Align_On_Picture;


  function In_Setup_Mode return Boolean is
  begin
    case The_Page is
    when Is_Setup =>
      return True;
    when others =>
      return False;
    end case;
  end In_Setup_Mode;


  function Setup_Kind return Setup_Object is
  begin
    return The_Setup_Object;
  end Setup_Kind;

end User;
