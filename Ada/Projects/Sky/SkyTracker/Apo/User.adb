-- *********************************************************************************************************************
-- *                        (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                         *
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

with Angle;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Application;
with Alignment;
with Data;
with Earth;
with Gui.Enumeration_Menu_Of;
with Gui.Key_Codes;
with Gui.Registered;
with Lexicon;
with Lx200;
with Objects;
with Os;
with Parameter;
with Persistent;
with Pole_Axis;
with Remote;
with Site;
with Space;
with Strings;
with Targets;
with Time;
with Traces;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Main_Version;

  Setup_Object_Key : constant String := "Setup Object";

  Control_Page   : Gui.Page;
  Action_Button  : Gui.Button;
  Control_Button : Gui.Button;
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
  Pier_Side    : Gui.Plain_Edit_Box;
  Ra_Axis      : Gui.Plain_Edit_Box;
  Dec_Axis     : Gui.Plain_Edit_Box;
  Lmst         : Gui.Plain_Edit_Box;
  Local_Time   : Gui.Plain_Edit_Box;

  Setup_Page           : Gui.Page;
  Setup_Action_Button  : Gui.Button;
  Setup_Control_Button : Gui.Button;
  Air_Pressure_Box     : Gui.Plain_Edit_Box;
  Temperature_Box      : Gui.Plain_Edit_Box;
  Setup_Control        : Gui.Plain_Combo_Box;
  Picture_Ra           : Gui.Plain_Edit_Box;
  Picture_Dec          : Gui.Plain_Edit_Box;
  Align_Points         : Gui.Plain_Edit_Box;
  Az_Pole_Box          : Gui.Plain_Edit_Box;
  Alt_Pole_Box         : Gui.Plain_Edit_Box;
  Pole_Error_Box       : Gui.Plain_Edit_Box;
  Pole_Angle_Box       : Gui.Plain_Edit_Box;
  Ortho_Error_Box      : Gui.Plain_Edit_Box;
  Az_Knob_Left_Box     : Gui.Plain_Edit_Box;
  Alt_Knob_Down_Box    : Gui.Plain_Edit_Box;
  Modeling_Terms_Box   : Gui.Plain_Edit_Box;
  Rms_Error_Box        : Gui.Plain_Edit_Box;
  Cone_Error           : Gui.Plain_Edit_Box;
  Az_Offset            : Gui.Plain_Edit_Box;
  Alt_Offset           : Gui.Plain_Edit_Box;

  type Page is (Is_Control, Is_Display, Is_Setup);

  Is_Expert_Mode : Boolean := False;

  The_Page : Page := Is_Control;

  type Target_Selection is (No_Target, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection := Target_Object;

  The_Alignment_Points        : Natural := 0;
  Alignment_Is_Ready          : Boolean := False;
  Align_Change                : Boolean := False;
  Align_On_Picture_Is_Enabled : Boolean := False;

  The_Setup_Object : Setup_Object := Pole_Left;

  The_Air_Pressure : Refraction.Hectopascal := Refraction.Undefined_Air_Pressure;
  The_Temperature  : Refraction.Celsius := Refraction.Undefined_Temperature;

  subtype State is Telescope.State;

  use all type State;

  subtype Error_State is Telescope.Error_State;

  subtype Transit_State is Telescope.Transit_State;

  The_Status : State := Disconnected;

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


  procedure Perform_Park is
  begin
    Signal_Action (Park);
  end Perform_Park;


  procedure Perform_Unpark is
  begin
    Signal_Action (Unpark);
  end Perform_Unpark;


  procedure Perform_Stop is
  begin
    Signal_Action (Stop);
  end Perform_Stop;


  procedure Perform_Align is
  begin
    Signal_Action (Align);
    Align_On_Picture_Is_Enabled := False;
  end Perform_Align;


  procedure Perform_Goto is
  begin
    Signal_Action (Go_To);
  end Perform_Goto;


  procedure Perform_Setup_Goto is

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

    Setup_Object_Image : constant String := Identifier_Of (Gui.Contents_Of (Setup_Control));

  begin -- Perform_Setup_Goto
    The_Setup_Object := Setup_Object'value(Setup_Object_Image);
    Log.Write ("Setup Object - " & Setup_Object_Image);
    case The_Setup_Object is
    when Align_Next =>
      Signal_Action (Go_To_Next);
    when Pole_Left =>
      Signal_Action (Go_To_Left);
    when Pole_Right =>
      Signal_Action (Go_To_Right);
    when Pole_Top =>
      Signal_Action (Go_To_Top);
    end case;
  exception
  when others =>
    Log.Error ("Perform_Setup_Goto");
  end Perform_Setup_Goto;


  procedure Perform_Clear is
  begin
    Pole_Axis.Clear;
    Alignment.Clear;
  end Perform_Clear;


  procedure No_Operation is
  begin
    null;
  end No_Operation;


  type Procedure_Access is not null access procedure;

  Perform_Action : Procedure_Access := No_Operation'access;

  Perform_Control : Procedure_Access := No_Operation'access;

  Perform_Setup_Action : Procedure_Access := No_Operation'access;

  Perform_Setup_Control : Procedure_Access := No_Operation'access;

  The_Actual_Direction : Earth.Direction; -- used for Skyline


  function Image_Of (The_Value : Refraction.Hectopascal) return String is
  begin
    return Strings.Trimmed (The_Value'img) & "hPa";
  end Image_Of;


  function Image_Of (The_Value : Refraction.Celsius) return String is
  begin
    return Strings.Trimmed (The_Value'img) & "°C";
  end Image_Of;


  procedure Show (Visible_In : Duration) is
  begin
    Show_Description (Targets.Text_Of (Visible_In));
  end Show;


  procedure Show (Information : Telescope.Data) is

    procedure Disable (The_Button : Gui.Button) is
    begin
      Gui.Set_Text (The_Button, "");
      Gui.Disable (The_Button);
    end Disable;

    procedure Enable (The_Button : Gui.Button;
                      The_Text   : String) is
    begin
      Gui.Set_Text (The_Button, The_Text);
      Gui.Enable (The_Button);
    end Enable;

    procedure Define_Control_Buttons is

      procedure Disable_Action is
      begin
        Disable (Action_Button);
        Perform_Action := No_Operation'access;
      end Disable_Action;

      procedure Disable_Control is
      begin
        Disable (Control_Button);
        Perform_Control := No_Operation'access;
      end Disable_Control;

      procedure Enable_Action (The_Text   : String;
                               The_Action : Procedure_Access) is
      begin
        Enable (Action_Button, The_Text);
        Perform_Action := The_Action;
      end Enable_Action;

      procedure Enable_Control (The_Text    : String;
                                The_Control : Procedure_Access) is
      begin
        Enable (Control_Button, The_Text);
        Perform_Control := The_Control;
      end Enable_Control;

      procedure Enable_Goto is
      begin
        if The_Target_Selection = No_Target then
          Disable_Action;
        else
          Enable_Action ("Goto", Perform_Goto'access);
        end if;
      end Enable_Goto;

      procedure Enable_Stop is
      begin
        Enable_Control ("Stop", Perform_Stop'access);
      end Enable_Stop;

    begin -- Define_Control_Buttons
      case Information.Status is
      when Disconnected | Error_State | Inhibited | Unparking =>
        Align_On_Picture_Is_Enabled := False;
        Disable_Action;
        Disable_Control;
      when Parked =>
        Disable_Action;
        Enable_Control ("Unpark", Perform_Unpark'access);
      when Slewing | Parking | Homing | Following | Transit_State | Positioned =>
        Align_On_Picture_Is_Enabled := False;
        Enable_Goto;
        Enable_Stop;
      when Stopped | Outside =>
        Enable_Goto;
        Enable_Control ("Park", Perform_Park'access);
      when Tracking =>
        if Align_On_Picture_Is_Enabled then
          Enable_Action ("Align", Perform_Align'access);
        else
          Enable_Goto;
        end if;
        Enable_Stop;
      when Solving =>
        Disable_Action;
        Enable_Stop;
      end case;
    end Define_Control_Buttons;


    procedure Define_Setup_Buttons is

      procedure Disable_Action is
      begin
        Disable (Setup_Action_Button);
        Perform_Setup_Action := No_Operation'access;
      end Disable_Action;

      procedure Disable_Control is
      begin
        Disable (Setup_Control_Button);
        Perform_Setup_Control := No_Operation'access;
      end Disable_Control;

      procedure Enable_Action (The_Text   : String;
                               The_Action : Procedure_Access) is
      begin
        Enable (Setup_Action_Button, The_Text);
        Perform_Setup_Action := The_Action;
      end Enable_Action;

      procedure Enable_Control (The_Text    : String;
                                The_Control : Procedure_Access) is
      begin
        Enable (Setup_Control_Button, The_Text);
        Perform_Setup_Control := The_Control;
      end Enable_Control;

      procedure Enable_Goto is
      begin
        Enable_Action ("Goto", Perform_Setup_Goto'access);
      end Enable_Goto;

      procedure Enable_Stop is
      begin
        Enable_Control ("Stop", Perform_Stop'access);
      end Enable_Stop;

    begin
      case Information.Status is
      when Disconnected | Error_State | Inhibited | Unparking =>
        Disable_Action;
        Disable_Control;
        Gui.Disable (Temperature_Box);
      when Parked =>
        Disable_Action;
        Enable_Control ("Unpark", Perform_Unpark'access);
      when Slewing | Parking | Homing | Following | Transit_State | Positioned =>
        Enable_Goto;
        if Pole_Axis.Has_Values then
          Enable_Control ("Clear", Perform_Clear'access);
        else
          Enable_Stop;
        end if;
      when Stopped | Outside =>
        if Alignment.Ready then
          Enable_Action ("Align", Perform_Align'access);
        else
          Enable_Goto;
        end if;
        if Alignment.Star_Count > 0 then
          Enable_Control ("Clear", Perform_Clear'access);
        else
          Enable_Control ("Park", Perform_Park'access);
        end if;
      when Tracking =>
        Enable_Goto;
        Enable_Stop;
      when Solving =>
        Disable_Action;
        Enable_Stop;
      end case;
    end Define_Setup_Buttons;


    procedure Define_Settings is
    begin
      case Information.Status is
      when Disconnected | Error_State =>
        Gui.Disable (Air_Pressure_Box);
        Gui.Disable (Temperature_Box);
      when others =>
        Gui.Enable (Air_Pressure_Box);
        Gui.Enable (Temperature_Box);
      end case;
    end Define_Settings;


    procedure Clear_Actual_Values is
    begin
      Gui.Set_Text (Actual_Dec, "");
      Gui.Set_Text (Actual_Ra, "");
      Gui.Set_Text (Actual_Alt, "");
      Gui.Set_Text (Actual_Az, "");
      Gui.Set_Text (Pier_Side, "");
      The_Actual_Direction := Earth.Unknown_Direction;
    end Clear_Actual_Values;

    function Image_Of (Item         : String;
                       No_Value_For : String := "") return String is
      Image : constant String := Strings.Trimmed (Item);
    begin
      if Image = No_Value_For then
        return "";
      end if;
      return Image;
    end Image_Of;

    use type Angle.Value;
    use type Time.Ut;

  begin -- Show
    if (The_Status /= Information.Status)
      or (Last_Target_Selection /= The_Target_Selection)
      or Align_Change
    then
      Align_Change := False;
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      Define_Control_Buttons;
      if Is_Expert_Mode then
        Define_Setup_Buttons;
        Define_Settings;
      end if;
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
        Gui.Set_Text (Pier_Side, [Information.Mount_Pier_Side]);
      else
        Clear_Actual_Values;
      end if;
      if Space.Direction_Is_Known (Information.Actual_Position) then
        Gui.Set_Text (Dec_Axis, Lx200.Position_Of (Space.Dec_Of (Information.Actual_Position)) & Angle.Degree);
        Gui.Set_Text (Ra_Axis, Lx200.Position_Of (Space.Ra_Of (Information.Actual_Position)) & Angle.Degree);
      else
        Gui.Set_Text (Dec_Axis, "");
        Gui.Set_Text (Ra_Axis, "");
      end if;
      if Information.Universal_Time = Time.Unknown then
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
    when Is_Setup =>
      if Refraction.New_Air_Pressure then
        Gui.Set_Text (Air_Pressure_Box, Image_Of (Refraction.Air_Pressure));
      end if;
      if Refraction.New_Temperature then
        Gui.Set_Text (Temperature_Box, Image_Of (Refraction.Temperature));
      end if;
      if Space.Direction_Is_Known (Information.Picture_Direction) then
        Gui.Set_Text (Picture_Dec, Space.Dec_Image_Of (Information.Picture_Direction));
        Gui.Set_Text (Picture_Ra, Space.Ra_Image_Of (Information.Picture_Direction));
      else
        Gui.Set_Text (Picture_Dec, "");
        Gui.Set_Text (Picture_Ra, "");
      end if;
      Align_Change := The_Alignment_Points /= Information.Align_Points or Alignment.Ready /= Alignment_Is_Ready;
      Alignment_Is_Ready := Alignment.Ready;
      The_Alignment_Points := Information.Align_Points;
      Gui.Set_Text (Align_Points, Image_Of (The_Alignment_Points'image, No_Value_For => "0"));
      declare
        Info : Alignment.Information renames Information.Alignment_Info;
      begin
        if Earth.Direction_Is_Known (Info.Ra_Axis_Direction) then
          Gui.Set_Text (Az_Pole_Box, Earth.Az_Offset_Image_Of (Info.Ra_Axis_Direction));
          Gui.Set_Text (Alt_Pole_Box, Earth.Alt_Offset_Image_Of (Info.Ra_Axis_Direction));
          Gui.Set_Text (Pole_Error_Box, Image_Of (Info.Polar_Align_Error'image));
          Gui.Set_Text (Pole_Angle_Box, Image_Of (Info.Ra_Axis_Angle'image));
          Gui.Set_Text (Ortho_Error_Box, Image_Of (Info.Orthogonality_Error'image, No_Value_For => "0.0000"));
          Gui.Set_Text (Az_Knob_Left_Box, Image_Of (Info.Az_Knob_Turns_Left'image));
          Gui.Set_Text (Alt_Knob_Down_Box, Image_Of (Info.Alt_Knob_Turns_Down'image));
          Gui.Set_Text (Modeling_Terms_Box, Image_Of (Info.Modeling_Terms'image, No_Value_For => "0"));
          Gui.Set_Text (Rms_Error_Box, Image_Of (Info.Rms_Error'image, No_Value_For => "0.0"));
        else
          Gui.Set_Text (Az_Pole_Box, "");
          Gui.Set_Text (Alt_Pole_Box, "");
          Gui.Set_Text (Pole_Error_Box, "");
          Gui.Set_Text (Pole_Angle_Box, "");
          Gui.Set_Text (Ortho_Error_Box, "");
          Gui.Set_Text (Az_Knob_Left_Box, "");
          Gui.Set_Text (Alt_Knob_Down_Box, "");
          Gui.Set_Text (Modeling_Terms_Box, "");
          Gui.Set_Text (Rms_Error_Box, "");
        end if;
      end;
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


  procedure Handle_Action is
  begin
    Perform_Action.all;
  end Handle_Action;


  procedure Handle_Control is
  begin
    Perform_Control.all;
  end Handle_Control;


  procedure Handle_Setup_Action is
  begin
    Perform_Setup_Action.all;
  end Handle_Setup_Action;


  procedure Handle_Setup_Control is
  begin
    Perform_Setup_Control.all;
  end Handle_Setup_Control;


  function Air_Pressure return Refraction.Hectopascal is
  begin
    return The_Air_Pressure;
  end Air_Pressure;


  function Temperature return Refraction.Celsius is
  begin
    return The_Temperature;
  end Temperature;


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


  procedure Define_Air_Pressure is
  begin
    declare
      Value : constant String := Strings.Trimmed (Gui.Contents_Of (Air_Pressure_Box));
      Last  : Natural := Value'last;
    begin
      loop
        exit when Value(Last) in '0'..'9';
        Last := Last - 1;
      end loop;
      The_Air_Pressure := Refraction.Hectopascal'value(Value(Value'first .. Last));
      Signal_Action (Define_Air_Pressure);
    exception
    when others =>
      Show_Error ("Incorrect Air Pressure: " & Value);
    end;
  exception
  when others =>
    Log.Error ("Define_Air_Pressure");
  end Define_Air_Pressure;


  procedure Define_Temperature is
  begin
    declare
      Value : constant String := Strings.Trimmed (Gui.Contents_Of (Temperature_Box));
      Last  : Natural := Value'last;
    begin
      loop
        exit when Value(Last) in '0'..'9';
        Last := Last - 1;
      end loop;
      The_Temperature := Refraction.Celsius'value(Value(Value'first .. Last));
      Signal_Action (Define_Temperature);
    exception
    when others =>
      Show_Error ("Incorrect Temperature: " & Value);
    end;
  exception
  when others =>
    Log.Error ("Define_Temperature");
  end Define_Temperature;


  The_Targets : Name.Id_List_Access;

  procedure Enter_Control_Page is
  begin
    The_Page := Is_Control;
    Catalog_Menu.Enable;
    Selection_Menu.Enable;
    Gui.Enable_Key_Handler;
  end Enter_Control_Page;


  procedure Enter_Display_Page is
  begin
    The_Page := Is_Display;
    Catalog_Menu.Disable;
    Selection_Menu.Disable;
    Gui.Enable_Key_Handler;
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


  procedure Put (The_Command : Telescope.Command) is
  begin
    Telescope.Execute (The_Command);
  end Put;


  Ignore_Next : Boolean := False;


  procedure Key_Handler (The_Event    : Gui.Key_Event;
                         The_Key_Code : Gui.Key_Code) is
    use all type Telescope.Command;
  begin
    case The_Page is
    when Is_Setup =>
      return;
    when Is_Control | Is_Display =>
      null;
    end case;
    case The_Event is
    when Gui.Key_Pressed =>
      Log.Write ("Key pressed: " & The_Key_Code'img);
      if Ignore_Next then
        return;
      end if;
      case The_Key_Code is
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down =>
        Put (Move_Down);
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up =>
        Put (Move_Up);
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left =>
        Put (Move_Left);
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right =>
        Put (Move_Right);
      when Gui.Key_Codes.KP_Add | Gui.Key_Codes.K_Page_Up =>
        Put (Increase_Moving_Rate);
      when Gui.Key_Codes.KP_Subtract | Gui.Key_Codes.K_Page_Down =>
        Put (Decrease_Moving_Rate);
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := True;
      when others =>
        null;
      end case;
    when Gui.Key_Released =>
      Log.Write ("Key released: " & The_Key_Code'img);
      case The_Key_Code is
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down =>
        Put (Move_Down_End);
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up =>
        Put (Move_Up_End);
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left =>
        Put (Move_Left_End);
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right =>
        Put (Move_Right_End);
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := True;
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

        Action_Button := Gui.Create (Control_Page, "", Handle_Action'access);
        Gui.Disable (Action_Button);
        Control_Button := Gui.Create (Control_Page, "", Handle_Control'access);
        Gui.Disable (Control_Button);

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
        Local_Time_Text : constant String := "Local Time";

        Title_Size : constant Natural := Gui.Text_Size_Of (Local_Time_Text) + Separation;
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
        Target_Dec := Gui.Create (Display_Page, "Target Dec", "",
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

        Pier_Side := Gui.Create (Display_Page, "Pier Side", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);

        Ra_Axis := Gui.Create (Display_Page, "RA Axis", "",
                               Is_Modifiable  => False,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);
        Dec_Axis := Gui.Create (Display_Page, "Dec Axis", "",
                                Is_Modifiable  => False,
                                The_Size       => Text_Size,
                                The_Title_Size => Title_Size);

        Lmst := Gui.Create (Display_Page, "LMST", "",
                            Is_Modifiable  => False,
                            The_Size       => Text_Size,
                            The_Title_Size => Title_Size);

        Local_Time := Gui.Create (Display_Page, Local_Time_Text, "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
      end Define_Display_Page;

      procedure Define_Setup_Page is

        -- largest texts
        Air_Pressure_Text : constant String := "Air Pressure";
        Title_Size : constant Natural := Gui.Text_Size_Of (Air_Pressure_Text) + Separation;
        Text_Size  : constant Natural := Gui.Text_Size_Of ("20h58m58.58s") + Separation;
      begin
        Setup_Page := Gui.Add_Page (The_Title  => "Setup",
                                    The_Action => Enter_Setup_Page'access,
                                    The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                   Gui.Buttons_Fill_Vertically   => False]);

        Setup_Action_Button := Gui.Create (Setup_Page, "", Handle_Setup_Action'access);
        Gui.Disable (Setup_Action_Button);
        Setup_Control_Button := Gui.Create (Setup_Page, "", Handle_Setup_Control'access);
        Gui.Disable (Setup_Control_Button);

        Air_Pressure_Box := Gui.Create (Setup_Page, Air_Pressure_Text, "",
                                        The_Action_Routine => Define_Air_Pressure'access,
                                        The_Size           => Text_Size,
                                        The_Title_Size     => Title_Size);
        Gui.Disable (Air_Pressure_Box);
        Temperature_Box := Gui.Create (Setup_Page, "Temperature", "",
                                       The_Action_Routine => Define_Temperature'access,
                                       The_Size           => Text_Size,
                                       The_Title_Size     => Title_Size);
        Gui.Disable (Temperature_Box);

        Setup_Control := Gui.Create (Setup_Page, Setup_Object_Key,
                                     The_Size           => Text_Size,
                                     The_Title_Size     => Title_Size);
        for Value in Setup_Object'range loop
          Gui.Add_Text (Setup_Control, Strings.Legible_Of (Value'img));
        end loop;
        Gui.Select_Text (Setup_Control, Strings.Legible_Of (The_Setup_Object'img));

        Picture_Ra := Gui.Create (Setup_Page, "Picture RA", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
        Picture_Dec := Gui.Create (Setup_Page, "Picture DEC", "",
                                   Is_Modifiable  => False,
                                   The_Size       => Text_Size,
                                   The_Title_Size => Title_Size);

        Align_Points := Gui.Create (Setup_Page, "Align Points", "",
                                    Is_Modifiable  => False,
                                    The_Size       => Text_Size,
                                    The_Title_Size => Title_Size);

        Az_Pole_Box := Gui.Create (Setup_Page, "Az Pole", "",
                                   Is_Modifiable  => False,
                                   The_Size       => Text_Size,
                                   The_Title_Size => Title_Size);
        Alt_Pole_Box := Gui.Create (Setup_Page, "Alt Pole", "",
                                    Is_Modifiable  => False,
                                    The_Size       => Text_Size,
                                    The_Title_Size => Title_Size);
        Pole_Error_Box := Gui.Create (Setup_Page, "Pole Error", "",
                                      Is_Modifiable  => False,
                                      The_Size       => Text_Size,
                                      The_Title_Size => Title_Size);
        Pole_Angle_Box := Gui.Create (Setup_Page, "Pole Angle", "",
                                      Is_Modifiable  => False,
                                      The_Size       => Text_Size,
                                      The_Title_Size => Title_Size);
        Ortho_Error_Box := Gui.Create (Setup_Page, "Cone Error", "",
                                       Is_Modifiable  => False,
                                       The_Size       => Text_Size,
                                       The_Title_Size => Title_Size);
        Az_Knob_Left_Box := Gui.Create (Setup_Page, "Knob Turns <", "",
                                        Is_Modifiable  => False,
                                        The_Size       => Text_Size,
                                        The_Title_Size => Title_Size);
        Alt_Knob_Down_Box := Gui.Create (Setup_Page, "Knob Turns v", "",
                                         Is_Modifiable  => False,
                                         The_Size       => Text_Size,
                                         The_Title_Size => Title_Size);
        Modeling_Terms_Box := Gui.Create (Setup_Page, "Model Terms", "",
                                          Is_Modifiable  => False,
                                          The_Size       => Text_Size,
                                          The_Title_Size => Title_Size);
        Rms_Error_Box := Gui.Create (Setup_Page, "RMS Error", "",
                                     Is_Modifiable  => False,
                                     The_Size       => Text_Size,
                                     The_Title_Size => Title_Size);

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
      if Parameter.Remote_Configured then
        Demo_21_Menu.Create ("Demo 21", Demo_21_Handler'access);
      end if;
      Define_Control_Page;
      Is_Expert_Mode := Parameter.Is_Expert_Mode;
      if Is_Expert_Mode then
        Define_Display_Page;
        Define_Setup_Page;
      end if;
      Gui.Enable_Key_Handler;
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
    Align_Change := True;
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

end User;
