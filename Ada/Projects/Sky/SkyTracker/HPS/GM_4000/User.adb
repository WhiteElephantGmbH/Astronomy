-- *********************************************************************************************************************
-- *                        (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                         *
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
with Alignment;
with Application;
with Camera;
with Earth;
with Focus;
with Gui.Enumeration_Menu_Of;
with Gui.Registered;
with Lexicon;
with Lx200;
with Name.Catalog;
with Objects;
with Os;
with Persistent;
with Refraction;
with Remote;
with Site;
with Sky.Catalog;
with Space;
with Targets.Filter;
with Ten_Micron;
with Text;
with Time;
with Traces;

package body User is

  package Log is new Traces ("User");

  Command_Key : constant String := "Command";

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
  Time_Offset  : Gui.Plain_Edit_Box;
  Air_Pressure : Gui.Plain_Edit_Box;
  Temperature  : Gui.Plain_Edit_Box;

  Setup_Page           : Gui.Page;
  Setup_Action_Button  : Gui.Button;
  Setup_Control_Button : Gui.Button;
  Setup_Command_Box    : Gui.Plain_Combo_Box;
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
  Camera_Model_Box     : Gui.Plain_Edit_Box;
  Camera_State_Box     : Gui.Plain_Edit_Box;
  Focuser_Model_Box    : Gui.Plain_Edit_Box;
  Focus_State_Box      : Gui.Plain_Edit_Box;
  Half_Flux_Box        : Gui.Plain_Edit_Box;
  Focus_HFD_Box        : Gui.Plain_Edit_Box;
  Focus_Position_Box   : Gui.Plain_Edit_Box;

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

  The_Setup_Command    : Setup_Command := Auto_Focus;
  Setup_Command_Change : Boolean := False;
  Is_Focusing          : Boolean := False;

  subtype State is Telescope.State;

  use all type State;

  subtype Error_State is Telescope.Error_State;

  subtype Transit_State is Telescope.Transit_State;

  The_Status : State := Disconnected;

  Action_Routine   : Action_Handler;
  The_Last_Action  : Action := Action'pred (Button_Action'first);
  Last_Action_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;

  Setup_Command_Is_Active : Boolean := False;


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
    Setup_Command_Is_Active := False;
    Signal_Action (Stop);
  end Perform_Stop;


  procedure Perform_Align is
  begin
    Signal_Action (Align);
    Setup_Command_Is_Active := False;
    Align_On_Picture_Is_Enabled := False;
  end Perform_Align;


  procedure Perform_Goto is
  begin
    Signal_Action (Go_To);
  end Perform_Goto;


  procedure Perform_Goto_Next is
  begin
    Signal_Action (Go_To_Next);
  end Perform_Goto_Next;


  procedure Perform_Setup_Command is
  begin
    Setup_Command_Is_Active := True;
    Log.Write ("Setup Command - " & Text.Legible_Of (The_Setup_Command'image));
    case The_Setup_Command is
    when Auto_Focus =>
      Signal_Action (Auto_Focus_Start);
      Is_Focusing := True;
    when Align_Stars =>
      Signal_Action (Go_To_Next);
      Is_Focusing := False;
    end case;
  exception
  when others =>
    Log.Error ("Perform_Setup_Command");
  end Perform_Setup_Command;


  procedure Perform_Clear is
  begin
    Setup_Command_Is_Active := False;
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
    return Text.Trimmed (The_Value'img) & "hPa";
  end Image_Of;


  function Image_Of (The_Value : Refraction.Celsius) return String is
  begin
    return Text.Trimmed (The_Value'img) & "°C";
  end Image_Of;


  procedure Show (Visible_In : Duration) is
  begin
    Show_Description (Targets.Text_Of (Visible_In));
  end Show;


  procedure Show (Information : Telescope.Data) is

    function Image_Of (Item         : String;
                       No_Value_For : String := "") return String is
      Image : constant String := Text.Trimmed (Item);
    begin
      if Image = No_Value_For then
        return "";
      end if;
      return Image;
    end Image_Of;

    procedure Show_Camera_Information is
    begin
      Gui.Set_Text (Camera_Model_Box, Camera.Model_Image);
      Gui.Set_Text (Camera_State_Box, Text.Legible_Of (Camera.Actual_Information.State'image));
    end Show_Camera_Information;

    procedure Show_Focus_Information is
      Focusing_State : constant Focus.Status := Focus.Actual_State;
      Evaluation     : constant Focus.Result := Focus.Evaluation_Result;
    begin
      case Focusing_State is
      when Focus.No_Focuser =>
        Gui.Set_Text (Focuser_Model_Box, "");
        Gui.Set_Text (Focus_State_Box, "");
        Gui.Set_Text (Half_Flux_Box, "");
        Gui.Set_Text (Focus_HFD_Box, "");
        Gui.Set_Text (Focus_Position_Box, "");
      when others =>
        Gui.Set_Text (Focuser_Model_Box, Focus.Focuser_Image);
        Gui.Set_Text (Focus_State_Box, Text.Legible_Of (Focusing_State'image));
        Gui.Set_Text (Half_Flux_Box, Image_Of (Evaluation.Half_Flux'image, No_Value_For => "0"));
        Gui.Set_Text (Focus_HFD_Box, Image_Of (Evaluation.HFD'image, No_Value_For => "0"));
        Gui.Set_Text (Focus_Position_Box, Image_Of (Evaluation.Position'image, No_Value_For => "0"));
      end case;
      case Focusing_State is
      when Focus.No_Focuser | Focus.Undefined | Focus.Evaluated =>
        if Is_Focusing then
          Is_Focusing := False;
          Setup_Command_Is_Active := False;
        end if;
      when Focus.Positioning | Focus.Capturing | Focus.Failed =>
        null;
      end case;
    end Show_Focus_Information;

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
      when Capturing | Focusing | Solving =>
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

      procedure Enable_Command is
      begin
        Enable_Action ("Do", Perform_Setup_Command'access);
      end Enable_Command;

      procedure Enable_Stop is
      begin
        Enable_Control ("Stop", Perform_Stop'access);
      end Enable_Stop;

    begin -- Define_Setup_Buttons
      case Information.Status is
      when Disconnected | Error_State | Inhibited | Unparking =>
        Disable_Action;
        Disable_Control;
      when Parked =>
        Disable_Action;
        Enable_Control ("Unpark", Perform_Unpark'access);
      when Parking | Homing | Following | Transit_State | Positioned =>
        Disable_Action;
        Enable_Stop;
      when Stopped | Outside =>
        if Alignment.Ready then
          Enable_Action ("Align", Perform_Align'access);
        else
          case The_Setup_Command is
          when Auto_Focus =>
            Disable_Action;
          when Align_Stars =>
            Enable_Command;
          end case;
        end if;
        if Alignment.Star_Count > 0 then
          Enable_Control ("Clear", Perform_Clear'access);
        else
          Enable_Control ("Park", Perform_Park'access);
        end if;
      when Tracking =>
        if not Setup_Command_Is_Active then
          Enable_Command;
        end if;
        Enable_Stop;
      when Capturing | Focusing | Slewing | Solving =>
        Disable_Action;
        Enable_Stop;
      end case;
    end Define_Setup_Buttons;

    procedure Clear_Actual_Values is
    begin
      Gui.Set_Text (Actual_Dec, "");
      Gui.Set_Text (Actual_Ra, "");
      Gui.Set_Text (Actual_Alt, "");
      Gui.Set_Text (Actual_Az, "");
      Gui.Set_Text (Pier_Side, "");
      The_Actual_Direction := Earth.Unknown_Direction;
    end Clear_Actual_Values;

    use type Time.Ut;
    use type Telescope.Time_Offset;

  begin -- Show
    if (The_Status /= Information.Status)
      or (Last_Target_Selection /= The_Target_Selection)
      or Align_Change
      or Setup_Command_Change
    then
      Align_Change := False;
      Setup_Command_Change := False;
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      Define_Control_Buttons;
      if Is_Expert_Mode then
        Define_Setup_Buttons;
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
      if Information.Time_Delta = 0.0 then
        Gui.Set_Text (Time_Offset, "");
      else
        Gui.Set_Text (Time_Offset, Telescope.Image_Of (Information.Time_Delta));
      end if;
      if Refraction.New_Air_Pressure then
        Gui.Set_Text (Air_Pressure, Image_Of (Refraction.Air_Pressure));
      end if;
      if Refraction.New_Temperature then
        Gui.Set_Text (Temperature, Image_Of (Refraction.Temperature));
      end if;
    when Is_Setup =>
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
      Show_Camera_Information;
      Show_Focus_Information;
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


  procedure Handle_Setup_Command_Change is

    function Identifier_Of (Item : String) return String is
      The_Image : String := Text.Trimmed (Item);
    begin
      for Index in The_Image'range loop
        if The_Image(Index) = ' ' then
          The_Image(Index) := '_';
        end if;
      end loop;
      return The_Image;
    end Identifier_Of;

    Setup_Command_Image : constant String := Identifier_Of (Gui.Contents_Of (Setup_Command_Box));

  begin -- Handle_Setup_Command_Change
    The_Setup_Command := Setup_Command'value(Setup_Command_Image);
    Setup_Command_Change := True;
  end Handle_Setup_Command_Change;


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
  end Enter_Control_Page;


  procedure Enter_Display_Page is
  begin
    The_Page := Is_Display;
  end Enter_Display_Page;


  procedure Enter_Setup_Page is
  begin
    The_Page := Is_Setup;
  end Enter_Setup_Page;


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
        return Name.Image_Of (Name_Id.all);
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


  procedure Define_Signal is
  begin
    Signal_Action (Define_Catalog);
  end Define_Signal;


  procedure Update_Signal is
  begin
    Signal_Action (Update);
  end Update_Signal;


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
      end Define_Control_Page;


      procedure Define_Display_Page is
        -- largest text
        Air_Pressure_Text : constant String := "Air Pressure";

        Title_Size : constant Natural := Gui.Text_Size_Of (Air_Pressure_Text) + Separation;
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

        Local_Time := Gui.Create (Display_Page, "Local Time", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);

        Time_Offset := Gui.Create (Display_Page, "Time Δ", "",
                                   Is_Modifiable  => False,
                                   The_Size       => Text_Size,
                                   The_Title_Size => Title_Size);

        Air_Pressure := Gui.Create (Display_Page, Air_Pressure_Text, "",
                                    Is_Modifiable  => False,
                                    The_Size       => Text_Size,
                                    The_Title_Size => Title_Size);
        Temperature := Gui.Create (Display_Page, "Temperature", "",
                                   Is_Modifiable  => False,
                                   The_Size       => Text_Size,
                                   The_Title_Size => Title_Size);
      end Define_Display_Page;


      procedure Define_Setup_Page is

        -- largest text
        Align_Points_Text : constant String := "Align Points";

        Title_Size : constant Natural := Gui.Text_Size_Of (Align_Points_Text) + Separation;
        Text_Size  : constant Natural := Gui.Text_Size_Of ("Align Stars v") + Separation;
      begin
        Setup_Page := Gui.Add_Page (The_Title  => "Setup",
                                    The_Action => Enter_Setup_Page'access,
                                    The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                   Gui.Buttons_Fill_Vertically   => False]);

        Setup_Action_Button := Gui.Create (Setup_Page, "", Handle_Setup_Action'access);
        Gui.Disable (Setup_Action_Button);
        Setup_Control_Button := Gui.Create (Setup_Page, "", Handle_Setup_Control'access);
        Gui.Disable (Setup_Control_Button);

        Setup_Command_Box := Gui.Create (Setup_Page, Command_Key, Handle_Setup_Command_Change'access,
                                         The_Size       => Text_Size,
                                         The_Title_Size => Title_Size);
        for Value in Setup_Command'range loop
          Gui.Add_Text (Setup_Command_Box, Text.Legible_Of (Value'img));
        end loop;
        Gui.Select_Text (Setup_Command_Box, Text.Legible_Of (The_Setup_Command'img));

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
        Camera_Model_Box := Gui.Create (Setup_Page, "Camera", "",
                                        Is_Modifiable  => False,
                                        The_Size       => Text_Size,
                                        The_Title_Size => Title_Size);
        Camera_State_Box := Gui.Create (Setup_Page, "Camera State", "",
                                        Is_Modifiable  => False,
                                        The_Size       => Text_Size,
                                        The_Title_Size => Title_Size);
        Focuser_Model_Box := Gui.Create (Setup_Page, "Focuser", "",
                                         Is_Modifiable  => False,
                                         The_Size       => Text_Size,
                                         The_Title_Size => Title_Size);
        Focus_State_Box := Gui.Create (Setup_Page, "Focus State", "",
                                       Is_Modifiable  => False,
                                       The_Size       => Text_Size,
                                       The_Title_Size => Title_Size);
        Half_Flux_Box := Gui.Create (Setup_Page, "Half Flux", "",
                                     Is_Modifiable  => False,
                                     The_Size       => Text_Size,
                                     The_Title_Size => Title_Size);
        Focus_HFD_Box := Gui.Create (Setup_Page, "Focus HFD", "",
                                     Is_Modifiable  => False,
                                     The_Size       => Text_Size,
                                     The_Title_Size => Title_Size);
        Focus_Position_Box := Gui.Create (Setup_Page, "Auto Focus", "",
                                          Is_Modifiable  => False,
                                          The_Size       => Text_Size,
                                          The_Title_Size => Title_Size);
      end Define_Setup_Page;

    begin -- Create_Interface
      Name.Catalog.Create_Menu (Define_Signal'access);
      Targets.Filter.Create_Menu (Update_Signal'access);
      if Remote.Configured then
        Demo_21_Menu.Create ("Demo 21", Demo_21_Handler'access);
      end if;
      Define_Control_Page;
      Is_Expert_Mode := Ten_Micron.Is_Expert_Mode;
      if Is_Expert_Mode then
        Define_Display_Page;
        Define_Setup_Page;
      end if;
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;


    procedure Termination is
    begin
      The_Display_Data.Width := Gui.Width_Of (The_Targets_Column);
      Signal_Action (Close);
      Log.Write ("Terminating");
      The_Termination_Handler.all;
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Action_Routine := The_Action_Handler;
    Gui.Registered.Execute (The_Application_Name    => Application.Title,
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
    Gui.Set_Title (The_Targets_Column, Sky.Catalog.Image_Of (The_Targets.Kind));
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

  begin -- Update_Targets
    Name.Update (The_Targets, Remove_Target'access, Insert_Target'access);
  end Update_Targets;


  procedure Enable_Align_On_Picture is
  begin
    Align_On_Picture_Is_Enabled := True;
    Align_Change := True;
  end Enable_Align_On_Picture;


  function Window_Minimized return Boolean is (Gui.Application_Is_Minimized);


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
