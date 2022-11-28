-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Alignment;
with Angle;
with Application;
with Astro;
with Catalog;
with Data;
with Device;
with Earth;
with Gui.Enumeration_Menu_Of;
with Gui.Key_Codes;
with Gui.Registered;
with Lexicon;
with Matrix;
with Motor;
with Objects;
with Parameter;
with Persistent;
with Picture;
with Pole_Axis;
with Refraction;
with Sky_Line;
with Site;
with Space;
with Strings;
with Targets;
with Time;
with Traces;
with User.Input;

package body User is

  package Log is new Traces ("User");

  Application_Name : constant String := Application.Name;
  Version          : constant String := Application.Version;

  Orientation_Key       : constant String := "Orientation";
  Operation_Control_Key : constant String := "Operation";

  Control_Page         : Gui.Page;
  First_Control_Button : Gui.Button;
  Stop_Or_Synch_Button : Gui.Button;
  Progress_Bar         : Gui.Progress_Bar;
  Target               : Gui.Plain_Edit_Box;
  Description          : Gui.Plain_Edit_Box;
  Display              : Gui.List_View;

  Display_Page      : Gui.Page;
  Target_Ra         : Gui.Plain_Edit_Box;
  Target_Dec        : Gui.Plain_Edit_Box;
  Actual_Ra         : Gui.Plain_Edit_Box;
  Actual_Dec        : Gui.Plain_Edit_Box;
  Actual_Az         : Gui.Plain_Edit_Box;
  Actual_Alt        : Gui.Plain_Edit_Box;
  Az_Adjustment     : Gui.Plain_Edit_Box;
  Alt_Adjustment    : Gui.Plain_Edit_Box;
  Az_Offset         : Gui.Plain_Edit_Box;
  Alt_Offset        : Gui.Plain_Edit_Box;
  Motor_1           : Gui.Plain_Edit_Box;
  Motor_2           : Gui.Plain_Edit_Box;
  Flipped           : Gui.Plain_Edit_Box;
  Board_Temperature : Gui.Plain_Edit_Box;
  Synch_State       : Gui.Plain_Edit_Box;
  Lmst              : Gui.Plain_Edit_Box;
  Local_Time        : Gui.Plain_Edit_Box;
  Time_Offset       : Gui.Plain_Edit_Box;

  Setup_Page           : Gui.Page;
  First_Setup_Button   : Gui.Button;
  Second_Setup_Button  : Gui.Button;
  Longitude            : Gui.Plain_Edit_Box;
  Latitude             : Gui.Plain_Edit_Box;
  Elevation            : Gui.Plain_Edit_Box;
  Orientation_Box      : Gui.Plain_Combo_Box;
  Autoguiding_Rate     : Gui.Plain_Edit_Box;
  Air_Pressure         : Gui.Plain_Edit_Box;
  Temperature          : Gui.Plain_Edit_Box;
  Operation_Control    : Gui.Plain_Combo_Box;
  Magnitude_Limit      : Gui.Plain_Edit_Box;
  Aligned_Stars        : Gui.Plain_Edit_Box;
  Cone_Error           : Gui.Plain_Edit_Box;
  Pole_Az_Offset       : Gui.Plain_Edit_Box;
  Pole_Height_Offset   : Gui.Plain_Edit_Box;
  Ra_Rotation          : Gui.Plain_Edit_Box;
  Dec_Rotation         : Gui.Plain_Edit_Box;
  System_Error         : Gui.Plain_Edit_Box;

  type Setup_Data_Storage is record
    Image_Orientation : Telescope.Orientation;
    Autoguiding_Rate  : Device.Autoguiding_Rate;
    Air_Pressure      : Refraction.Hectopascal;
    Temperature       : Refraction.Celsius;
    Max_Magnitude     : Catalog.Magnitude;
  end record;

  package Persistent_Setup is new Persistent (Setup_Data_Storage, "Setup");

  The_Setup_Data : Persistent_Setup.Data;

  The_Image_Orientation : Telescope.Orientation   renames The_Setup_Data.Storage.Image_Orientation;
  The_Autoguiding_Rate  : Device.Autoguiding_Rate renames The_Setup_Data.Storage.Autoguiding_Rate;
  The_Air_Pressure      : Refraction.Hectopascal  renames The_Setup_Data.Storage.Air_Pressure;
  The_Temperature       : Refraction.Celsius      renames The_Setup_Data.Storage.Temperature;
  Max_Magnitude         : Catalog.Magnitude       renames The_Setup_Data.Storage.Max_Magnitude;

  type Page is (Is_Control, Is_Display, Is_Setup);

  Is_Expert_Mode : Boolean := False;
  The_Page      : Page := Is_Control;

  type Target_Selection is (No_Target, Park_Position, Target_Object);

  The_Target_Selection  : Target_Selection := No_Target;
  Last_Target_Selection : Target_Selection;

  Autoguiding_Change : Boolean := False;

  Align_On_Picture_Change     : Boolean := False;
  Align_On_Picture_Is_Enabled : Boolean := False;

  Synchronizing_Is_Enabled : Boolean := False;

  The_Status : Telescope.State := Telescope.Disconnected;

  type Operation is (Set_Sky_Line,
                     Align_Global, Align_Local, Align_1_Star, Align_Pole_Axis,
                     Get_Pole_Top, Get_Pole_Left, Get_Pole_Right);

  subtype Get_Pole is Operation range Get_Pole_Top .. Get_Pole_Right;

  The_Operation               : Operation := Align_1_Star;
  Alignment_Adding_Is_Enabled : Boolean := False;

  type Alignment_Star is (No_Star, First_Star, Second_Star, Third_Star);

  The_Alignment_Star : Alignment_Star := First_Star;

  Action_Routine   : Action_Handler;
  The_Last_Action  : Action := Action'pred (Button_Action'first);
  Last_Action_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;


  function Park_Position_Name return String is
  begin
    return "<" & Lexicon.Image_Of (Lexicon.Park_Position) & ">";
  end Park_Position_Name;


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
      if The_Action = Go_To then
        Alignment_Adding_Is_Enabled := True;
      end if;
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


  procedure Show_Error (The_Text : String := Error.Message) is
  begin
    Gui.Beep;
    Gui.Message_Box (The_Text);
  exception
  when others =>
    Log.Error ("Show_Error failed to Show: " & The_Text);
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


  function Next_Operation_Enabled return Boolean is
  begin
    case The_Operation is
    when Align_Global =>
      return True;
    when Align_Local | Align_1_Star | Align_Pole_Axis | Set_Sky_Line | Get_Pole =>
      return False;
    end case;
  end Next_Operation_Enabled;


  Align_Is_Enabled                    : Boolean := False;
  Is_Ready_For_Alignment_Calculations : Boolean := False;

  procedure Disable_Operation_Buttons (Is_Stopped : Boolean := False) is
  begin
    Align_Is_Enabled  := False;
    Is_Ready_For_Alignment_Calculations := False;
    if Is_Expert_Mode then
      case The_Operation is
      when Set_Sky_Line =>
        if Is_Stopped then
          Gui.Enable (First_Setup_Button);
        else
          Gui.Disable (First_Setup_Button);
        end if;
      when Get_Pole =>
        null;
      when Align_Global =>
        Gui.Disable (First_Setup_Button);
        Gui.Disable (Second_Setup_Button);
      when Align_Local =>
        case The_Status is
        when Telescope.Stopped =>
          if Alignment.Has_Correction_Data then
            Is_Ready_For_Alignment_Calculations := True;
            Gui.Set_Text (First_Setup_Button, "Apply");
            Gui.Enable (First_Setup_Button);
          end if;
        when others =>
          Gui.Disable (First_Setup_Button);
        end case;
        if not Alignment.Has_Correction then
          Gui.Disable (Second_Setup_Button);
        end if;
      when Align_1_Star =>
        Gui.Disable (First_Setup_Button);
        Gui.Disable (Second_Setup_Button);
      when Align_Pole_Axis =>
        Gui.Disable (First_Setup_Button);
        if not Alignment.Has_Correction then
          Gui.Disable (Second_Setup_Button);
        end if;
      end case;
    end if;
  end Disable_Operation_Buttons;


  procedure Set_First_Button_Text is
  begin
    Is_Ready_For_Alignment_Calculations := False;
    case The_Operation is
    when Align_1_Star =>
      Gui.Set_Text (First_Setup_Button, "Align");
    when Align_Pole_Axis =>
      case The_Alignment_Star is
      when No_Star =>
        Gui.Set_Text (First_Setup_Button, "Align");
      when First_Star =>
        Gui.Set_Text (First_Setup_Button, "Add First");
      when Second_Star =>
        Gui.Set_Text (First_Setup_Button, "Add Second");
      when Third_Star =>
        Gui.Set_Text (First_Setup_Button, "Add Third");
      end case;
    when Set_Sky_Line | Align_Global | Align_Local =>
      Gui.Set_Text (First_Setup_Button, "Add");
    when Get_Pole =>
      Gui.Set_Text (First_Setup_Button, "Get");
    end case;
  end Set_First_Button_Text;


  procedure Enable_Operation_Buttons is
  begin
    if Is_Expert_Mode then
      case The_Operation is
      when Get_Pole =>
        if Site.Is_Defined then
          Gui.Enable (Second_Setup_Button);
        end if;
      when Set_Sky_Line =>
        Set_First_Button_Text;
        Gui.Disable (First_Setup_Button);
        if Sky_Line.Is_Defined then
          Gui.Enable (Second_Setup_Button);
        end if;
      when Align_1_Star =>
        if Alignment.Has_One_Star_Offsets then
          Gui.Enable (First_Setup_Button);
        end if;
      when Align_Global | Align_Local | Align_Pole_Axis =>
        if Alignment_Adding_Is_Enabled then
          Set_First_Button_Text;
          if The_Alignment_Star = No_Star then
            Align_Is_Enabled := True;
          end if;
        end if;
        Gui.Enable (First_Setup_Button);
        if Next_Operation_Enabled then
          Gui.Enable (Second_Setup_Button);
        elsif Alignment.Has_Correction or The_Alignment_Star /= First_Star then
          Gui.Enable (Second_Setup_Button);
        end if;
      end case;
    end if;
  end Enable_Operation_Buttons;


  The_Actual_Direction : Earth.Direction;

  procedure Show (Information : Telescope.Data) is
    use type Angle.Value;
    use type Device.Time_Synch_State;
    use type Telescope.State;
  begin
    if The_Target_Selection = No_Target then
      Define_Park_Position;
      return;
    end if;
    if (The_Status /= Information.Status)
      or (Last_Target_Selection /= The_Target_Selection)
      or Autoguiding_Change
      or Align_On_Picture_Change
    then
      Autoguiding_Change := False;
      Align_On_Picture_Change := False;
      The_Status := Information.Status;
      Last_Target_Selection := The_Target_Selection;
      Synchronizing_Is_Enabled := False;
      case The_Target_Selection is
      when Park_Position =>
        Align_On_Picture_Is_Enabled := False;
        Gui.Set_Text (First_Control_Button, "Park");
        case The_Status is
        when Telescope.Startup | Telescope.Stopped | Telescope.Ready =>
          Gui.Set_Text (Stop_Or_Synch_Button, "Synch");
          Synchronizing_Is_Enabled := True;
        when Telescope.Parking | Telescope.Positioning | Telescope.Stopping | Telescope.Directing =>
          Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
        when others =>
          null;
        end case;
      when Target_Object =>
        Gui.Set_Text (First_Control_Button, "Goto");
        if Parameter.Synch_On_Targets then
          case The_Status is
          when Telescope.Ready | Telescope.Stopped | Telescope.Parked =>
            Gui.Set_Text (Stop_Or_Synch_Button, "Synch");
            Synchronizing_Is_Enabled := True;
            Align_On_Picture_Is_Enabled := False;
          when Telescope.Tracking | Telescope.Solving =>
            if Align_On_Picture_Is_Enabled then
              Gui.Set_Text (First_Control_Button, "Align");
            end if;
            Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
          when others =>
            Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
            Align_On_Picture_Is_Enabled := False;
          end case;
        else
          Gui.Set_Text (Stop_Or_Synch_Button, "Stop");
        end if;
      when No_Target =>
        raise Program_Error;
      end case;
      case The_Status is
      when Telescope.Startup | Telescope.Disconnected | Telescope.Directing | Telescope.Stopping =>
        Disable_Operation_Buttons;
        Gui.Disable (Stop_Or_Synch_Button);
        Gui.Disable (First_Control_Button);
      when Telescope.Ready | Telescope.Stopped =>
        Disable_Operation_Buttons (Is_Stopped => The_Status = Telescope.Stopped);
        case The_Target_Selection is
        when Park_Position =>
          Gui.Enable (Stop_Or_Synch_Button);
        when Target_Object =>
          if Synchronizing_Is_Enabled then
            Gui.Enable (Stop_Or_Synch_Button);
          else
            Gui.Disable (Stop_Or_Synch_Button);
          end if;
        when others =>
          raise Program_Error;
        end case;
        Gui.Enable (First_Control_Button);
      when Telescope.Parked =>
        Disable_Operation_Buttons;
        case The_Target_Selection is
        when Park_Position =>
          Gui.Disable (First_Control_Button);
          Gui.Disable (Stop_Or_Synch_Button);
        when Target_Object =>
          if Synchronizing_Is_Enabled then
            Gui.Enable (Stop_Or_Synch_Button);
          else
            Gui.Disable (Stop_Or_Synch_Button);
          end if;
          Gui.Enable (First_Control_Button);
        when others =>
          raise Program_Error;
        end case;
      when Telescope.Adjusting =>
        Disable_Operation_Buttons;
        Gui.Disable (First_Control_Button);
      when Telescope.Parking =>
        Disable_Operation_Buttons;
        Gui.Enable (Stop_Or_Synch_Button);
        Gui.Disable (First_Control_Button);
      when Telescope.Waiting =>
        Disable_Operation_Buttons;
        Gui.Disable (Stop_Or_Synch_Button);
        Gui.Enable (First_Control_Button);
      when Telescope.Positioning | Telescope.Approaching | Telescope.Preparing =>
        Disable_Operation_Buttons;
        Gui.Enable (Stop_Or_Synch_Button);
        Gui.Enable (First_Control_Button);
      when Telescope.Tracking | Telescope.Solving=>
        Enable_Operation_Buttons;
        Gui.Enable (Stop_Or_Synch_Button);
        Gui.Enable (First_Control_Button);
      end case;
    end if;
    Gui.Set_Status_Line (Information.Status'img);
    if Earth.Direction_Is_Known (Information.Local_Direction) then
      The_Actual_Direction := Information.Local_Direction;
    end if;
    if Earth.Direction_Is_Known (Information.Adjustment) then
      Alignment.Set (Direction => Information.Local_Direction,
                     Offset    => Information.Adjustment);
    end if;
    case The_Page is
    when Is_Control =>
      null;
    when Is_Display =>
      if Earth.Direction_Is_Known (Information.Adjustment) then
        Gui.Set_Text (Alt_Adjustment, Earth.Alt_Offset_Image_Of (Information.Adjustment));
        Gui.Set_Text (Az_Adjustment, Earth.Az_Offset_Image_Of (Information.Adjustment));
      else
        Gui.Set_Text (Alt_Adjustment, "");
        Gui.Set_Text (Az_Adjustment, "");
      end if;
      if Earth.Direction_Is_Known (Information.Alignment_Offsets) then
        Gui.Set_Text (Alt_Offset, Earth.Alt_Offset_Image_Of (Information.Alignment_Offsets));
        Gui.Set_Text (Az_Offset, Earth.Az_Offset_Image_Of (Information.Alignment_Offsets));
      else
        Gui.Set_Text (Alt_Offset, "");
        Gui.Set_Text (Az_Offset, "");
      end if;
      if Motor.Is_Defined (Information.Motor_Positions.Positions) then
        Gui.Set_Text (Motor_1,
                      Angle.Unsigned_Degrees_Image_Of (Motor.First_Of (Information.Motor_Positions.Positions)));
        Gui.Set_Text (Motor_2,
                      Angle.Unsigned_Degrees_Image_Of (Motor.Second_Of (Information.Motor_Positions.Positions)));
        Gui.Set_Text (Flipped, Information.Motor_Positions.Inverted'image);
      else
        Gui.Set_Text (Motor_1, "");
        Gui.Set_Text (Motor_2, "");
        Gui.Set_Text (Flipped, "");
      end if;
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
      if Motor.Board_Temperature_Is_Known then
        Gui.Set_Text (Board_Temperature, Motor.Board_Temperature'img & "°C");
      else
        Gui.Set_Text (Board_Temperature, "");
      end if;
      if Motor.Synch_State /= Device.Idle then
        Gui.Set_Text (Synch_State, Strings.Legible_Of (Motor.Synch_State'img));
      else
        Gui.Set_Text (Synch_State, "");
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
      if Information.Time_Adjustment = 0.0 then
        Gui.Set_Text (Time_Offset, "");
      else
        Gui.Set_Text (Time_Offset, Information.Time_Adjustment'img & "s");
      end if;
    when Is_Setup =>
      if Site.Is_Defined then
        Gui.Set_Text (Longitude, Angle.Image_Of (Site.Longitude, Decimals => 2));
        Gui.Set_Text (Latitude, Angle.Image_Of (Site.Latitude, Decimals => 2, Show_Signed => True));
        Gui.Set_Text (Elevation, Strings.Trimmed (Site.Elevation'img) & 'm');
      else
        Gui.Set_Text (Longitude, "");
        Gui.Set_Text (Latitude, "");
        Gui.Set_Text (Elevation, "");
      end if;
      case The_Operation is
      when Get_Pole | Set_Sky_Line | Align_Global | Align_1_Star | Align_Pole_Axis =>
        Gui.Set_Text (Aligned_Stars, "");
      when Align_Local =>
        Gui.Set_Text (Aligned_Stars, Strings.Trimmed (Alignment.Number_Of_Aligned_Stars'img));
      end case;
      if Information.Cone_Error = Angle.Zero then
        Gui.Set_Text (Cone_Error, "");
      else
        Gui.Set_Text (Cone_Error, Angle.Image_Of (Information.Cone_Error, Show_Signed => True));
      end if;
      if Earth.Direction_Is_Known (Information.Pole_Offsets) then
        if Earth.Az_Of (Information.Pole_Offsets) = Angle.Zero then
          Gui.Set_Text (Pole_Az_Offset, "");
        else
          Gui.Set_Text (Pole_Az_Offset, Earth.Az_Offset_Image_Of (Information.Pole_Offsets));
        end if;
        if Earth.Alt_Of (Information.Pole_Offsets) = Angle.Zero then
          Gui.Set_Text (Pole_Height_Offset, "");
        else
          Gui.Set_Text (Pole_Height_Offset, Earth.Alt_Offset_Image_Of (Information.Pole_Offsets));
        end if;
      else
        Gui.Set_Text (Pole_Az_Offset, "");
        Gui.Set_Text (Pole_Height_Offset, "");
      end if;
      if Space.Direction_Is_Known (Information.Rotations) then
        Gui.Set_Text (Ra_Rotation, Angle.Image_Of (Space.Ra_Of(Information.Rotations),
                                                   Decimals    => 3,
                                                   Show_Signed => True));
        Gui.Set_Text (Dec_Rotation, Angle.Image_Of (Space.Dec_Of(Information.Rotations),
                                                    Decimals    => 3,
                                                    Show_Signed => True));
        Gui.Set_Text (System_Error, Angle.Image_Of (Information.System_Error,
                                                    Decimals    => 3,
                                                    Show_Signed => True));
      else
        Gui.Set_Text (Ra_Rotation, "");
        Gui.Set_Text (Dec_Rotation, "");
        Gui.Set_Text (System_Error, "");
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


  procedure Perform_Park is
  begin
    Signal_Action (Park);
  end Perform_Park;


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
    if Synchronizing_Is_Enabled then
      Perform_Synch;
    else
      Input.Put (Device.Stop, From => Input.Button);
    end if;
  exception
  when others =>
    Log.Error ("Perform_Stop_Or_Synch");
  end Perform_Stop_Or_Synch;


  procedure Perform_First_Control is
  begin
    case The_Target_Selection is
    when Park_Position =>
      Perform_Park;
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


  Is_Entering_Number : Boolean := False;
  The_Number         : Natural;
  The_Number_Id      : Name.Selector;
  The_Targets        : Name.Id_List_Access;


  function Image_Of (The_Value : Device.Autoguiding_Rate) return String is
  begin
    return Strings.Trimmed (The_Value'img) & "%";
  end Image_Of;


  procedure Define_Autoguiding is
  begin
    declare
      Value : constant String := Strings.Trimmed (Gui.Contents_Of (Autoguiding_Rate));
      Last  : Natural := Value'last;
    begin
      loop
        exit when Value(Last) in '0'..'9';
        Last := Last - 1;
      end loop;
      The_Autoguiding_Rate := Device.Autoguiding_Rate'value(Value(Value'first .. Last));
      Autoguiding_Change := True;
      Motor.Set (The_Autoguiding_Rate);
    exception
    when others =>
      Show_Error ("Incorrect Autoguiding Rate: " & Value);
    end;
    Gui.Set_Text (Autoguiding_Rate, Image_Of (The_Autoguiding_Rate));
  exception
  when others =>
    Log.Error ("Define_Autoguiding");
  end Define_Autoguiding;


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


  procedure Define_Magnitude is
    Value : constant String := Gui.Contents_Of (Magnitude_Limit);
  begin
    Max_Magnitude := Catalog.Magnitude'value(Value);
  exception
  when others =>
    Show_Error ("Incorrect Magnitude: " & Value);
  end Define_Magnitude;


  use type Name.Id;

  package Stars is new Ada.Containers.Doubly_Linked_Lists (Name.Id);

  The_Next_Stars    : Stars.List;
  The_Next_Altitude : Angle.Degrees;
  The_Next_Azimuth  : Angle.Degrees;
  The_Next_Width    : Angle.Degrees;
  Next_Is_First     : Boolean;

  procedure Define_Operation is

    Control_Image : constant String := Identifier_Of (Gui.Contents_Of (Operation_Control));

    procedure Initialize_Next_Operation is
      use type Catalog.Magnitude;
    begin
      Gui.Set_Text (Second_Setup_Button, "Next");
      The_Next_Stars.Clear;
      Next_Is_First := True;
      if Max_Magnitude <= 0.0 then
        The_Next_Width := 45.0;
      elsif Max_Magnitude <= 1.0 then
        The_Next_Width := 30.0;
      elsif Max_Magnitude <= 2.0 then
        The_Next_Width := 20.0;
      elsif Max_Magnitude <= 3.0 then
        The_Next_Width := 15.0;
      else
        The_Next_Width := 10.0;
      end if;
      Log.Write ("Next Width:" & The_Next_Width'img);
      Catalog_Menu.Set (Data.Hip);
      Gui.Disable (Second_Setup_Button);
    end Initialize_Next_Operation;

  begin -- Define_Operation
    Define_Magnitude;
    The_Operation := Operation'value(Control_Image);
    Log.Write ("Operation - " & Control_Image & " - Maximum Magnitude:" & Max_Magnitude'img);
    case The_Operation is
    when Align_Global =>
      Matrix.Clear;
      Initialize_Next_Operation;
    when Align_1_Star =>
      Set_First_Button_Text;
      Gui.Set_Text (Second_Setup_Button, "");
      Gui.Disable (Second_Setup_Button);
    when Align_Pole_Axis =>
      Alignment.Clear_Corrections;
      Matrix.Clear;
      Set_First_Button_Text;
      Gui.Set_Text (Second_Setup_Button, "Clear");
      Gui.Disable (Second_Setup_Button);
    when Align_Local =>
      Matrix.Clear;
      Set_First_Button_Text;
      Gui.Set_Text (Second_Setup_Button, "Clear");
      Gui.Disable (Second_Setup_Button);
    when Set_Sky_Line =>
      Set_First_Button_Text;
      Gui.Set_Text (Second_Setup_Button, "Clear");
      if Sky_Line.Is_Defined then
        Gui.Enable (Second_Setup_Button);
      else
        Gui.Disable (Second_Setup_Button);
      end if;
    when Get_Pole =>
      Alignment.Clear_Corrections;
      Set_First_Button_Text;
      Gui.Set_Text (Second_Setup_Button, "Clear");
      Gui.Enable (First_Setup_Button);
      if Site.Is_Defined then
        Gui.Enable (Second_Setup_Button);
      else
        Gui.Disable (Second_Setup_Button);
      end if;
      return;
    end case;
    Gui.Disable (First_Setup_Button);
  exception
  when others =>
    Log.Error ("Define_Operation");
  end Define_Operation;


  procedure Next_Global_Alignment is
    The_Distance : Angle.Degrees;
    The_Item     : Name.Id;
    use Astro;
  begin
    Gui.Disable (Second_Setup_Button);
    Log.Write ("Next_Azimuth :" & The_Next_Azimuth'img);
    Log.Write ("Next_Altitude:" & The_Next_Altitude'img);
    The_Distance := Angle.Degrees'last;
    for The_Index in The_Targets.Ids'first .. The_Targets.Last loop
      declare
        Item      : constant Name.Id := The_Targets.Ids(The_Index);
        Magnitude : constant Catalog.Magnitude := Catalog.Magnitude(Name.Magnitude_Of (Item));
        use all type Data.Object_Type;
        use type Catalog.Magnitude;
      begin
        if Name.Type_Of (Item) = Star and then not The_Next_Stars.Contains (Item) and then
          (Name.Is_Visible (Item) and (Magnitude < Max_Magnitude))
        then
          declare
            Direction : constant Earth.Direction := Objects.Direction_Of (Name.Direction_Of (Item, Time.Universal),
                                                                          Time.Lmst);
            use type Angle.Value;
            use type Angle.Signed;

            Altitude     : constant Angle.Degrees := +Angle.Signed'(+Earth.Alt_Of (Direction));
            Azimuth      : constant Angle.Degrees := +Earth.Az_Of (Direction);
            Alt_Distance : constant Angle.Degrees := abs (The_Next_Altitude - Altitude);
            Az_Distance  : constant Angle.Degrees := The_Next_Azimuth - Azimuth;
            Max_Distance : constant Angle.Degrees := The_Next_Width / 2.0;
          begin
            case The_Operation is
            when Align_Global =>
              if (Az_Distance < The_Distance) and (Alt_Distance < Max_Distance) then
                The_Distance := Az_Distance;
                The_Item := Item;
              end if;
            when Align_Local | Align_1_Star | Align_Pole_Axis | Set_Sky_Line | Get_Pole =>
              raise Program_Error;
            end case;
          end;
        end if;
      end;
    end loop;
    if The_Distance < Angle.Degrees'last then
      The_Next_Stars.Prepend (The_Item);
      declare
        Next_Star : constant String := Name.Image_Of (The_Item);
      begin
        Set_Target_Name (Next_Star);
        Signal_Action (Define_Target);
        Signal_Action (Go_To);
        return;
      end;
    end if;
  end Next_Global_Alignment;


  procedure Handle_Second_Setup_Button is
  begin
    Define_Magnitude;
    case The_Operation is
    when Align_Global =>
      Next_Global_Alignment;
    when Align_Local | Align_Pole_Axis =>
      case The_Operation is
      when Align_Local =>
        Alignment_Adding_Is_Enabled := True;
      when Align_Pole_Axis =>
        Alignment_Adding_Is_Enabled := False;
        The_Alignment_Star := First_Star;
      when others =>
        raise Program_Error;
      end case;
      Set_First_Button_Text;
      Alignment.Clear_Corrections;
      Gui.Disable (Second_Setup_Button);
    when Set_Sky_Line =>
      Sky_Line.Clear;
      Gui.Disable (Second_Setup_Button);
    when Get_Pole =>
      Site.Clear;
      Pole_Axis.Clear;
      Gui.Disable (Second_Setup_Button);
    when Align_1_Star =>
      raise Program_Error;
    end case;
  exception
  when others =>
    Log.Error ("Handle_Second_Setup_Button");
  end Handle_Second_Setup_Button;


  procedure Handle_First_Setup_Button is

    procedure Initialize_First_Next is
      use type Angle.Value;
    begin
      if Next_Is_First then
        The_Next_Altitude := +Earth.Alt_Of (The_Actual_Direction);
        The_Next_Azimuth := +Earth.Az_Of (The_Actual_Direction);
        Next_Is_First := False;
      end if;
    end Initialize_First_Next;

  begin -- Handle_First_Setup_Button
    case The_Operation is
    when Get_Pole_Top =>
      Pole_Axis.Evaluate_Pole_Top;
      Gui.Enable (Second_Setup_Button);
    when Get_Pole_Left =>
      Pole_Axis.Evaluate_Pole_Left;
      Gui.Enable (Second_Setup_Button);
    when Get_Pole_Right =>
      Pole_Axis.Evaluate_Pole_Right;
      Gui.Enable (Second_Setup_Button);
    when Set_Sky_Line =>
      Sky_Line.Add (The_Actual_Direction);
      Gui.Enable (Second_Setup_Button);
    when Align_1_Star =>
      Signal_Action (Align);
      Gui.Disable (First_Setup_Button);
    when Align_Global | Align_Local | Align_Pole_Axis =>
      if Align_Is_Enabled then
        Gui.Disable (Second_Setup_Button);
        Align_Is_Enabled := False;
        The_Alignment_Star := First_Star;
        Set_First_Button_Text;
        Signal_Action (Align);
      elsif Is_Ready_For_Alignment_Calculations then
        Is_Ready_For_Alignment_Calculations := False;
        Gui.Disable (First_Setup_Button);
        Gui.Set_Text (First_Setup_Button, "busy...");
        Alignment.Apply;
        Gui.Set_Text (First_Setup_Button, "Add");
      elsif Alignment_Adding_Is_Enabled then
        case The_Operation is
        when Align_Global =>
          Initialize_First_Next;
          Alignment.Store;
          Handle_Second_Setup_Button;
        when Align_Local =>
          Alignment.Add;
          Gui.Set_Text (First_Setup_Button, "Add");
          if Alignment.Has_Correction then
            Gui.Enable (Second_Setup_Button);
          end if;
        when Align_Pole_Axis =>
          case The_Alignment_Star is
          when First_Star =>
            Alignment.Add_First;
            The_Alignment_Star := Second_Star;
            Set_First_Button_Text;
          when Second_Star =>
            Alignment.Add_Second;
            The_Alignment_Star := Third_Star;
            Set_First_Button_Text;
          when Third_Star =>
            begin
              Alignment.Add_Third;
              Alignment_Adding_Is_Enabled := False;
              The_Alignment_Star := No_Star;
              Gui.Set_Text (First_Setup_Button, "Adjust");
              Signal_Action (Go_To);
              return;
            exception
            when Alignment.Failure =>
              The_Alignment_Star := First_Star;
              Set_First_Button_Text;
            end;
          when No_Star =>
            null;
          end case;
          Gui.Enable (Second_Setup_Button);
        when Align_1_Star | Set_Sky_Line | Get_Pole =>
          raise Program_Error;
        end case;
      else
        Set_First_Button_Text;
      end if;
      Gui.Disable (First_Setup_Button);
    end case;
  exception
  when Pole_Axis.Picture_Not_Found =>
    Show_Error ("Picture " & Picture.Filename & " not found");
  when Pole_Axis.Picture_Not_Solved =>
    Show_Error ("Picture not solved");
  when others =>
    Log.Error ("Handle_First_Setup_Button");
  end Handle_First_Setup_Button;


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


  procedure Set_Park_Position is
  begin
    Set_Target_Name (Park_Position_Name);
    The_Target_Selection := Park_Position;
  end Set_Park_Position;


  procedure Enter_Number is
  begin
    if Is_Entering_Number then
      if The_Number = 0 then
        Set_Park_Position;
        Signal_Action (Define_Target);
        Signal_Action (Park);
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
    elsif The_Target_Selection = Target_Object and then Gui.Is_Enabled (First_Control_Button) then
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
    if Is_Expert_Mode and then Gui.Is_Enabled (First_Setup_Button) then
      Handle_First_Setup_Button;
    else
      Enter_Number;
    end if;
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
      when Gui.Key_Codes.KP_Decimal =>
        if Is_Expert_Mode and then Gui.Is_Enabled (Second_Setup_Button) then
          Handle_Second_Setup_Button;
        else
          Not_A_Number;
        end if;
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
        return "S0 " & Park_Position_Name;
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
    if Name_Id = null then
      Set_Park_Position;
    else
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
                                      The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                     Gui.Buttons_Fill_Vertically   => False]);

        First_Control_Button := Gui.Create (Control_Page, "Goto", Perform_First_Control'access);
        Gui.Disable (First_Control_Button);
        Stop_Or_Synch_Button := Gui.Create (Control_Page, "Stop", Perform_Stop_Or_Synch'access);
        Gui.Disable (Stop_Or_Synch_Button);
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

        Az_Adjustment := Gui.Create (Display_Page, "Az Adjustment", "",
                                     Is_Modifiable  => False,
                                     The_Size       => Text_Size,
                                     The_Title_Size => Title_Size);
        Alt_Adjustment := Gui.Create (Display_Page, Alt_Adjustment_Text, "",
                                      Is_Modifiable  => False,
                                      The_Size       => Text_Size,
                                      The_Title_Size => Title_Size);

        Az_Offset := Gui.Create (Display_Page, "Az Offset", "",
                                 Is_Modifiable  => False,
                                 The_Size       => Text_Size,
                                 The_Title_Size => Title_Size);
        Alt_Offset := Gui.Create (Display_Page, "Alt Offset", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);

        Motor_1 := Gui.Create (Display_Page, "Motor 1", "",
                               Is_Modifiable  => False,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);
        Motor_2 := Gui.Create (Display_Page, "Motor 2", "",
                               Is_Modifiable  => False,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);
        Flipped := Gui.Create (Display_Page, "Flipped Over", "",
                               Is_Modifiable  => False,
                               The_Size       => Text_Size,
                               The_Title_Size => Title_Size);

        Board_Temperature := Gui.Create (Display_Page, "Board Temp.", "",
                                         Is_Modifiable  => False,
                                         The_Size       => Text_Size,
                                         The_Title_Size => Title_Size);

        Synch_State := Gui.Create (Display_Page, "Synch State", "",
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
        Time_Offset := Gui.Create (Display_Page, "Time Offset", "",
                                   Is_Modifiable  => False,
                                   The_Size       => Text_Size,
                                   The_Title_Size => Title_Size);
      end Define_Display_Page;


      procedure Define_Setup_Page is

        -- largest texts
        Max_Magnitude_Text      : constant String := "Max Magnitude";
        Pole_Height_Offset_Text : constant String := "Pole Alt Offset";

        Title_Size : constant Natural := Natural'max (Gui.Text_Size_Of (Max_Magnitude_Text),
                                                      Gui.Text_Size_Of (Pole_Height_Offset_Text)) + Separation;
        Text_Size : constant Natural := Natural'max (Gui.Text_Size_Of ("+360d00'00.000"""),
                                                     Gui.Text_Size_Of ("Upside Down sb")) + Separation;
      begin
        Setup_Page := Gui.Add_Page (The_Title  => "Setup",
                                    The_Action => Enter_Setup_Page'access,
                                    The_Style  => [Gui.Buttons_Fill_Horizontally => True,
                                                   Gui.Buttons_Fill_Vertically   => False]);

        First_Setup_Button := Gui.Create (Setup_Page, "Align", Handle_First_Setup_Button'access);
        Second_Setup_Button := Gui.Create (Setup_Page, "", Handle_Second_Setup_Button'access);

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

        Orientation_Box := Gui.Create (Setup_Page, Orientation_Key,
                                       The_Action_Routine => Set_Orientation'access,
                                       The_Size           => Text_Size,
                                       The_Title_Size     => Title_Size);
        for Value in Telescope.Orientation'range loop
          Gui.Add_Text (Orientation_Box, Strings.Legible_Of (Value'img));
        end loop;
        Gui.Select_Text (Orientation_Box, Strings.Legible_Of (The_Image_Orientation'img));
        Autoguiding_Rate := Gui.Create (Setup_Page, "Autoguiding", Image_Of (The_Autoguiding_Rate),
                                        The_Action_Routine => Define_Autoguiding'access,
                                        The_Size           => Text_Size,
                                        The_Title_Size     => Title_Size);
        Air_Pressure := Gui.Create (Setup_Page, "Air Pressure", Image_Of (The_Air_Pressure),
                                    The_Action_Routine => Define_Air_Pressure'access,
                                    The_Size           => Text_Size,
                                    The_Title_Size     => Title_Size);
        Temperature := Gui.Create (Setup_Page, "Temperature", Image_Of (The_Temperature),
                                   The_Action_Routine => Define_Temperature'access,
                                   The_Size           => Text_Size,
                                   The_Title_Size     => Title_Size);

        Operation_Control := Gui.Create (Setup_Page, Operation_Control_Key,
                                         The_Action_Routine => Define_Operation'access,
                                         The_Size           => Text_Size,
                                         The_Title_Size     => Title_Size);
        for Value in Operation'range loop
          Gui.Add_Text (Operation_Control, Strings.Legible_Of (Value'img));
        end loop;
        Gui.Select_Text (Operation_Control, Strings.Legible_Of (The_Operation'img));
        Magnitude_Limit := Gui.Create (Setup_Page, Max_Magnitude_Text, Max_Magnitude'img,
                                       The_Action_Routine => Define_Magnitude'access,
                                       The_Size           => Text_Size,
                                       The_Title_Size     => Title_Size);
        Aligned_Stars := Gui.Create (Setup_Page, "Aligned Stars", "",
                                     Is_Modifiable  => False,
                                     The_Size       => Text_Size,
                                     The_Title_Size => Title_Size);
        Cone_Error := Gui.Create (Setup_Page, "Cone Error", "",
                                  Is_Modifiable  => False,
                                  The_Size       => Text_Size,
                                  The_Title_Size => Title_Size);
        Pole_Az_Offset := Gui.Create (Setup_Page, "Pole Az Offset", "",
                                      Is_Modifiable  => False,
                                      The_Size       => Text_Size,
                                      The_Title_Size => Title_Size);
        Pole_Height_Offset := Gui.Create (Setup_Page, Pole_Height_Offset_Text, "",
                                          Is_Modifiable  => False,
                                          The_Size       => Text_Size,
                                          The_Title_Size => Title_Size);
        Ra_Rotation := Gui.Create (Setup_Page, "Ra Rotation", "",
                                   Is_Modifiable  => False,
                                   The_Size       => Text_Size,
                                   The_Title_Size => Title_Size);
        Dec_Rotation := Gui.Create (Setup_Page, "Dec Rotation", "",
                                    Is_Modifiable  => False,
                                    The_Size       => Text_Size,
                                    The_Title_Size => Title_Size);
        System_Error := Gui.Create (Setup_Page, "System Error", "",
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
        The_Image_Orientation := Telescope.Correct;
        The_Autoguiding_Rate := 0;
        The_Air_Pressure := 0;
        The_Temperature := 10;
        Max_Magnitude := 4.0;
      end if;
      Signal_Action (Set_Orientation);
      Motor.Set (The_Autoguiding_Rate);
      Refraction.Set (The_Air_Pressure);
      Refraction.Set (The_Temperature);
      Is_Expert_Mode := Parameter.Expert_Mode;
      if Is_Expert_Mode then
        Define_Display_Page;
        Define_Setup_Page;
      end if;
      Gui.Enable_Key_Handler;
      Gui.Show;
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
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


  procedure Define (New_Targets : Name.Id_List_Access) is
  begin
    The_Targets := New_Targets;
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


  procedure Define_Park_Position is
  begin
    Set_Park_Position;
  end Define_Park_Position;


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

end User;
