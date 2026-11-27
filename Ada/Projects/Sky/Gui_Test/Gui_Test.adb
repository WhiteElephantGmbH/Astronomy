-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

pragma Build (Description => "GUI Test",
              Version  => (0, 0, 0, 0),
              Kind     => Windows,
              Icon     => False,
              Compiler => "GNAT\14.2");

with Ada.Streams;
with Application;
with Gui;
with Rt;
with Traces;

procedure Gui_Test is

  package Log is new Traces ("Gui_Test");

  Page                                                   : Gui.Page;
  Speed, Handbox, Direction_Buttons, Top, Middle, Bottom : Gui.Group;
  Slower, Faster                                         : Gui.Button with Unreferenced;
  Speed_Value                                            : Gui.Value_Box;
  Speed_Index                                            : Natural := 1;
  Speed_Button_Size                                      : constant := 44;
  Up, Left, Center, Right, Down                          : Gui.Button with Unreferenced;

  Slider_Page    : Gui.Page;
  Sliders        : Gui.Group;
  Slider_Group   : Gui.Group;
  Slider_Row     : Gui.Group;
  Button_Row     : Gui.Group;
  Slider         : Gui.Track_Bar;
  Slider_Value   : Gui.Value_Box;
  Reset_Button   : Gui.Button with Unreferenced;
  Toggle_Button  : Gui.Button with Unreferenced;

  type Tone is (Shadows, Midtones, Highlights);

  Vertical_Group  : Gui.Group;
  Tone_Row        : Gui.Group;
  Tone_Buttons    : Gui.Group;
  Tone_Sliders    : array (Tone) of Gui.Track_Bar;
  Tone_Values     : array (Tone) of Gui.Value_Box;
  Initial_Tones   : constant array (Tone) of Natural := [10, 50, 90];
  Vertical_Reset  : Gui.Button with Unreferenced;
  Vertical_Toggle : Gui.Button with Unreferenced;


  Exposure_Box, Sensitivity_Box : Gui.Plain_Combo_Box;
  Alternate_Exposures           : Boolean := False;


  procedure Selection_Changed is
  begin
    Gui.Set_Status_Line ("EXPOSURE: " & Gui.Contents_Of (Exposure_Box) &
                         "  ISO: " & Gui.Contents_Of (Sensitivity_Box));
  end Selection_Changed;


  procedure Reset_Selections is
  begin
    Gui.Select_Text (Exposure_Box, (if Alternate_Exposures then "5 s" else "30 s"));
    Gui.Select_Text (Sensitivity_Box, "6400");
    Selection_Changed;
  end Reset_Selections;


  procedure Populate_Exposures is
  begin
    Gui.Clear_Contents_Of (Exposure_Box);
    if Alternate_Exposures then
      Gui.Add_Text (Exposure_Box, "1/100 s");
      Gui.Add_Text (Exposure_Box, "1/15 s");
      Gui.Add_Text (Exposure_Box, "1 s");
      Gui.Add_Text (Exposure_Box, "5 s");
    else
      Gui.Add_Text (Exposure_Box, "5 s");
      Gui.Add_Text (Exposure_Box, "10 s");
      Gui.Add_Text (Exposure_Box, "30 s");
      Gui.Add_Text (Exposure_Box, "60 s");
      Gui.Add_Text (Exposure_Box, "120 s");
    end if;
  end Populate_Exposures;


  procedure Change_Exposures is
  begin
    Alternate_Exposures := not Alternate_Exposures;
    Populate_Exposures;
    Reset_Selections;
  end Change_Exposures;


  procedure Toggle_Selections is
  begin
    if Gui.Is_Enabled (Exposure_Box) then
      Gui.Disable (Exposure_Box);
      Gui.Disable (Sensitivity_Box);
    else
      Gui.Enable (Exposure_Box);
      Gui.Enable (Sensitivity_Box);
    end if;
  end Toggle_Selections;


  procedure Test_Selections is
    Selection_Page  : constant Gui.Page := Gui.Add_Page ("Selections");
    Settings        : constant Gui.Group := Gui.Create (Selection_Page, Gui.Vertical);
    Camera_Group    : constant Gui.Group := Gui.Create (Settings, Gui.Vertical,
                                                       Expand => Gui.Fixed, Title => "Camera Settings");
    Exposure_Row    : constant Gui.Group := Gui.Create (Camera_Group, Gui.Horizontal, Equal_Cells => True);
    Sensitivity_Row : Gui.Group;
    Buttons         : Gui.Group;
    Reset, Toggle, Change : Gui.Button with Unreferenced;
  begin
    Gui.Create_Label (Exposure_Row, "Exposure");
    Exposure_Box := Gui.Create (Exposure_Row, Selection_Changed'unrestricted_access,
                                The_Style => [others => False]);
    Sensitivity_Row := Gui.Create (Camera_Group, Gui.Horizontal, Equal_Cells => True);
    Gui.Create_Label (Sensitivity_Row, "Sensitivity");
    Sensitivity_Box := Gui.Create (Sensitivity_Row, Selection_Changed'unrestricted_access,
                                   The_Style => [others => False]);
    Populate_Exposures;
    Gui.Add_Text (Sensitivity_Box, "100");
    Gui.Add_Text (Sensitivity_Box, "400");
    Gui.Add_Text (Sensitivity_Box, "800");
    Gui.Add_Text (Sensitivity_Box, "1600");
    Gui.Add_Text (Sensitivity_Box, "3200");
    Gui.Add_Text (Sensitivity_Box, "6400");
    Reset_Selections;
    Buttons := Gui.Create (Camera_Group, Gui.Vertical, Expand => Gui.Fixed);
    Reset := Gui.Create (Buttons, "Reset", Reset_Selections'unrestricted_access);
    Toggle := Gui.Create (Buttons, "Toggle", Toggle_Selections'unrestricted_access);
    Change := Gui.Create (Buttons, "Change exposures", Change_Exposures'unrestricted_access);
  end Test_Selections;


  procedure Slider_Changed is
  begin
    Gui.Set_Text (Slider_Value, Natural'image (Gui.Get_Position (Slider)));
  end Slider_Changed;


  procedure Reset_Slider is
  begin
    Gui.Set_Position (Slider, 50);
    Slider_Changed;
  end Reset_Slider;


  procedure Toggle_Slider is
  begin
    if Gui.Is_Enabled (Slider) then
      Gui.Disable (Slider);
    else
      Gui.Enable (Slider);
    end if;
  end Toggle_Slider;


  procedure Tone_Changed (Item : Tone) is
  begin
    Gui.Set_Text (Tone_Values (Item), Natural'image (Gui.Get_Position (Tone_Sliders (Item))));
  end Tone_Changed;


  procedure Shadows_Changed is
  begin
    Tone_Changed (Shadows);
  end Shadows_Changed;


  procedure Midtones_Changed is
  begin
    Tone_Changed (Midtones);
  end Midtones_Changed;


  procedure Highlights_Changed is
  begin
    Tone_Changed (Highlights);
  end Highlights_Changed;


  procedure Reset_Vertical is
  begin
    for Item in Tone loop
      Gui.Set_Position (Tone_Sliders (Item), Initial_Tones (Item));
      Tone_Changed (Item);
    end loop;
  end Reset_Vertical;


  procedure Toggle_Vertical is
    Enabled : constant Boolean := Gui.Is_Enabled (Tone_Sliders (Shadows));
  begin
    for Item in Tone loop
      if Enabled then
        Gui.Disable (Tone_Sliders (Item));
      else
        Gui.Enable (Tone_Sliders (Item));
      end if;
    end loop;
  end Toggle_Vertical;


  procedure Return_To_Target (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("return to target");
    when Gui.Released =>
      null;
    end case;
  end Return_To_Target;


  procedure Move_Up (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move up");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving up");
    end case;
  end Move_Up;


  procedure Move_Down (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move down");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving down");
    end case;
  end Move_Down;


  procedure Move_Left (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move left");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving left");
    end case;
  end Move_Left;


  procedure Move_Right (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move right");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving right");
    end case;
  end Move_Right;


  procedure Show_Speed is
  begin
    case Speed_Index is
    when 0 => Gui.Set_Text (Speed_Value, "7.50""/s");
    when 1 => Gui.Set_Text (Speed_Value, "14.99""/s");
    when others => Gui.Set_Text (Speed_Value, "29.98""/s");
    end case;
  end Show_Speed;


  procedure Decrease_Speed is
  begin
    if Speed_Index > 0 then
      Speed_Index := Speed_Index - 1;
      Show_Speed;
    end if;
  end Decrease_Speed;


  procedure Increase_Speed is
  begin
    if Speed_Index < 2 then
      Speed_Index := Speed_Index + 1;
      Show_Speed;
    end if;
  end Increase_Speed;


  Targets : Gui.List_View;

  type Target is (M11, M13, M27, M31, M57, M1, M3, M5, M8, M16, M17, M20, M33, M42, M45,
                  M51, M63, M64, M81, M82, M92, M101, M104, M110);

  Present : array (Target) of Boolean := [others => False];


  function Target_Text (For_Column       : Natural;
                        With_Information : Gui.Information) return String is
    Item : constant Target := Target'val (With_Information);
  begin
    case For_Column is
    when 0 => return Target'image (Item);
    when 1 =>
      case Item is
      when M11 | M45 => return "Open cluster";
      when M13 | M3 | M5 | M92 => return "Globular cluster";
      when M27 | M57 => return "Planetary nebula";
      when M31 | M33 | M51 | M63 | M64 | M81 | M82 | M101 | M104 | M110 => return "Galaxy";
      when M1 => return "Supernova remnant";
      when M8 | M16 | M17 | M20 | M42 => return "Emission nebula";
      end case;
    when others => raise Program_Error;
    end case;
  end Target_Text;


  function Compare_Targets (For_Column         : Natural;
                            First_Information  : Gui.Information;
                            Second_Information : Gui.Information) return Gui.Relation is
    First  : constant String := Target_Text (For_Column, First_Information);
    Second : constant String := Target_Text (For_Column, Second_Information);
  begin
    if First < Second then
      return Gui.Less_Than;
    elsif First > Second then
      return Gui.Greater_Than;
    else
      return Gui.Equal_To;
    end if;
  end Compare_Targets;


  procedure Target_Selected (Item : Gui.Information) is
  begin
    Gui.Set_Status_Line ("TARGET: " & Target_Text (0, Item));
  end Target_Selected;


  procedure Populate_Targets is
  begin
    Gui.Remove_All_From (Targets);
    Present := [others => False];
    for Item in M11 .. M57 loop
      Gui.Add_Data (Targets, Target'pos (Item));
      Present(Item) := True;
    end loop;
  end Populate_Targets;


  procedure Clear_Targets is
  begin
    Gui.Remove_All_From (Targets);
    Present := [others => False];
    Gui.Set_Status_Line ("TARGETS CLEARED");
  end Clear_Targets;


  procedure Add_Target is
  begin
    for Item in Target loop
      if not Present(Item) then
        Gui.Add_Data (Targets, Target'pos (Item));
        Present(Item) := True;
        Gui.Set_Status_Line ("ADDED: " & Target'image (Item));
        return;
      end if;
    end loop;
    Gui.Set_Status_Line ("ALL TARGETS PRESENT");
  end Add_Target;


  procedure Remove_Target is
    Selection : constant Gui.Information_Array := Gui.Selected_Information_From (Targets);
  begin
    for Item of Selection loop
      Gui.Remove_Data (Targets, Item);
      Present(Target'val (Item)) := False;
    end loop;
    if Selection'length = 0 then
      Gui.Set_Status_Line ("NO TARGET SELECTED");
    else
      Gui.Set_Status_Line ("TARGETS REMOVED");
    end if;
  end Remove_Target;


  procedure Test_Targets is
    Target_Page : constant Gui.Page := Gui.Add_Page ("Targets");
    Content     : constant Gui.Group := Gui.Create (Target_Page, Gui.Vertical);
    Buttons     : Gui.Group;
    Name, Kind  : Gui.Column with Unreferenced;
    Reset, Clear, Add, Remove : Gui.Button with Unreferenced;
  begin
    Targets := Gui.Create (Content,
                           Target_Text'unrestricted_access,
                           The_Sort_Routine  => Compare_Targets'unrestricted_access,
                           The_Click_Routine => Target_Selected'unrestricted_access,
                           The_Click_Kind    => Gui.Single_Click);
    Name := Gui.Add_Column (Targets, "Object");
    Kind := Gui.Add_Column (Targets, "Type");
    Gui.Add_Gridlines (Targets);
    Populate_Targets;
    Buttons := Gui.Create (Content, Gui.Horizontal, Expand => Gui.Fixed);
    Reset := Gui.Create (Buttons, "Reset", Populate_Targets'unrestricted_access);
    Clear := Gui.Create (Buttons, "Clear", Clear_Targets'unrestricted_access);
    Buttons := Gui.Create (Content, Gui.Horizontal, Expand => Gui.Fixed);
    Add := Gui.Create (Buttons, "Add", Add_Target'unrestricted_access);
    Remove := Gui.Create (Buttons, "Remove", Remove_Target'unrestricted_access);
  end Test_Targets;


  Processing_Bar, Exposure_Bar : Gui.Progress_Bar;

  task Progress_Test is
    entry Start;
    entry Stop;
    entry Reset;
    entry Shutdown;
  end Progress_Test;


  task body Progress_Test is
    Running    : Boolean := False;
    Processing : Natural := 0;
    Exposure   : Natural := 0;
  begin
    loop
      select
        accept Start do
          Running := Processing < 100 or else Exposure < 30;
        end Start;
      or
        accept Stop do
          Running := False;
        end Stop;
      or
        accept Reset do
          Running := False;
          Processing := 0;
          Exposure := 0;
          Gui.Report_Progress (Processing_Bar, 0);
          Gui.Report_Progress (Exposure_Bar, 0);
        end Reset;
      or
        accept Shutdown;
        exit;
      or
        when Running => delay until Rt.In_Future (1.0);
          if Processing < 100 then
            Processing := Processing + 5;
            Gui.Report_Progress (Processing_Bar, Processing);
          end if;
          if Exposure < 30 then
            Exposure := Exposure + 1;
            Gui.Increment_Progress (Exposure_Bar);
          end if;
          Running := Processing < 100 or else Exposure < 30;
      end select;
    end loop;
  exception
  when Occurrence : others =>
    Log.Termination (Occurrence);
  end Progress_Test;


  procedure Start_Progress is
  begin
    Progress_Test.Start;
  end Start_Progress;


  procedure Stop_Progress is
  begin
    Progress_Test.Stop;
  end Stop_Progress;


  procedure Reset_Progress is
  begin
    Progress_Test.Reset;
  end Reset_Progress;


  procedure Termination is
  begin
    Progress_Test.Shutdown;
  end Termination;


  procedure Test_Progress is
    Progress_Page : constant Gui.Page := Gui.Add_Page ("Progress");
    Content       : constant Gui.Group := Gui.Create (Progress_Page, Gui.Vertical);
    Bars          : constant Gui.Group := Gui.Create (Content, Gui.Vertical, Expand => Gui.Fixed);
    Buttons       : Gui.Group;
    Start, Stop, Reset : Gui.Button with Unreferenced;
  begin
    Gui.Create_Label (Bars, "Processing");
    Processing_Bar := Gui.Create (Bars);
    Gui.Define_Range (Processing_Bar, 100, 5);
    Gui.Set_Color (Processing_Bar, Gui.Green);
    Gui.Create_Label (Bars, "Exposure");
    Exposure_Bar := Gui.Create (Bars);
    Gui.Define_Range (Exposure_Bar, 30);
    Gui.Set_Color (Exposure_Bar, Gui.Blue);
    Buttons := Gui.Create (Content, Gui.Horizontal, Expand => Gui.Fixed);
    Start := Gui.Create (Buttons, "Start", Start_Progress'unrestricted_access);
    Stop := Gui.Create (Buttons, "Stop", Stop_Progress'unrestricted_access);
    Reset := Gui.Create (Content, "Reset", Reset_Progress'unrestricted_access);
  end Test_Progress;


  procedure Test_Detached_Window is
    -- Embedded 4 x 4 bitmap with four coloured quadrants.
    Picture : constant Ada.Streams.Stream_Element_Array :=
      [
       66, 77, 102, 0, 0, 0, 0, 0, 0, 0, 54, 0, 0, 0, 40, 0,
       0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 1, 0, 24, 0, 0, 0,
       0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 255, 0, 255, 0, 0,
       255, 0, 0, 0, 255, 0, 0, 255, 0, 255, 0, 0, 255, 0, 255, 0,
       0, 255, 0, 0, 255, 255, 255, 255, 255, 255, 255, 0, 0, 255, 0, 0,
       255, 255, 255, 255, 255, 255];
    Window  : constant Gui.Detached_Window := Gui.Create ("Detached Test", (350, 50, 400, 300));
  begin
    Gui.Show (Window, Picture);
  end Test_Detached_Window;


  procedure Startup is
    Column   : Gui.Group;
    Handlers : constant array (Tone) of Gui.Action_Routine :=
      [Shadows_Changed'unrestricted_access, Midtones_Changed'unrestricted_access,
       Highlights_Changed'unrestricted_access];
  begin
    Page := Gui.Add_Page ("Handbox");
    Handbox := Gui.Create (Page, Gui.Vertical,
                           Expand => Gui.Fixed,
                           Title  => "Handbox");
    Speed := Gui.Create (Handbox, Gui.Horizontal, Expand => Gui.Fixed, Title => "Speed");
    Slower := Gui.Create (Speed, Gui.Arrow_Left, Decrease_Speed'unrestricted_access,
                          The_Button_Size => Speed_Button_Size);
    Speed_Value := Gui.Create_Value (Speed, "14.99""/s", Minimum_Width => 90);
    Faster := Gui.Create (Speed, Gui.Arrow_Right, Increase_Speed'unrestricted_access,
                          The_Button_Size => Speed_Button_Size);
    Direction_Buttons := Gui.Create (Handbox, Gui.Vertical,
                                     Equal_Cells => True,
                                     Ratio       => Gui.Square,
                                     Frame       => True);
    Top := Gui.Create (Direction_Buttons, Gui.Horizontal, Equal_Cells => True);
    Gui.Space (Top);
    Up := Gui.Create (Top, Gui.Arrow_Up,
                      Move_Up'unrestricted_access,
                      Expand => True);
    Gui.Space (Top);
    Middle := Gui.Create (Direction_Buttons, Gui.Horizontal, Equal_Cells => True);
    Left := Gui.Create (Middle, Gui.Arrow_Left,
                        Move_Left'unrestricted_access,
                        Expand => True);
    Center := Gui.Create (Middle, Gui.Cross_Symbol, Return_To_Target'unrestricted_access, Expand => True);
    Right := Gui.Create (Middle, Gui.Arrow_Right,
                         Move_Right'unrestricted_access,
                         Expand => True);
    Bottom := Gui.Create (Direction_Buttons, Gui.Horizontal, Equal_Cells => True);
    Gui.Space (Bottom);
    Down := Gui.Create (Bottom, Gui.Arrow_Down,
                        Move_Down'unrestricted_access,
                        Expand => True);
    Gui.Space (Bottom);
    Slider_Page := Gui.Add_Page ("Sliders");
    Sliders := Gui.Create (Slider_Page, Gui.Vertical);
    Slider_Group := Gui.Create (Sliders, Gui.Vertical, Expand => Gui.Fixed, Title => "Horizontal slider");
    Slider_Row := Gui.Create (Slider_Group, Gui.Horizontal, Expand => Gui.Fixed);
    Slider := Gui.Create (Slider_Row, Slider_Changed'unrestricted_access);
    Slider_Value := Gui.Create_Value (Slider_Row, "50", Minimum_Width => 50);
    Gui.Define_Range (Slider, 100);
    Gui.Define_Limits (Slider, 10, 90);
    Gui.Define_Tic_Frequency (Slider, 10);
    Gui.Set_Position (Slider, 50);
    Button_Row := Gui.Create (Slider_Group, Gui.Vertical, Expand => Gui.Fixed);
    Reset_Button := Gui.Create (Button_Row, "Reset", Reset_Slider'unrestricted_access);
    Toggle_Button := Gui.Create (Button_Row, "Toggle", Toggle_Slider'unrestricted_access);
    Vertical_Group := Gui.Create (Sliders, Gui.Vertical, Title => "Tones");
    Tone_Row := Gui.Create (Vertical_Group, Gui.Horizontal, Equal_Cells => True);
    for Item in Tone loop
      Column := Gui.Create (Tone_Row, Gui.Vertical);
      Gui.Create_Label (Column, (case Item is
                                when Shadows    => "Shadows",
                                when Midtones   => "Midtones",
                                when Highlights => "Highlights"));
      Tone_Sliders (Item) := Gui.Create (Column, Gui.Vertical, Handlers (Item));
      Tone_Values (Item) := Gui.Create_Value (Column, "", Minimum_Width => 50, Expand => False);
      Gui.Define_Range (Tone_Sliders (Item), 100);
      Gui.Define_Limits (Tone_Sliders (Item), 10, 90);
      Gui.Define_Tic_Frequency (Tone_Sliders (Item), 10);
    end loop;
    Reset_Vertical;
    Tone_Buttons := Gui.Create (Vertical_Group, Gui.Vertical, Expand => Gui.Fixed);
    Vertical_Reset := Gui.Create (Tone_Buttons, "Reset", Reset_Vertical'unrestricted_access);
    Vertical_Toggle := Gui.Create (Tone_Buttons, "Toggle", Toggle_Vertical'unrestricted_access);
    Test_Selections;
    Test_Targets;
    Test_Progress;
    Test_Detached_Window;
    Gui.Set_Status_Line ("READY");
  end Startup;

begin
  Gui.Execute (Application_Name    => Application.Title,
               Startup_Routine     => Startup'access,
               Termination_Routine => Termination'access,
               Initial_Metrics     => (0, 0, 1, 350));
end Gui_Test;
