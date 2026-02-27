-- *********************************************************************************************************************
-- *                               (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with PWI4;
with Error;
with Section;

package body Focus.Parameter is

  Samples_Key       : constant String := "Samples";
  Start_At_Key      : constant String := "Start At";
  Step_Key          : constant String := "Step";
  Tolerance_Key     : constant String := "Tolerance";
  Grid_Size_Key     : constant String := "Grid Size";
  HF_Threshold_Key  : constant String := "HF Threshold";
  Trigger_Level_Key : constant String := "Trigger_Level";
  Minimum_Delta_Key : constant String := "Minimum Delta";


  function Distance_For (Key : String) return Distance is
    Image : constant String := Section.String_Of (Key, Id);
  begin
    return Distance(PWI4.Microns'value(Image) / PWI4.Microns_Delta);
  exception
  when others =>
    Error.Raise_With ("Incorrect Position: " & Image);
  end Distance_For;


  function Step_For (Key : String) return Step is
    Image : constant String := Section.String_Of (Key, Id);
  begin
    return Step(PWI4.Microns'value(Image) / PWI4.Microns_Delta);
  exception
  when others =>
    Error.Raise_With ("Incorrect Step: " & Image);
  end Step_For;


  function Count_For (Key : String) return HFD_Sample_Count is
    Image : constant String := Section.String_Of (Key, Id);
  begin
    return HFD_Sample_Count'value(Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Samples (range" & HFD_Sample_Count'first'image & " .." & HFD_Sample_Count'last'image
                                       & "): " & Image);
  end Count_For;


  function Size_For (Key : String) return Camera.Square_Size is
    Minimum : constant Integer := 1000;
    Maximum : constant Integer := Integer(Camera.Min_With_Or_Height);
  begin
    return Camera.Square_Size(Section.Value_Of (Key, Id, Minimum, Maximum));
  exception
  when Error.Occurred =>
    raise;
  when others =>
    Error.Raise_With ("Focus Grid Size must be even");
  end Size_For;


  function Pixel_For (Key : String) return Camera.Pixel is
    Minimum : constant Integer := 0;
    Maximum : constant Integer := 500;
  begin
    return Camera.Pixel(Section.Value_Of (Key, Id, Minimum, Maximum));
  end Pixel_For;


  function Diameter_For (Key : String) return Diameter is
    Minimum : constant Integer := 10;
    Maximum : constant Integer := Integer(The_Grid_Size) / 2;
  begin
    return Diameter(Section.Value_Of (Key, Id, Minimum, Maximum));
  end Diameter_For;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    if Section.Exists then
      The_HFD_Samples := Count_For (Samples_Key);
      Log.Write (Samples_Key & ":" & The_HFD_Samples'image);
      The_Start_Position := Distance_For (Start_At_Key);
      Log.Write (Start_At_Key & ":" & The_Start_Position 'image);
      The_Position_Step := Step_For (Step_Key);
      Log.Write (Step_Key & ":" & The_Position_Step'image);
      if The_Start_Position < Minimum_Start_Position then
        Error.Raise_With ("Focusing start to low (<" & Minimum_Start_Position'image & "):" & The_Start_Position'image);
      end if;
      The_Tolerance := Distance_For (Tolerance_Key);
      Log.Write (Tolerance_Key & ":" & The_Tolerance'image);
      The_Grid_Size := Size_For (Grid_Size_Key);
      Log.Write (Grid_Size_Key & ":" & The_Grid_Size'image);
      The_HF_Threshold := Pixel_For (HF_Threshold_Key);
      Log.Write (HF_Threshold_Key & ":" & The_HF_Threshold'image);
      The_Trigger_Level := Diameter_For (Trigger_Level_Key);
      Log.Write (Trigger_Level_Key & ":" & The_Trigger_Level'image);
      The_Minimum_Delta := Diameter_For (Minimum_Delta_Key);
      Log.Write (Minimum_Delta_Key & ":" & The_Minimum_Delta'image);
    end if;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Samples_Key & "       =" & HFD_Sample_Count'last'image);
    Put (Start_At_Key & "      = 6000.0");
    Put (Step_Key & "          = 25.0");
    Put (Tolerance_Key & "     = 0.5");
    Put (Grid_Size_Key & "     = 1000");
    Put (HF_Threshold_Key & "  = 100");
    Put (Trigger_Level_Key & " = 80");
    Put (Minimum_Delta_Key & " = 25");
  end Defaults;

end Focus.Parameter;
