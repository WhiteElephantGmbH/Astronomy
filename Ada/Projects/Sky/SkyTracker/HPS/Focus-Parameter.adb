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

with Error;
with Section;

package body Focus.Parameter is

  Samples_Key   : constant String := "Samples";
  Start_At_Key  : constant String := "Start At";
  Step_Key      : constant String := "Step";
  Tolerance_Key : constant String := "Tolerance";
  Grid_Size_Key : constant String := "Grid Size";


  function Distance_For (Key : String) return Distance is
    Image : constant String := Section.String_Of (Key, Id);
  begin
    return Distance'value(Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Position: " & Image);
  end Distance_For;


  function Step_For (Key : String) return Step is
    Image : constant String := Section.String_Of (Key, Id);
  begin
    return Step'value(Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Step : " & Image);
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
    Image : constant String := Section.String_Of (Key, Id);
  begin
    return Camera.Square_Size'value (Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Grid Size: " & Image);
  end Size_For;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    if Section.Exists then
      The_HFD_Samples := Count_For (Samples_Key);
      Log.Write (Samples_Key & " :" & The_HFD_Samples'image);
      The_Start_Position := Distance_For (Start_At_Key);
      Log.Write (Start_At_Key & " :" & The_Start_Position 'image);
      The_Position_Step := Step_For (Step_Key);
      Log.Write (Step_Key & " :" & The_Position_Step'image);
      if The_Start_Position < Minimum_Start_Position then
        Error.Raise_With ("Focusing start to low (<" & Minimum_Start_Position'image & "):" & The_Start_Position'image);
      end if;
      The_Tolerance := Distance_For (Tolerance_Key);
      Log.Write (Tolerance_Key & " :" & The_Tolerance'image);
      The_Grid_Size := Size_For (Grid_Size_Key);
      Log.Write (Grid_Size_Key & " :" & The_Grid_Size'image);
    end if;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Samples_Key & "   =" & HFD_Sample_Count'first'image);
    Put (Start_At_Key & "  = 18000");
    Put (Step_Key & "      = 200");
    Put (Tolerance_Key & " = 0");
    Put (Grid_Size_Key & " = 1000");
  end Defaults;

end Focus.Parameter;
