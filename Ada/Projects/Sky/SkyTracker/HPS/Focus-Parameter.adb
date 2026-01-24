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

  Start_At_Key  : constant String := "Start At";
  Increment_Key : constant String := "Increment";
  Tolerance_Key : constant String := "Tolerance";
  Grid_Size_Key : constant String := "Grid Size";


  function Distance_For (Key : String) return Distance is
    Image : constant String := Section.String_Value_Of (Key);
  begin
    return Distance'value(Image);
  exception
  when others =>
    Error.Raise_With ("Incorrect Position: " & Image);
  end Distance_For;


  function Size_For (Key : String) return Camera.Square_Size is
    Image : constant String := Section.String_Value_Of (Key);
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
      declare
        Start_At  : constant Distance := Distance_For (Start_At_Key);
        Increment : constant Distance := Distance_For (Increment_Key);
        Tolerance : constant Distance := Distance_For (Tolerance_Key);
        Grid_Size : constant Camera.Square_Size := Size_For (Grid_Size_Key);
      begin
        Log.Write ("Start At :" & Start_At 'image);
        Log.Write ("Increment:" & Increment'image);
        Log.Write ("Tolerance:" & Tolerance'image);
        Log.Write ("Grid Size:" & Grid_Size'image);
        Focus_Data.Set (First_Position  => Start_At,
                        First_Increment => Increment,
                        Tolerance       => Tolerance,
                        Square_Size     => Grid_Size);
      end;
    end if;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Start_At_Key & "  = 18000");
    Put (Increment_Key & " = 200");
    Put (Tolerance_Key & " = 0");
    Put (Grid_Size_Key & " = 1000");
  end Defaults;

end Focus.Parameter;
