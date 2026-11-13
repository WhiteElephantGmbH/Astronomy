-- *********************************************************************************************************************
-- *                           (c) 2024 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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

with Error;
with Section;

package body Moon.Parameter is

  Minimum_Solar_Elevation_Key : constant String := "Minimum Solar Elevation";
  Maximum_Solar_Elevation_Key : constant String := "Maximum Solar Elevation";


  procedure Define (Handle : Configuration.File_Handle) is

    function Solar_Elevation_Of (Key : String) return Units.Solar_Elevation is
      Image : constant String := Section.String_Of (Key);
    begin
      return Units.Solar_Elevation'value(Image(Image'first .. Image'last - Angle.Degree'length));
    exception
    when others =>
       Error.Raise_With ("Incorrect value of " & Key & ": <" & Image & ">");
    end Solar_Elevation_Of;

  begin -- Define
    Section.Set (Configuration.Handle_For (Handle, Id));
    Define (Solar_Elevation_Range => (Minimum => Solar_Elevation_Of (Minimum_Solar_Elevation_Key),
                                      Maximum => Solar_Elevation_Of (Maximum_Solar_Elevation_Key)));
    exception
    when Error.Occurred =>
      raise;
    when others =>
      Error.Raise_With ("Negative range not allowed");
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Minimum_Solar_Elevation_Key & " = -2" & Angle.Degree);
    Put (Maximum_Solar_Elevation_Key & " = 15" & Angle.Degree);
  end Defaults;

end Moon.Parameter;
