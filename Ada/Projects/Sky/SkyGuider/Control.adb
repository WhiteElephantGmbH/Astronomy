-- *********************************************************************************************************************
-- *                               (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Command_Line;
with Ada.Text_IO;
with Angle;
with Earth;
with Exceptions;
with Picture;
with Space;
with Strings;
with Time;

package body Control is

  procedure Put (Item : String) renames Ada.Text_IO.Put_Line;

  function Image_Of (Item : Angle.Value) return String is
  begin
    return Strings.Ansi_Of (Angle.Image_Of (The_Value   => Item,
                                            Unit        => Angle.In_Degrees,
                                            Decimals    => 3,
                                            Show_Signed => True));
  end Image_Of;


  procedure Start is
    The_Direction : Space.Direction;
    The_Position  : Earth.Direction;
  begin
    case Ada.Command_Line.Argument_Count is
    when 0 =>
      Put ("Picture filename expected");
    when 1 =>
      Picture.Read (Filename    => Ada.Command_Line.Argument(1),
                    Height      => 2.97,
                    Width       => 4.46,
                    Search_From => Space.North_Pole);
      The_Direction := Picture.Direction;
      Put ("RA  J2000 : " & Space.Ra_Image_Of (The_Direction));
      Put ("DEC J2000 : " & Space.Dec_Image_Of (The_Direction));
      Put ("Elevation :" & Picture.Elevation'image & 'm');
      Put ("Latitude  : " & Image_Of ( Picture.Latitude));
      Put ("Longitude : " & Image_Of (Picture.Longitude));
      Put ("Time      : " & Time.Image_Of (Picture.Time_Stamp));
      The_Direction := Picture.Actual_Direction;
      Put ("RA        : " & Space.Ra_Image_Of (The_Direction));
      Put ("DEC       : " & Space.Dec_Image_Of (The_Direction));
      The_Position := Picture.Direction;
      Put ("ALT       : " & Earth.Alt_Image_Of (The_Position));
      Put ("AZ        : " & Earth.Az_Image_Of (The_Position));
    when others =>
      Put ("Too many arguments");
    end case;
  exception
  when Picture.File_Not_Found =>
    Put ("Picture file not found");
  when Picture.Invalid_File =>
    Put ("Invalid picture file");
  when Picture.Not_Solved =>
    Put ("Picture not locatable");
  when Picture.Undefined_Value =>
    Put ("Undefined picture information");
  when Item: others =>
    Put (Exceptions.Information_Of(Item));
  end Start;

end Control;
