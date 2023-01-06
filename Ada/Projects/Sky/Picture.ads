-- *********************************************************************************************************************
-- *                           (c) 2021 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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
with Astap;
with Earth;
with Exif;
with Space;
with Time;

package Picture is

  File_Not_Found  : exception renames Exif.File_Not_Found;
  Invalid_File    : exception renames Exif.Invalid_File;
  Not_Solved      : exception renames Astap.Not_Solved;
  Undefined_Value : exception;

  Maximum_Heigth : constant Angle.Degrees := 5.0;
  Maximum_Width  : constant Angle.Degrees := 5.0;

  procedure Define (Name   : String;
                    Height : Angle.Degrees;
                    Width  : Angle.Degrees);

  function Filename return String;

  function Exists return Boolean;

  procedure Set_Site;

  function Solve (Search_From : Space.Direction) return Boolean;

  function Solved return Boolean;

  procedure Stop_Solving;

  function Time_Stamp return Time.Ut;
  -- returns the time when the picture is taken.

  function Direction return Space.Direction;
  -- returns the center of the picture in J2000 coordinates.

  procedure Evaluate (Center : out Space.Direction;
                      Lmst   : out Time.Value);
  -- PRECONDITION: The site must be defined (persistent type in package Site) or the camera must provide the location.
  -- evaluate the center of the picture in actual coordinates and the sideral time when the picture was taken.

  function Direction return Earth.Direction;
  -- PRECONDITION: The site must be defined (persistent type in package Site) or the camera must provide the location.
  -- returns the center of the picture in alt/az coordinates.

end Picture;
