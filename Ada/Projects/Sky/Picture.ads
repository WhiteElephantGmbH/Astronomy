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

  procedure Read (Filename : String;
                  Height   : Angle.Degrees;
                  Width    : Angle.Degrees);

  function Elevation return Integer;

  function Latitude return Angle.Value;

  function Longitude return Angle.Value;

  function Time_Stamp return Time.Ut;
  -- returns the time when the picture is taken.

  function Direction return Space.Direction;
  -- returns the center of the picture in J2000 coordinates.

  function Actual_Direction return Space.Direction;
  -- return the center of the picture in actual coordinates.
  
  function Direction return Earth.Direction;
  -- returns the center of the picture in alt/az coordinates.

end Picture;
