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

with Unsigned;

package Exif is

  File_Not_Found : exception;
  Invalid_File   : exception;
  
  subtype Reference is Character;
  
  Undefined_Ref : constant Reference := ' ';
  
  type Size is new Unsigned.Word;
  
  Undefined_Size : constant Size := 0;

  type Longword is new Unsigned.Longword;

  type Rational_Value is record
    Nominator   : Longword := 0;
    Denominator : Longword := 0;
  end record;
  
  type Rational_Values is array (Positive range <>) of Rational_Value;

  type Image_Orientation is (Undefined, Horizontal, Mirror_Horizontal, Rotate_180, Mirror_Vertical,
                             Mirror_Horizontal_And_Rotate_270, Rotate_90, Mirror_Horizontal_And_Rotate_90, Rotate_270);
                             -- rotations are clockwise

  type See_Level is (Below, Above, Undefined);
  
  subtype Height is Rational_Value; -- in meters

  Undefined_Height : constant Height := (others => <>);
  
  subtype Values is Rational_Values(1..3);

  Undefined_Values : constant Values := (others => <>);
  
  subtype Date is String(1..10); -- yyyy:mm:dd
  
  Undefined_Date : constant Date := (others => ' ');

  procedure Read (Filename : String);

  function Orientation return Image_Orientation;

  function Image_Height return Size;

  function Image_Width return Size;

  function Altitude_Ref return See_Level;

  function Altitude return Height;

  function Latitude_Ref return Reference; -- N or S

  function Latitude return Values;

  function Longitude_Ref return Reference; -- E or W

  function Longitude return Values;

  function Time_Stamp return Values;
  
  function Date_Stamp return Date;

end Exif;
