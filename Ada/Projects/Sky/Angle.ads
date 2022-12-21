-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Astro;

package Angle is

  type Signed is range - 16#8000_0000# .. 16#7FFF_FFFF# with Size => 32;

  type Unsigned is mod 2 ** 32 with Size => 32;

  subtype Degrees is Astro.REAL;

  subtype Hours is Astro.TIMLIB.HOURS;

  type Value is private;

  type Values is array (Positive range <>) of Value;

  Zero : constant Value;

  Quadrant : constant Value; -- 90 degrees

  Semi_Circle : constant Value; -- 180 degrees

  Negative_Quadrant : constant Value; -- 270 degrees

  One_Minute : constant Value;

  Maximum : constant Value; -- just before zero (cyclic)

  Epsilon : constant Value; -- smallest value

  function Degree return String;

  function "+" (Item : Degrees) return Value;

  function "+" (Item : Degrees) return Signed;

  function "+" (Item : Degrees) return Unsigned;

  function "+" (Item : Value) return Degrees;

  function "+" (Item : Hours) return Value;

  function "+" (Item : Value) return Hours;

  function "+" (Item : Signed) return Value with Inline;

  function "+" (Item : Signed) return Degrees with Inline;

  function "+" (Item : Value) return Signed with Inline;

  function "-" (Item : Value) return Signed with Inline;

  function "+" (Item : Unsigned) return Value with Inline;

  function "+" (Item : Unsigned) return Degrees with Inline;

  function "+" (Item : Value) return Unsigned with Inline;

  function "+" (Left, Right : Value) return Value with Inline;

  function "+" (Left : Value; Right : Degrees) return Value with Inline;

  function "+" (Left : Value; Right : Signed) return Value with Inline;

  function "+" (Left : Unsigned; Right : Signed) return Unsigned with Inline;

  function "-" (Left, Right : Value) return Value with Inline;

  function "-" (Left, Right : Value) return Signed with Inline;

  function "-" (Left, Right : Value) return Degrees with Inline;

  function "-" (Left : Value; Right : Signed) return Value with Inline;

  function "-" (Left : Value; Right : Signed) return Unsigned with Inline;

  function "-" (Left, Right : Unsigned) return Signed with Inline;

  function "-" (Left : Value; Right : Degrees) return Value with Inline;

  function "<" (Left, Right : Value) return Boolean with Inline;

  type Decimal_Places is range 0 .. 3;

  type Units is (In_Degrees, In_Hours, Default_Degrees, Default_Hours);

  subtype In_Unit is Units range In_Degrees .. In_Hours;

  function Image_Of (The_Value   : Value;
                     Unit        : In_Unit := In_Degrees;
                     Decimals    : Decimal_Places := 1;
                     Show_Signed : Boolean := False) return String;

  function Value_Of (The_Image  : String;
                     With_Units : Units := Default_Degrees) return Value;

  function Hours_Image_Of (Item : Value) return String;

  function Signed_Degrees_Image_Of (Item : Value) return String;

  function Unsigned_Degrees_Image_Of (Item : Value) return String;

  function Degrees_Of (The_Image : String;
                       Limit     : Degrees) return Degrees;

  function "+" (Left  : String;
                Right : Unsigned) return String;

  function "+" (Left  : String;
                Right : Signed) return String;

  function Interpolation_Of (A, A1, A2 : Unsigned;
                                V1, V2 : Signed) return Signed;

private

  type Value is new Unsigned;

  Zero : constant Value := Value'first;

  Maximum : constant Value := Value'last;

  Epsilon : constant Value := 1;

  Quadrant          : constant Value := 16#4000_0000#; -- 90 degrees
  Semi_Circle       : constant Value := 16#8000_0000#; -- 180 degrees
  Negative_Quadrant : constant Value := 16#C000_0000#; -- 270 degrees
  One_Minute        : constant Value := Quadrant / (90 * 60);

end Angle;
