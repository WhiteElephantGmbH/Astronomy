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

with Astro;
with Numerics;
with Traces;

package body Picture is

  package Log is new Traces ("Picture");

  Solved  : Boolean := False;
  The_Ra  : Astap.Degrees;
  The_Dec : Astap.Degrees;


  procedure Read (Filename    : String;
                  Height      : Angle.Degrees;
                  Width       : Angle.Degrees;
                  Search_From : Space.Direction) is

    The_Height : Astap.Degrees;

    use all type Exif.Image_Orientation;
    use type Angle.Value;
    use type Angle.Degrees;

  begin
    Solved := False;
    Exif.Read (Filename);
    The_Ra := Astap.Degrees(Angle.Degrees'(+Space.Ra_Of (Search_From)));
    The_Dec := Astap.Degrees(Angle.Degrees'(+Space.Dec_Of (Search_From)));
    case Exif.Orientation is
    when Undefined =>
      Log.Warning ("Orientation Undefined");
      begin
        -- try width
        Astap.Solve (Filename => Filename,
                     Height   => Astap.Degrees(Width),
                     Ra       => The_Ra,
                     Dec      => The_Dec);
        Solved := True;
        return;
      exception
      when Not_Solved =>
        -- when not solved try height
        The_Height := Astap.Degrees(Height);
      end;
    when Horizontal | Mirror_Horizontal | Rotate_180 | Mirror_Vertical =>
      The_Height := Astap.Degrees(Height);
    when Mirror_Horizontal_And_Rotate_270 | Rotate_90 | Mirror_Horizontal_And_Rotate_90 | Rotate_270 =>
      The_Height := Astap.Degrees(Width);
    end case;
    Astap.Solve (Filename => Filename,
                 Height   => The_Height,
                 Ra       => The_Ra,
                 Dec      => The_Dec);
    Solved := True;
  end Read;


  Value_Delta   : constant := 1.0 / 3600_000.0; -- millisecond
  Lowest_Value  : constant := -2**62 * Value_Delta;
  Highest_Value : constant := 2**62 * Value_Delta;

  type Value is delta Value_Delta range Lowest_Value .. Highest_Value;


  function Value_Of (Item : Exif.Rational_Value) return Value is
  begin
    return  Value(Item.Nominator) / Value(Item.Denominator);
  end Value_Of;


  function Value_Of (Item : Exif.Values) return Value is
  begin
    return Value_Of (Item(Item'first))
         + Value_Of (Item(Item'first + 1)) / 60.0
         + Value_Of (Item(Item'first + 2)) / 3600.0;
  end Value_Of;


  function Elevation return Integer is

    See_Level : constant Exif.See_Level := Exif.Altitude_Ref;
    Altitude  : constant Exif.Height    := Exif.Altitude;

    The_Elevation : Integer;

    use type Exif.Rational_Value;
    use type Exif.See_Level;

  begin
    if See_Level = Exif.Undefined or Altitude = Exif.Undefined_Height then
      raise Undefined_Value;
    end if;
    The_Elevation := Integer(Value_Of (Altitude));
    if See_Level = Exif.Below then
      The_Elevation := - The_Elevation;
    end if;
    return The_Elevation;
  end Elevation;


  function Angle_Of (Item        : Exif.Rational_Values;
                     Is_Negative : Boolean) return Angle.Value is
    The_Degrees : Angle.Degrees;
    use type Angle.Value;
    use type Exif.Values;
    use type Angle.Degrees;
  begin
    if Item = Exif.Undefined_Values then
      raise Undefined_Value;
    end if;
    The_Degrees := Angle.Degrees(Value_Of (Item));
    if Is_Negative then
      The_Degrees := - The_Degrees;
    end if;
    return +The_Degrees;
  end Angle_Of;


  function Latitude return Angle.Value is
  begin
    return Angle_Of (Exif.Latitude, Exif.Latitude_Ref = 'S');
  end Latitude;


  function Longitude return Angle.Value is
  begin
    return Angle_Of (Exif.Longitude, Exif.Latitude_Ref = 'W');
  end Longitude;


  function Time_Stamp return Time.Ut is

    Date : constant Exif.Date := Exif.Date_Stamp;
    TS   : constant Exif.Values := Exif.Time_Stamp;

    use type Exif.Values;

  begin -- Time_Stamp
    if Date = Exif.Undefined_Date or TS = Exif.Undefined_Values then
      raise Undefined_Value;
    end if;
    return Time.Universal_Of (Ut_Year  => Time.Year'value(Date(Date'first .. Date'first + 3)),
                              Ut_Month => Time.Month'value(Date(Date'first + 5 .. Date'first + 6)),
                              Ut_Day   => Time.Day'value(Date(Date'last - 1 .. Date'last)),
                              Ut_Hour  => Duration(Value_Of (TS)));
  end Time_Stamp;


  function Direction return Space.Direction is
  begin
    if not Solved then
      raise Undefined_Value;
    end if;
    return Space.Direction_Of (Ra  => Angle.Degrees(The_Ra),
                               Dec => Angle.Degrees(The_Dec));
  end Direction;


  function Actual_Direction return Space.Direction is

    T : constant Time.T := Time.Tut_Of (Time_Stamp);

    use Astro;
    use Astro.PNULIB;
    use Astro.SPHLIB;

    Pn_Mat : REAL33;
    Ve     : VECTOR;
    Ra     : REAL := REAL(The_Ra);
    Dec    : REAL := REAL(The_Dec);

  begin
    if not Solved then
      raise Undefined_Value;
    end if;
    PN_MATRIX (Time.T_J2000, T, Pn_Mat);
    ABERRAT (T, Ve);
    APPARENT (Pn_Mat, Ve, Ra, Dec);
    return Space.Direction_Of (Ra  => Angle.Degrees(Ra),
                               Dec => Angle.Degrees(Dec));
  end Actual_Direction;


  function Direction return Earth.Direction is
  begin
    return Numerics.Direction_Of (Direction => Actual_Direction,
                                  Latitude  => Latitude,
                                  Lmst      => Time.Lmst_Of (Time_Stamp));
  end Direction;

end Picture;
