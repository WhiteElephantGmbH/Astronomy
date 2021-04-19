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
with File;
with Site;
with Text;
with Traces;

package body Picture is

  package Log is new Traces ("Picture");

  The_Filename : Text.String;
  The_Height   : Angle.Degrees;
  The_Width    : Angle.Degrees;


  procedure Define (Name   : String;
                    Height : Angle.Degrees;
                    Width  : Angle.Degrees) is
  begin
    The_Filename := Text.String_Of (Name);
    The_Height := Height;
    The_Width := Width;
  end Define;


  function Filename return String is
  begin
    return Text.String_Of (The_Filename);
  end Filename;


  function Exists return Boolean is
  begin
    return File.Exists (Filename);
  end Exists;


  function Solve (Search_From : Space.Direction) return Boolean is

    Actual_Height : Astap.Degrees;

    use type Angle.Value;
    use type Angle.Degrees;
    use type Exif.Size;

  begin
    Log.Write ("Read - RA: " & Space.Ra_Image_Of (Search_From) & " - DEC: " & Space.Dec_Image_Of (Search_From));
    Exif.Read (Filename);
    if (Exif.Image_Height = Exif.Undefined_Size) or (Exif.Image_Width = Exif.Undefined_Size) then
      Log.Error ("Image size undefined");
      raise Not_Solved;
    elsif Exif.Image_Height < Exif.Image_Width then
      Actual_Height := Astap.Degrees(The_Height);
    else
      Actual_Height := Astap.Degrees(The_Width);
    end if;
    Astap.Solve (Filename => Filename,
                 Height   => Actual_Height,
                 Ra       => Astap.Degrees(Angle.Degrees'(+Space.Ra_Of (Search_From))),
                 Dec      => Astap.Degrees(Angle.Degrees'(+Space.Dec_Of (Search_From))));
    return True;
  exception
  when Not_Solved =>
    File.Delete (Filename);
    return False;
  end Solve;


  The_Ra    : Astap.Degrees;
  The_Dec   : Astap.Degrees;
  Is_Solved : Boolean := False;

  function Solved return Boolean is
  begin
    Is_Solved := Astap.Solved (Ra => The_Ra, Dec => The_Dec);
    if Is_Solved then
      File.Delete (Filename);
    end if;
    return Is_Solved;
  exception
  when others =>
    File.Delete (Filename);
    raise;
  end Solved;


  procedure Stop_Solving is
  begin
    Astap.Stop;
    File.Delete (Filename);
  end Stop_Solving;
  

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


  use Astro;

  procedure Evaluate_Actual (Ra  : out REAL;
                             Dec : out REAL) is

    function T return Time.T is
    begin
      return Time.Tut_Of (Time_Stamp);
    exception
    when Undefined_Value =>
      Log.Warning ("Undefined GPS Time_Stamp (used actual time)");
      return Time.Tut;
    end T;

    use Astro.PNULIB;
    use Astro.SPHLIB;

    Pn_Mat : REAL33;
    Ve     : VECTOR;

  begin -- Evaluate_Actual;
    if not Is_Solved then
      raise Undefined_Value;
    end if;
    Ra := REAL(The_Ra);
    Dec := REAL(The_Dec);
    PN_MATRIX (Time.T_J2000, T, Pn_Mat);
    ABERRAT (T, Ve);
    APPARENT (Pn_Mat, Ve, Ra, Dec);
  end Evaluate_Actual;


  function Actual_Direction return Space.Direction is
    Ra, Dec : REAL;
  begin
    Evaluate_Actual (Ra => Ra, Dec => Dec);
    return Space.Direction_Of (Ra  => Angle.Degrees(Ra),
                               Dec => Angle.Degrees(Dec));
  end Actual_Direction;


  function Direction return Earth.Direction is

    function Lmst return Time.Value is
    begin
      return Time.Lmst_Of (Time_Stamp);
    exception
    when Undefined_Value =>
      return Time.Lmst;
    end Lmst;

    function Phy return Angle.Value is
    begin
      return Latitude;
    exception
    when Undefined_Value =>
      Log.Warning ("Undefined GPS Latitude (used Site.Latitude)");
      return Site.Latitude;
    end Phy;

    Ra, Dec, Alt, Az : REAL;

    use Astro.SPHLIB;
    use type Angle.Value;

  begin -- Direction
    Evaluate_Actual (Ra => Ra, Dec => Dec);
    EQUHOR (DEC => +Dec,
            TAU => +(Lmst - Ra),
            PHI => +Phy,
            H   => Alt,
            AZ  => Az);
    return Earth.Direction_Of (Alt => +Alt,
                               Az  => Angle.Semi_Circle + Az);
  end Direction;

end Picture;
