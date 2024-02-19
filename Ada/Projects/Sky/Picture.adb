-- *********************************************************************************************************************
-- *                           (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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

with Ada.Calendar;
with Astro;
with File;
with Site;
with Strings;
with Traces;

package body Picture is

  package Log is new Traces (Id);

  The_Filename : Strings.Element;
  The_Height   : Angle.Degrees;
  The_Width    : Angle.Degrees;


  procedure Define (Name   : String;
                    Height : Angle.Degrees;
                    Width  : Angle.Degrees) is
  begin
    The_Filename := [Name];
    The_Height := Height;
    The_Width := Width;
  end Define;


  function Filename return String is
    use type Strings.Element;
  begin
    return +The_Filename;
  end Filename;


  function Exists return Boolean is
  begin
    return File.Exists (Filename);
  end Exists;


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


  function Image_Of (Item : Angle.Value) return String is
  begin
    return Angle.Image_Of (The_Value   => Item,
                           Unit        => Angle.In_Degrees,
                           Decimals    => 3,
                           Show_Signed => True);
  end Image_Of;


  procedure Define_Site is
    The_Site : Site.Data;
  begin
    The_Site.Latitude := Latitude;
    The_Site.Longitude := Longitude;
    The_Site.Elevation := Elevation;
    Site.Define (The_Site);
    Log.Write ("Elevation :" & Picture.Elevation'image & 'm');
    Log.Write ("Latitude  : " & Image_Of (Picture.Latitude));
    Log.Write ("Longitude : " & Image_Of (Picture.Longitude));
  end Define_Site;


  procedure Set_Site is
  begin
    Exif.Read (Filename);
    Define_Site;
    File.Delete (Filename);
  exception
  when others =>
    File.Delete (Filename);
    raise;
  end Set_Site;


  Is_Solving : Boolean := False;

  function Solve (Search_From : Space.Direction) return Boolean is

    Actual_Height : Angle.Degrees;

    use type Angle.Value;
    use type Angle.Degrees;
    use type Exif.Size;

  begin
    Log.Write ("Read - RA: " & Space.Ra_Image_Of (Search_From) & " - DEC: " & Space.Dec_Image_Of (Search_From));
    Exif.Read (Filename);
    if not Site.Is_Defined then
      Define_Site;
    end if;
    if (Exif.Image_Height = Exif.Undefined_Size) or (Exif.Image_Width = Exif.Undefined_Size) then
      Log.Error ("Image size undefined");
      raise Not_Solved;
    elsif Exif.Image_Height < Exif.Image_Width then
      Actual_Height := The_Height;
    else
      Actual_Height := The_Width;
    end if;
    Astap.Solve (Filename => Filename,
                 Height   => Actual_Height,
                 Start    => [Angle.Degrees'(+Space.Ra_Of (Search_From)),
                              Angle.Degrees'(+Space.Dec_Of (Search_From))]);
    Is_Solving := True;
    return True;
  exception
  when Not_Solved =>
    File.Delete (Filename);
    return False;
  when Exif.File_Not_Found =>
    return False;
  end Solve;


  The_Ra    : Angle.Degrees;
  The_Dec   : Angle.Degrees;
  Is_Solved : Boolean := False;

  function Solved return Boolean is
  begin
    Is_Solved := Astap.Solved (The_Ra, The_Dec);
    if Is_Solved then
      Is_Solving := False;
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
    if Is_Solving then
      Is_Solving := False;
      Astap.Stop;
      File.Delete (Filename);
    end if;
  end Stop_Solving;


  function Universal_Of (DT : String) return Time.Ut is

    The_Time : Ada.Calendar.Time;

    function Seconds_Of (T : String) return Duration is
      --     01234567
      -- T: <hh:mm:ss>
      The_Seconds : Natural;
    begin
      The_Seconds := Natural'value(T(T'first .. T'first + 1)) * 3600;
      The_Seconds := The_Seconds + Natural'value(T(T'first + 3 .. T'first + 4)) * 60;
      The_Seconds := The_Seconds + Natural'value(T(T'first + 6 .. T'first + 7));
      return Duration(The_Seconds);
    end Seconds_Of;

    --      0123456789 76543210
    -- DT: <JJJJ:MM:DD hh:mm:ss>

  begin -- Universal_Of
    The_Time := Ada.Calendar.Time_Of (Year    => Ada.Calendar.Year_Number'value(DT(DT'first .. DT'first + 3)),
                                      Month   => Ada.Calendar.Month_Number'value(DT(DT'first + 5 .. DT'first + 6)),
                                      Day     => Ada.Calendar.Day_Number'value(DT(DT'first + 8 .. DT'first + 9)),
                                      Seconds => Seconds_Of (DT(DT'last - 7 .. DT'last)));
    return Time.Universal_Of (The_Time);
  end Universal_Of;


  function Time_Stamp return Time.Ut is

    Date      : constant Exif.Date := Exif.Date_Stamp;
    TS        : constant Exif.Values := Exif.Time_Stamp;
    Date_Time : constant String := Exif.Date_Time_Digitized;

    use type Exif.Values;

  begin -- Time_Stamp
    if Date /= Exif.Undefined_Date and TS /= Exif.Undefined_Values then
      return Time.Universal_Of (Ut_Year  => Time.Year'value(Date(Date'first .. Date'first + 3)),
                                Ut_Month => Time.Month'value(Date(Date'first + 5 .. Date'first + 6)),
                                Ut_Day   => Time.Day'value(Date(Date'last - 1 .. Date'last)),
                                Ut_Hour  => Duration(Value_Of (TS)));
    elsif Date_Time /= "" then
      return Universal_Of (Date_Time);
    else
      raise Undefined_Value;
    end if;
  end Time_Stamp;


  function Direction return Space.Direction is
  begin
    if not Solved then
      raise Undefined_Value;
    end if;
    return Space.Direction_Of (Ra  => The_Ra,
                               Dec => The_Dec);
  end Direction;


  use Astro;

  procedure Evaluate_Actual (Ra   : out REAL;
                             Dec  : out REAL;
                             Lmst : out Time.Value) is

    use Astro.PNULIB;
    use Astro.SPHLIB;

    TS     : Time.Ut;
    T      : Time.T;
    Pn_Mat : REAL33;
    Ve     : VECTOR;

  begin -- Evaluate_Actual;
    if not Is_Solved then
      raise Undefined_Value;
    end if;
    begin
      TS := Time_Stamp;
      Log.Write ("Time Stamp: " & Time.Image_Of (TS));
      T := Time.Tut_Of (TS);
      Lmst := Time.Lmst_Of (TS);
    exception
    when Undefined_Value =>
      Log.Warning ("Undefined Picture Time_Stamp (used actual time)");
      T := Time.Tut;
      Lmst := Time.Lmst;
    end;
    Ra := REAL(The_Ra);
    Dec := REAL(The_Dec);
    PN_MATRIX (Time.T_J2000, T, Pn_Mat);
    ABERRAT (T, Ve);
    APPARENT (Pn_Mat, Ve, Ra, Dec);
  end Evaluate_Actual;


  procedure Evaluate (Center : out Space.Direction;
                      Lmst   : out Time.Value) is
    Ra, Dec : REAL;
  begin
    Evaluate_Actual (Ra => Ra,
                     Dec => Dec,
                     Lmst => Lmst);
    Center := Space.Direction_Of (Ra  => Ra,
                                  Dec => Dec);
  end Evaluate;


  function Direction return Earth.Direction is

    function Phy return Angle.Value is
    begin
      return Latitude;
    exception
    when Undefined_Value =>
      Log.Warning ("Undefined GPS Latitude (used Site.Latitude)");
      return Site.Latitude;
    end Phy;

    Ra, Dec, Alt, Az : REAL;

    Lmst : Time.Value;

    use Astro.SPHLIB;
    use type Angle.Value;

  begin -- Direction
    Evaluate_Actual (Ra   => Ra,
                     Dec  => Dec,
                     Lmst => Lmst);
    EQUHOR (DEC => +Dec,
            TAU => +(Lmst - Ra),
            PHI => +Phy,
            H   => Alt,
            AZ  => Az);
    return Earth.Direction_Of (Alt => +Alt,
                               Az  => Angle.Semi_Circle + Az);
  end Direction;

end Picture;
