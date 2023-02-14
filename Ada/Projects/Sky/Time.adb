-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar.Time_Zones;
with Site;
with Strings;

package body Time is

  function "+" (Left  : Ut;
                Right : Duration) return Ut is
  begin
    return Left + Ut(Right);
  end "+";


  function "-" (Left  : Ut;
                Right : Duration) return Ut is
  begin
    return Left - Ut(Right);
  end "-";


  function Image_Of (Item : Value) return String is
  begin
    return Angle.Image_Of (Item, Angle.In_Hours);
  end Image_Of;


  function Time_Shift (Now : Ada.Calendar.Time) return Duration is
  begin
    return Duration(Ada.Calendar.Time_Zones.UTC_Time_Offset (Now)) * 60.0;
  end Time_Shift;


  function Lambda return Angle.Value is
  begin
    return Site.Longitude;
  end Lambda;


  -------------------------
  -- local date and time --
  -------------------------

  Actual_Day   : Day;
  Actual_Month : Month;
  Actual_Year  : Year;

  The_Time_Shift : Duration;


  function Local_Time return Value is
    Now         : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    The_Seconds : Ada.Calendar.Day_Duration;
    use all type Angle.Value;
    use type Angle.Hours;
  begin
    Ada.Calendar.Split (Date    => Now,
                        Year    => Actual_Year,
                        Month   => Actual_Month,
                        Day     => Actual_Day,
                        Seconds => The_Seconds);
    The_Time_Shift := Time_Shift (Now);
    return +(Angle.Hours(The_Seconds) / 3600.0);
  end Local_Time;


  function Local_Shift return Duration is
  begin
    return The_Time_Shift;
  end Local_Shift;


  function Local_Day return Day is
  begin
    return Actual_Day;
  end Local_Day;


  function Local_Month return Month is
  begin
    return Actual_Month;
  end Local_Month;


  function Local_Year return Year is
  begin
    return Actual_Year;
  end Local_Year;


  function Calendar_Value_Of (Image : String) return Calendar_Value is
  begin
    declare
      Images      : constant Strings.Item := Strings.Item_Of (Image, Separator => ' ');
      Date_Image  : constant Strings.Item := Strings.Item_Of (Images(1), Separator => '.', Purge => False);
      Time_Image  : constant Strings.Item := Strings.Item_Of (Images(2), Separator => ':', Purge => False);
      Year_Image  : constant String := Date_Image(3);
      The_Year    : constant Integer := Integer'value(Year_Image);
      The_Seconds : Duration;
    begin
      if not (The_Year in Year) then
        raise Out_Of_Range;
      elsif Images.Count = 2 and then Date_Image.Count = 3 and then Time_Image.Count in 2 .. 3 then
        The_Seconds := Natural'value(Time_Image(1)) * One_Hour + Natural'value(Time_Image(2)) * One_Minute;
        if Time_Image.Count = 3 then
          begin
            The_Seconds := @ + Duration(Natural'value(Time_Image(3)));
          exception
          when others =>
            The_Seconds := @ + Duration'value(Time_Image(3));
          end;
        end if;
        return Ada.Calendar.Time_Of (Year    => Year'value(Date_Image(3)),
                                     Month   => Month'value(Date_Image(2)),
                                     Day     => Day'value(Date_Image(1)),
                                     Seconds => The_Seconds);
      end if;
      raise Illegal;
    end;
  exception
  when Out_Of_Range =>
    raise;
  when others =>
    raise Illegal;
  end Calendar_Value_Of;


  --------------------------
  -- Modified Julian Date --
  --------------------------

  function Mod_Jd_Of (Time_Local : Ada.Calendar.Time) return Astro.REAL is
    use type Ada.Calendar.Time;
    use Astro.TIMLIB;
    Y, M, D  : Natural;
    S        : Duration;
  begin
    Ada.Calendar.Split (Date    => Time_Local - Time_Shift (Time_Local),
                        Year    => Y,
                        Month   => M,
                        Day     => D,
                        Seconds => S);
    return MJD (D, M, Y, HOURS(S) / 3600.0);
  exception
  when others =>
    raise Ut_Range_Error;
  end Mod_Jd_Of;


  function Mod_Jd return Astro.REAL is
  begin
    return Mod_Jd_Of (Ada.Calendar.Clock);
  end Mod_Jd;


  -----------------------------
  -- local mean sideral time --
  -----------------------------

  function Lmst return Value is
    use all type Angle.Value;
  begin
    return +Astro.TIMLIB.LMST (Mod_Jd, - (+Lambda));
  end Lmst;


  function Lmst_Of (Item : Ut) return Value is
    use Astro;
    Modjd : constant REAL := REAL(Item / One_Day) + TIMLIB.MJD_OFFSET;
    use all type Angle.Value;
  begin
    return +TIMLIB.LMST (Modjd, - (+Lambda));
  end Lmst_Of;


  --------------------
  -- universal time --
  --------------------

  function Local_Of (Item : Ut) return Ada.Calendar.Time is

    use Astro;
    use TIMLIB;

    Modjd : constant REAL :=  REAL((Item) / One_Day) + MJD_OFFSET;

    The_Day          : Natural;
    The_Month        : Natural;
    The_Year         : Natural;
    The_Hours        : HOURS;

  begin
    CALDAT (Modjd, The_Day, The_Month, The_Year, The_Hours);
    return Ada.Calendar.Time_Of (Year    => The_Year,
                                 Month   => The_Month,
                                 Day     => The_Day,
                                 Seconds => Duration(The_Hours) * One_Hour);
  end Local_Of;


  function Universal_Of (Time_Local : Ada.Calendar.Time) return Ut is
  begin
    return Ut(Mod_Jd_Of (Time_Local) -Astro.TIMLIB.MJD_OFFSET) * One_Day;
  end Universal_Of;


  function Universal return Ut is
  begin
    return Ut(Mod_Jd - Astro.TIMLIB.MJD_OFFSET) * One_Day;
  end Universal;


  function Nearest_Universal (Base : Duration) return Ut is
  begin
    return Ut(Long_Long_Integer(Universal / Base)) * Base;
  end Nearest_Universal;


  function Synchronized_Universal_Of (Item : Ut;
                                      Base : Duration) return Ut is
  begin
    return Ut(Long_Long_Integer((Item + Base) / Base)) * Base;
  end Synchronized_Universal_Of;


  function Synchronized_Universal (Base : Duration) return Ut is
  begin
    return Synchronized_Universal_Of (Universal, Base);
  end Synchronized_Universal;


  function Universal_Of (Ut_Year  : Year;
                         Ut_Month : Month;
                         Ut_Day   : Day;
                         Ut_Hour  : Duration) return Ut is

    Mjd : constant Astro.REAL := Astro.TIMLIB.MJD (DAY   => Ut_Day,
                                                   MONTH => Ut_Month,
                                                   YEAR  => Ut_Year,
                                                   HOUR  => Astro.TIMLIB.HOURS(Ut_Hour));
  begin
    return Ut(Mjd - Astro.TIMLIB.MJD_OFFSET) * One_Day;
  end Universal_Of;


  function Image_Of (Item      : Ut;
                     Time_Only : Boolean := False) return String is

  begin
    declare

      use Astro;
      use TIMLIB;

      TS : constant Duration := Time_Shift (Local_Of (Item));

      Modjd : constant Astro.REAL :=  Astro.REAL((Item + TS) / One_Day) + MJD_OFFSET;

      function Image_Of (Number : Natural) return String is
        Image : constant String := Number'img;
      begin
        return Image(Image'first + 1 .. Image'last);
      end Image_Of;

      function Filed_Image_Of (Number : Natural) return String is
        Image : constant String := "0" & Image_Of(Number);
      begin
        return Image(Image'last - 1 .. Image'last);
      end Filed_Image_Of;

      The_Day          : Natural;
      The_Month        : Natural;
      The_Year         : Natural;
      The_Hours        : HOURS;
      The_Hour         : Natural;
      The_Minutes      : Natural;
      The_Seconds      : Natural;
      The_Deci_Seconds : Natural;

    begin
      CALDAT (Modjd, The_Day, The_Month, The_Year, The_Hours);
      The_Deci_Seconds := Natural (The_Hours * 36000.0);
      The_Hour := The_Deci_Seconds / 36000;
      The_Deci_Seconds := The_Deci_Seconds - The_Hour * 36000;
      The_Minutes := The_Deci_Seconds / 600;
      The_Deci_Seconds := The_Deci_Seconds - The_Minutes * 600;
      The_Seconds := The_Deci_Seconds / 10;
      The_Deci_Seconds := The_Deci_Seconds - The_Seconds * 10;
      declare
        Time_Image : constant String := Filed_Image_Of (The_Hour) & ":" & Filed_Image_Of (The_Minutes) & ":" &
                                        Filed_Image_Of (The_Seconds) & '.' & Image_Of (The_Deci_Seconds);
      begin
        if Time_Only then
          return Time_Image;
        elsif Item = In_The_Past then
          return "<undefined>";
        else
          return Image_Of (The_Day) & '.' & Image_Of (The_Month) & "." & Image_Of (The_Year) & " " & Time_Image;
        end if;
      end;
    end;
  exception
  when others =>
    return "<undefined>";
  end Image_Of;


  ----------------
  -- standard T --
  ----------------

  function Tut return T is
  begin
    return Tut_Of (Universal);
  end Tut;


  function Tet_Of (Item : Ut) return T is
    use Astro;
    use TIMLIB;
    DT    : REAL;
    Is_Ok : Boolean;
    Utime : constant T := Tut_Of (Item);
  begin
    ETMINUT (Utime, DT, Is_Ok);
    if Is_Ok then
      return DT * T_Second + Utime;
    else
      return Utime;
    end if;
  end Tet_Of;


  function Tut_Of (Item : Ut) return T is
  begin
    return T(Item) * T_Second;
  end Tut_Of;


  -----------------
  -- Julian Date --
  -----------------

  JD_Offset : constant Astro.REAL := 2451545.0;

  function Julian_Date_Of (Utime : Ut) return Astro.REAL is
  begin
     return Astro.REAL(Utime / One_Day) + JD_Offset;
  end Julian_Date_Of;


  function Ut_Of (Jd : String) return Ut is
  begin
    return Ut(Astro.REAL'value(Jd) - JD_Offset) * One_Day;
  end Ut_Of;

end Time;
