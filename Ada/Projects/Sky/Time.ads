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

with Ada.Calendar;
with Angle;
with Astro;

package Time is

  Epsilon : constant := 1.0E-4;

  One_Second : constant Duration := 1.0;
  One_Minute : constant Duration := One_Second * 60.0;
  One_Hour   : constant Duration := One_Minute * 60.0;
  One_Day    : constant Duration := One_Hour * 24.0;

  subtype Value is Angle.Value;

  function Image_Of (Item : Value) return String;

  subtype Day   is Ada.Calendar.Day_Number;
  subtype Month is Ada.Calendar.Month_Number;
  subtype Year  is Ada.Calendar.Year_Number;

  subtype Calendar_Value is Ada.Calendar.Time;

  function Calendar_Value_Of (Image : String) return Calendar_Value;
  -- Item -> [d]d.[m]m.yyyy [h]h:[m]m[:[s]s[.s]]
  Illegal      : exception;
  Out_Of_Range : exception;

  function Calendar_Now return Calendar_Value is (Ada.Calendar.Clock);


  -------------------------
  -- local date and time --
  -------------------------

  function Local_Time return Value;

  -- Precondition: Local_Time read
  function Local_Shift return Duration;
  function Local_Day   return Day;
  function Local_Month return Month;
  function Local_Year  return Year;


  --------------------
  -- universal time --
  --------------------

  Ut_Range_Error : exception;

  Delta_Time : constant := 10.0**(-8);

  type Ut is delta Delta_Time range -(2**63 * Delta_Time) .. +((2**63 - 1) * Delta_Time)
  with
    Small => Delta_Time,
    Size  => 64;

  function "+" (Left  : Ut;
                Right : Duration) return Ut;

  function "-" (Left  : Ut;
                Right : Duration) return Ut;

  In_The_Past   : constant Ut := 0.0;
  In_The_Future : constant Ut := Ut'last;
  Unknown       : constant Ut := 0.0;

  type Period is record
    Arrival_Time : Ut := In_The_Future;
    Leaving_Time : Ut := In_The_Past;
  end record;

  Undefined : constant Period := (others => <>);

  function Local_Of (Item : Ut) return Calendar_Value;

  function Universal_Of (Time_Local : Calendar_Value) return Ut;

  function Universal return Ut;

  function Nearest_Universal (Base : Duration) return Ut;

  function Synchronized_Universal_Of (Item : Ut;
                                      Base : Duration) return Ut;

  function Synchronized_Universal (Base : Duration) return Ut;

  function Universal_Of (Ut_Year  : Year;
                         Ut_Month : Month;
                         Ut_Day   : Day;
                         Ut_Hour  : Duration) return Ut;

  function Image_Of (Item      : Ut;
                     Time_Only : Boolean := False) return String;
  -- returns the local time image


  -----------------------------
  -- local mean sideral time --
  -----------------------------

  function Lmst return Value;

  function Lmst_Of (Item : Ut) return Value;


  ----------------
  -- standard T --
  ----------------

  subtype T is Astro.REAL; -- in 100 years since year 2000

  use type T;

  T_J2000 : constant T := 0.0;

  T_Second : constant T := 1.0 / (36525.0 * T(One_Day));

  function Tut return T;

  function Tet_Of (Item : Ut) return T;

  function Tut_Of (Item : Ut) return T;


  -----------------
  -- Julian Date --
  -----------------

  JD_Second : constant := 1.0 / 86_400.0;

  JD_Minute : constant := 60.0 * JD_Second;

  JD_Delta : constant := JD_Second / 10_000.0 ;

  type JD is delta JD_Delta range -2 ** 63 * JD_Delta .. (2 ** 63 - 1) * JD_Delta with
    Small => JD_Delta,
    Size  => 64;

  function Julian_Date return JD;

  function Julian_Date_Of (Utime : Ut) return JD;

  function Ut_Of (Item : JD) return Ut;

end Time;
