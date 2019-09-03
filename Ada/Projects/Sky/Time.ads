-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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


  -------------------
  -- set longitude --
  -------------------

  procedure Set (Longitude : Value); -- east positive


  -------------------------
  -- local date and time --
  -------------------------

  subtype Day     is Ada.Calendar.Day_Number;
  subtype Month   is Ada.Calendar.Month_Number;
  subtype Year    is Ada.Calendar.Year_Number;

  function Local_Time return Value;

  -- Precondition: Local_Time read
  function Local_Shift return Duration;
  function Local_Day   return Day;
  function Local_Month return Month;
  function Local_Year  return Year;


  --------------------
  -- universal time --
  --------------------

  subtype Ut is Duration; -- in seconds since year 2000

  In_The_Past   : constant Ut := 0.0;
  In_The_Future : constant Ut := Ut'last;

  type Period is record
    Arrival_Time : Ut := In_The_Future;
    Leaving_Time : Ut := In_The_Past;
  end record;

  Undefined : constant Period := (others => <>);

  function Universal return Ut;

  function Nearest_Universal (Base : Ut) return Ut;

  function Synchronized_Universal_Of (Item : Ut;
                                      Base : Ut) return Ut;

  function Synchronized_Universal (Base : Ut) return Ut;

  function Image_Of (Item      : Ut;
                     Time_Only : Boolean := False) return String;


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

  function Julian_Date_Of (Utime : Duration) return Astro.REAL;

  function Ut_Of (Jd : String) return Ut;

end Time;
