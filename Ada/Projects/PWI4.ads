-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

private with AWS.Messages;

package PWI4 is

  Request_Rate : constant := 2; -- per second

  Arc_Second_Delta : constant := 0.000_1;
  Arc_Second_Limit : constant := 10_000_000.0 - Arc_Second_Delta;

  type Arc_Second is delta Arc_Second_Delta range -Arc_Second_Limit .. Arc_Second_Limit with Small => Arc_Second_Delta;

  function Image_Of (Item : Arc_Second) return String;

  Degrees_Delta : constant := 0.000_000_000_1;
  Degrees_Limit : constant := 1000.0 - Degrees_Delta;

  type Degrees is delta Degrees_Delta range -Degrees_Limit .. Degrees_Limit with Small => Degrees_Delta;

  function Image_Of (Item : Degrees) return String;

  Hours_Delta : constant := 0.000_000_000_01;
  Hours_Limit : constant := 24.0;

  type Hours is delta Hours_Delta range -Hours_Limit .. Hours_Limit with Small => Hours_Delta;

  Undefined_Hours : constant := Hours'first;

  function Image_Of (Item : Hours) return String;

  type Axis_Data is record
    Position : Degrees;
  end record;

  type Points is new Natural range 0 .. 255;

  function Image_Of (Item : Points) return String;

  type Model_Data is record
    Points_Total   : Points;
    Points_Enabled : Points;
  end record;

  Microns_Delta : constant := 0.1;
  Microns_Limit : constant := 100000.0 - Microns_Delta;

  type Microns is delta Microns_Delta range -Microns_Limit .. Microns_Limit with Small => Microns_Delta;

  function Image_Of (Item : Microns) return String;

  type M3_Port is (Unknown, Between, Port_1, Port_2);

  subtype Port is M3_Port range Port_1 .. Port_2;

  function Startup (Filename   : String;
                    Ip_Address : String) return Boolean;

  procedure Shutdown;

  procedure Get_System;

private

  subtype Client_Error is AWS.Messages.Client_Error;

  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "");

  function Image_Of (The_Port : Port) return Character;

end PWI4;
