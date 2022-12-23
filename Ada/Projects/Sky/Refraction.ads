-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package Refraction is

  type Celsius is delta  10.0**(-1) range -999.9 .. +999.9;

  type Hectopascal is delta 10.0**(-1) range 0000.0 .. 9999.9;

  Undefined_Temperature  : constant Celsius := Celsius'last;
  Undefined_Air_Pressure : constant Hectopascal := Hectopascal'last;

  procedure Set (The_Air_Pressure : Hectopascal);

  function New_Air_Pressure return Boolean;

  function Air_Pressure return Hectopascal;
  
  procedure Set (The_Temperature : Celsius);

  function New_Temperature return Boolean;

  function Temperature return Celsius;

  procedure Correct (Alt : in out Angle.Degrees);

end Refraction;
