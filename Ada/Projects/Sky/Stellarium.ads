-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Applications;
with Language;
with Network;
with Space;

package Stellarium is

  package Application is new Applications (Product => "stellarium"); -- lower case for Linux

  subtype Port_Number is Network.Port_Number;

  type Goto_Handler is access procedure (Direction : Space.Direction);

  function Altitude return Integer;

  function Latitude return Angle.Degrees;

  function Longitude return Angle.Degrees;

  function Landscape return String;

  function Language return Language.Kind;

  procedure Start (Used_Port : Port_Number);

  procedure Define_Handler (The_Handler : Goto_Handler);

  procedure Set (Direction : Space.Direction);

  procedure Close;

end Stellarium;
