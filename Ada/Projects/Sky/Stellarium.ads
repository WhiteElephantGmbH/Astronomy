-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Language;
with Network;
with Space;

package Stellarium is

  subtype Port_Number is Network.Port_Number;

  type Magnitude is delta 0.1 range -30.0 .. 30.0;

  type Goto_Handler is access procedure (Direction : Space.Direction);

  procedure Set_Maximum (Item : Magnitude);

  function Magnitude_Maximum return Magnitude;

  function Startup (Filename : String;
                    The_Port : Network.Port_Number) return Boolean;

  procedure Shutdown;

  function Landscape_Filename return String;

  function Landscape_Rotation return Angle.Degrees;

  function Language return Language.Kind;

  function Satellites_Filename return String;

  procedure Start (Used_Port : Port_Number);

  procedure Define_Handler (The_Handler : Goto_Handler);

  procedure Set (Direction : Space.Direction);

  procedure Close;

end Stellarium;
