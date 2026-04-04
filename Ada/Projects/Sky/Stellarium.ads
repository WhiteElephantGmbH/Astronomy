-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Angle;
with Language;
with Network;
with Space;
with Text;
with Traces;

package Stellarium is

  type Goto_Handler is access procedure (Direction : Space.Direction);

  function Landscape_Filename return String;

  function Landscape_Rotation return Angle.Degrees;

  function Language return Language.Kind;

  function Search_Tolerance return Angle.Degrees;

  procedure Startup;

  procedure Shutdown;

  procedure Define_Handler (The_Handler : Goto_Handler);

  procedure Set (Direction : Space.Direction);

private

  Id : constant String := "Stellarium";

  package Log is new Traces (Id);

  The_Filename : Text.String;

  The_Port_Number : Network.Port_Number;

  The_Search_Tolerance : Angle.Degrees;

end Stellarium;
