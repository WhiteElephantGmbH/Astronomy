-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ten_Micron;
with Space;
with Time;
with Traces;

package Alignment is

  procedure Clear;

  function Align_More return Boolean;

  function Next_Star return Space.Direction;

  function Star_Count return Natural;

  procedure Define (Direction : Space.Direction;
                    Lmst      : Time.Value;
                    Pier_Side : Character);

  function Ready return Boolean;

  procedure Generate;

  subtype Information is Ten_Micron.Alignment_Data;

  procedure Update_Info;

  function Info return Information;

private

  Id : constant String := "Alignment";

  package Log is new Traces (Id);

  The_Alignment_Stars : Natural;

end Alignment;
