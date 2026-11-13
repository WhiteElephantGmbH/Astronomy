-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Real_Time;

package Rt is

  subtype Time is Ada.Real_Time.Time;

  subtype Time_Span is Ada.Real_Time.Time_Span;

  function Now return Time renames Ada.Real_Time.Clock;

  function To_Duration (TS : Time_Span) return Duration renames Ada.Real_Time.To_Duration;

  function To_Time_Span (D : Duration) return Time_Span renames Ada.Real_Time.To_Time_Span;

  For_Termination : constant Duration := 0.01;

  function In_Future (Time_Offset : Duration) return Time;

  procedure Wait (D : Duration := For_Termination);

end Rt;
