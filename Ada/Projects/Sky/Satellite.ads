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

with Norad;
with Discrete_Set;

package Satellite is

  subtype Number is Norad.Number;

  package Numbers is new Discrete_Set (Number);

  subtype Tle is Norad.Two_Line;

  procedure Read_Data;

  function Targets return Numbers.Set;

  function Tle_Of (Object : Number) return Tle;

  function Tle_Name_Of (Object : Number) return String;

  function Name_Of (Object : Number) return String;
  
end Satellite;
