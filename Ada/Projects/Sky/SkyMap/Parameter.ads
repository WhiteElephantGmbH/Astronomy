-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Eps;
with Solar;
with Star;

package Parameter is

  procedure Read;

  function Earth_Color return Eps.Color;

  function Day_Sky_Color return Eps.Color;

  function Night_Sky_Color return Eps.Color;

  function Line_Color return Eps.Color;

  function Line_Size return Eps.Value;

  function Star_Color return Eps.Color;

  function Star_Min return Eps.Value;

  function Star_Max return Eps.Value;

  function Magnitude_Min return Star.Magnitude;

  function Magnitude_Max return Star.Magnitude;

  function Sun_Color return Eps.Color;

  function Sun_Size return Eps.Value;

  function Bright_Moon_Color return Eps.Color;

  function Dark_Moon_Color return Eps.Color;

  function Moon_Size return Eps.Value;

  function Color_Of (The_Planet : Solar.Planet) return Eps.Color;

  function Size_Of (The_Planet : Solar.Planet) return Eps.Value;

end Parameter;
