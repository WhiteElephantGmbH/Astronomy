-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Earth;
with Motor;
with Space;
with Time;

package Numerics is

  function Interpolation_Of (Az, Az1, Az2, Alt1, Alt2 : Angle.Value) return Angle.Value;

  function Interpolation_Of (T, T1, T2 : Time.Ut;
                                V1, V2 : Angle.Value) return Angle.Value;

  function Interpolation_Of (T, T1, T2 : Time.Ut;
                                V1, V2 : Angle.Degrees) return Angle.Degrees;


  -------------------------------------------------
  -- Convert eqatorial to horizontal coordinates --
  -------------------------------------------------

  function Direction_Of (Direction : Space.Direction;
                         Lmst      : Time.Value) return Earth.Direction;


  -------------------------------------------------
  -- Convert horizontal to eqatorial coordinates --
  -------------------------------------------------

  function Direction_Of (Direction : Earth.Direction;
                         Ut        : Time.Ut) return Space.Direction;


  ---------------------------------------------
  -- Motor position of eqatorial coordinates --
  ---------------------------------------------

  function Position_Of (Direction : Space.Direction;
                        Rotations : Space.Direction;
                        At_Time   : Time.Ut) return Motor.Position;


  ----------------------------------------------
  -- Motor position of horizontal coordinates --
  ----------------------------------------------

  function Position_Of (Direction : Earth.Direction) return Motor.Position;


  ----------------------------------------------
  -- Horizontal coordinates of motor position
  ----------------------------------------------

  procedure Calculate_Horizontal_Coordinates_For (Data          :     Motor.Position_Data;
                                                  The_Positions : out Earth.Direction;
                                                  The_Offsets   : out Earth.Direction);

end Numerics;
