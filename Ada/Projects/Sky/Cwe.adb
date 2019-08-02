-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Numerics.Discrete_Random;
with Angle;
with Astro;
with Parameter;
with Traces;

package body Cwe is

  package Log is new Traces ("Cwe");

  The_Mode : Mode;

  procedure Set (To : Mode) is
  begin
    The_Mode := To;
  end Set;

  type Degrees is range 0 .. 359;

  package Angle_Values is new Ada.Numerics.Discrete_Random (Degrees);

  Angle_Generator : Angle_Values.Generator;


  The_Random_Angle : Degrees := 0;

  procedure New_Offset is
  begin
    case The_Mode is
    when Off =>
      The_Random_Angle := 0;
    when On =>
      The_Random_Angle := Angle_Values.Random (Angle_Generator);
      Log.Write ("Random Angle:" & The_Random_Angle'img);
    end case;
  end New_Offset;


  function Adjustment return Earth.Direction is
    use Astro.MATLIB;
    Distance     : constant Angle.Degrees := Parameter.Cwe_Distance;
    use type Angle.Value;
    use type Angle.Degrees;
  begin
    case The_Mode is
    when Off =>
      return Earth.Unknown_Direction;
    when On =>
      return Earth.Direction_Of (Alt => +Distance * CS(Angle.Degrees(The_Random_Angle)),
                                 Az  => +Distance * SN(Angle.Degrees(The_Random_Angle)));
    end case;
  end Adjustment;

end Cwe;
