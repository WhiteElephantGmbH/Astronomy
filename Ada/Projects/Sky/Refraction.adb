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

with Astro;
with Traces;

package body Refraction is

  package Log is new Traces ("Refraction");

  T : Celsius     := 10;
  P : Hectopascal := 1019;

  procedure Set (Temperatur : Celsius) is
  begin
    Log.Write ("Temperature =>" & Temperatur'img);
    T := Temperatur;
  end Set;


  procedure Set (Air_Pressure : Hectopascal) is
  begin
    Log.Write ("Air_Pressure =>" & Air_Pressure'img);
    P := Air_Pressure;
  end Set;


  use Astro.MATLIB;

  procedure Correct (Alt : in out Angle.Degrees) is
    R : Angle.Degrees;
    use type Angle.Degrees;
  begin
    if Alt < 0.0 then
      Alt := 0.0;
    end if;
    R := 1.02 * CTN (Alt + 10.3 / (Alt + 5.11)) * (Angle.Degrees(P) / 1010.0) * (283.0 / (273.0 + Angle.Degrees(T)));
    Alt := Alt + (R / 60.0);
    if Alt > 90.0 then
      Alt := 90.0;
    end if;
  end Correct;

end Refraction;
