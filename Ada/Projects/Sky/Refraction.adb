-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Refraction is

  P : Hectopascal := Undefined_Air_Pressure;
  T : Celsius     := Undefined_Temperature;

  New_P : Boolean := False;
  New_T : Boolean := False;


  procedure Set (The_Air_Pressure : Hectopascal) is
  begin
    if The_Air_Pressure /= P then
      New_P := True;
    end if;
    P := The_Air_Pressure;
  end Set;


  function New_Air_Pressure return Boolean is
  begin
    if New_P then
      New_P := False;
      return True;
    end if;
    return False;
  end New_Air_Pressure;


  function Air_Pressure return Hectopascal is (P);


  procedure Set (The_Temperature : Celsius) is
  begin
    if The_Temperature /= T then
      New_T := True;
    end if;
    T := The_Temperature;
  end Set;


  function New_Temperature return Boolean is
  begin
    if New_T then
      New_T := False;
      return True;
    end if;
    return False;
  end New_Temperature;


  function Temperature return Celsius is (T);


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
