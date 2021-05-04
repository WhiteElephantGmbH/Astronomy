-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Values is

  function Interpolation_Of (Az, Az1, Az2, Alt1, Alt2 : Angle.Value) return Angle.Value is

    use type Angle.Degrees;

    function Normalized (A : Angle.Degrees) return Angle.Degrees is
    begin
      if A < 0.0 then
        return A + 360.0;
      else
        return A;
      end if;
    end Normalized;

    use type Angle.Value;

    Daz  : constant Angle.Degrees := Normalized (Az2 - Az1);
    Dalt : constant Angle.Degrees := Alt2 - Alt1;

  begin -- Interpolation_Of
    if Daz = 0.0 then
      return Alt1;
    else
      return Alt1 + Dalt * Normalized (Az - Az1) / Daz;
    end if;
  end Interpolation_Of;


  function Interpolation_Of (T, T1, T2 : Time.Ut;
                                V1, V2 : Angle.Value) return Angle.Value is
    use type Angle.Degrees;
    use type Angle.Value;
    DT : constant Angle.Degrees := Angle.Degrees(T2 - T1);
    DV : constant Angle.Degrees := V2 - V1;
  begin
    if DT = 0.0 then
      return V1;
    else
      return V1 + (Angle.Degrees(T - T1) * DV / DT);
    end if;
  end Interpolation_Of;


  function Interpolation_Of (T, T1, T2 : Time.Ut;
                                V1, V2 : Angle.Degrees) return Angle.Degrees is
    use type Angle.Value;
  begin
    return +(Interpolation_Of (T, T1, T2, +V1, +V2));
  end Interpolation_Of;

end Values;
