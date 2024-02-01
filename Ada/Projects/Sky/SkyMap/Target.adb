-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Astro;
with Simbad.Catalog;

package body Target is

  use Astro;
  use Astro.PNULIB;
  use Astro.SPHLIB;

  Pn_Mat  : REAL33;
  Ve      : VECTOR;

  The_Time : Time.Ut;


  procedure Apparent (Ra  : in out REAL;
                      Dec : in out REAL) is
  begin
    APPARENT (Pn_Mat, Ve, Ra, Dec);
  end Apparent;


  procedure Set (Ut : Time.Ut) is
    T : constant Time.T := Time.Tut_Of (Ut);
  begin
    The_Time := Ut;
    PN_MATRIX (Time.T_J2000, T, Pn_Mat);
    ABERRAT (T, Ve);
  end Set;


  function Direction_For (Ra_J2000   : Angle.Degrees;
                          Dec_J2000  : Angle.Degrees;
                          Ra_Motion  : Angle.Degrees;
                          Dec_Motion : Angle.Degrees) return Space.Direction is

    Factor  : constant REAL := Time.T_Second / 36000.0;
    Delta_T : constant REAL := REAL(The_Time) * Factor;
    Ra, Dec : REAL;

  begin
    Ra := Ra_J2000 + Ra_Motion * Delta_T;
    Dec := Dec_J2000 + Dec_Motion * Delta_T;
    Apparent (Ra, Dec);
    return Space.Direction_Of (Dec => Dec,
                               Ra  => Ra);
  end Direction_For;


  function Direction_Of (Item : Simbad.Index) return Space.Direction is
  begin
    return Direction_For (Ra_J2000   => Simbad.Catalog.Ra_J2000_Of (Item),
                          Dec_J2000  => Simbad.Catalog.Dec_J2000_Of (Item),
                          Ra_Motion  => Simbad.Catalog.Ra_Motion_Of (Item),
                          Dec_Motion => Simbad.Catalog.Dec_Motion_Of (Item));
  end Direction_Of;

end Target;
