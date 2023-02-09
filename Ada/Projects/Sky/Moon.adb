-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar;
with Astro;
with Site;
with Traces;

package body Moon is

  package Log is new Traces ("Moon");

  use Astro;
  use MATLIB;

  function Direction_Of (Id : Name.Id := Name.No_Id;
                         UT : Time.Ut) return Space.Direction is

    DEC, RA, R, RSPHI, RCPHI, LMST : REAL;
    G: VECTOR; -- geozentric moon vector
    L: VECTOR; -- Location vector
    T: VECTOR; -- Topocentric Vector
    use all type Angle.Value;
    pragma Unreferenced (Id);

  begin -- Direction_Of
    MOOLIB.MOONEQU (T   => Time.Tet_Of (UT),
                    RA  => RA,
                    DEC => DEC,
                    R   => R);

    G := CART (R, DEC, RA);

    SPHLIB.SITE (PHI   => +Site.Latitude,
                 RCPHI => RCPHI,
                 RSPHI => RSPHI);

    LMST := +Time.Lmst_Of (UT);

    L := [X => RCPHI * CS (LMST),
          Y => RCPHI * SN (LMST),
          Z => RSPHI];

    T := G - L;

    POLAR (T, R, DEC, RA);

    return Space.Direction_Of (Dec => DEC,
                               Ra  => RA);
  end Direction_Of;


  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut) is
    Unused : Angle.Degrees;
  begin
    Get_New_Phase (Around    => Around,
                   Before    => Before,
                   After     => After,
                   Libration => Unused);
  end Get_New_Phase;


  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut;
                           Libration : out Angle.Degrees) is

    use TIMLIB;

    The_Year       : constant Integer := Ada.Calendar.Year (Time.Local_Of (Around));
    First_Lunation : constant Integer := Integer(REAL'truncation(MOOLIB.D1 * REAL(The_Year - 2000) / 100.0));

    The_Lunation : Integer := First_Lunation;
    T_NEW_MOON   : REAL;
    MJD_NEW_MOON : REAL;

    use type Time.Ut;
    use type Angle.Value;

  begin -- Get_New_Phase
    After := Time.Ut'last;
    loop
      T_NEW_MOON := (REAL(The_Lunation) - MOOLIB.D0) / MOOLIB.D1;
      for Unused in 1 .. 6 loop
        MOOLIB.IMPROVE (T_NEW_MOON, Libration);
      end loop;
      MJD_NEW_MOON := 36525.0 * T_NEW_MOON + 51544.5;
      Before := After;
      After := Time.Ut(MJD_NEW_MOON - MJD_OFFSET) * Time.One_Day;
      exit when After > Around;
      The_Lunation := @ + 1;
      if The_Lunation > First_Lunation + 14 then
        raise Program_Error;
      end if;
    end loop;
    Log.Write ("new phase - before " & Time.Image_Of (Before) &
                        " - after " & Time.Image_Of (After) &
                        " - libration " & Angle.Image_Of (+Libration, Show_Signed => True));
  end Get_New_Phase;

end Moon;
