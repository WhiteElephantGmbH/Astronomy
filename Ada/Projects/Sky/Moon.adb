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
with Astro;
with Parameter;

package body Moon is

  use Astro;
  use MATLIB;
  use MOOLIB;
  use SPHLIB;

  function Direction_Of (Id : Name.Id;
                         UT : Time.Ut) return Space.Direction is

    DEC, RA, R, RSPHI, RCPHI, LMST : REAL;
    G: VECTOR; -- geozentric moon vector
    L: VECTOR; -- Location vector
    T: VECTOR; -- Topocentric Vector
    use all type Angle.Value;
    pragma Unreferenced (Id);

  begin
    MOONEQU (T   => Time.Tet_Of (UT),
             RA  => RA,
             DEC => DEC,
             R   => R);

    G := CART (R, DEC, RA);

    SITE (PHI   => +Parameter.Latitude,
          RCPHI => RCPHI,
          RSPHI => RSPHI);

    LMST := +Time.Lmst_Of (UT);

    L := (X => RCPHI * CS (LMST),
          Y => RCPHI * SN (LMST),
          Z => RSPHI);

    T := G - L;

    POLAR (T, R, DEC, RA);

    return Space.Direction_Of (Dec => DEC,
                               Ra  => RA);
  end Direction_Of;

end Moon;
