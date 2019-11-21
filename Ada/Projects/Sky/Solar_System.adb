-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Lexicon;

package body Solar_System is

  package Log is new Traces ("Solar_System");

  use Astro;

  use SPHLIB, PLALIB, PNULIB, MATLIB, SUNLIB;

  function Direction_Of (Item : Name.Id;
                         Ut   : Time.Ut) return Space.Direction is
    Planet_Name : constant String := Name.Image_Of (Item);
  begin
    declare
      E : constant Lexicon.Word := Lexicon.Word_Of (Planet_Name);
      P : constant PLANET := PLANET'value(E'img);
      T : constant REAL := Time.Tet_Of (Ut);
      LS, BS, RS, L, B , R, XP, YP, ZP, XS, YS, ZS, X, Y, Z, DELTA0, DELT, DEC, RA: REAL;
    begin
      SUN200 (T,LS,BS,RS);

      -- heliozentrische ekliptikale Planetenkoordinaten

      case P is
      when Mercury =>
        MER200 (T,L,B,R);
      when Venus =>
        VEN200 (T,L,B,R);
      when Mars =>
        MAR200 (T,L,B,R);
      when Jupiter =>
        JUP200 (T,L,B,R);
      when Saturn =>
        SAT200 (T,L,B,R);
      when Uranus =>
        URA200 (T,L,B,R);
      when Neptune =>
        NEP200 (T,L,B,R);
      when Pluto =>
        PLU200(T,L,B,R);
      when Sun =>
        L := 0.0;
        B := 0.0;
        R := 0.0;
      when Earth =>
        raise Program_Error;
      end case;

      -- geozentrische ekliptikale PLanetenkoordinaten (retardiert)

      GEOCEN (T, L, B, R, LS, BS, RS, P, Apparent, XP, YP, ZP, XS, YS, ZS, X, Y, Z, DELTA0);

      -- Praezession, aequat. Koord., Nutation

      ECLEQU (T, X, Y, Z);
      NUTEQU (T, X, Y, Z);

      -- Polarkoordinaten

      POLAR (X, Y, Z, DELT, DEC, RA);
      return Space.Direction_Of (Dec => DEC,
                                 Ra  => RA);
    end;
  exception
  when others =>
    Log.Write ("Unknown Planet: " & Planet_Name);
    return Space.Unknown_Direction;
  end Direction_Of;

end Solar_System;
