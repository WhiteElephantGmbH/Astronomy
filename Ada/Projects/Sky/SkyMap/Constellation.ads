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

with Earth;
with Star;

package Constellation is

  type Item is (Anr, Aql, Aqr, Ara, Ari, Aur, Boo, Cam, Cap, Car, Cas, Cen, Cep, Cet, Cir, Cma, Cmi, Cnc, Col, Crb, Crt,
                Cru, Crv, Cvn, Cyg, Del, Dra, Dor, Equ, Eri, Frm, Gem, Gru, Hyi, Her, Hya, Ind, Lac, Leo, Lep, Lib, Lmi,
                Lup, Lyn, Lyr, Mon, Oct, Oph, Opt, Ori, Pav, Peg, Per, Phe, Pic, Psa, Psc, Pup, Pyx, Ret, Sco, Sge, Sgr,
                Srh, Srt, Tau, Tra, Uma, Umi, Vir, Vol);

  procedure Prepare;

  type List is array (Positive range <>) of Item;

  function Visible return List;

  type Point is record
    Id        : Star.Number;
    Direction : Earth.Direction;
  end record;

  type Line is record
    From : Point;
    To   : Point;
  end record;

  type Lines is array (Positive range <>) of Line;

  function Visible_Lines_Of (The_Item : Item) return Lines;

  function Is_Used (Id : Star.Number) return Boolean;

end Constellation;
