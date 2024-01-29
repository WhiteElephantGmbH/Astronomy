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

with Ada.Containers.Doubly_Linked_Lists;
with Object;
with Earth;
with Eps;
with Time;

package Star is

  subtype Id is Object.Index;

  subtype Magnitude is Object.Magnitude;

  subtype Parallax is Object.Parallax;

  subtype Spectral_Class is Object.Spectral_Class;

  subtype Direction is Earth.Direction;

  subtype Color_Range is Eps.Color_Class;

  type Colors is array (Color_Range) of Eps.Color;

  type Information is record
    Ident : Id;
    Mag   : Magnitude;
    Class : Spectral_Class;
    Plx   : Parallax;
    Loc   : Direction;
  end record;

  package Stars is new Ada.Containers.Doubly_Linked_Lists (Information);

  subtype List is Stars.List;

  function Data_List return List;

  function Location_Of (Item : Id) return Direction;

  procedure Read (Ut : Time.Ut);

end Star;
