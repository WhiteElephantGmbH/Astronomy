-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Definite_Doubly_Linked_Lists;
with Earth;

package Matrix is

  type Offsets is record
    Alt : Angle.Signed := 0;
    Az  : Angle.Signed := 0;
  end record;

  type Data is record
    Alt    : Angle.Unsigned;
    Az     : Angle.Unsigned;
    Offset : Offsets;
  end record;

  package List is new Definite_Doubly_Linked_Lists (Data);

  type Data_Lists is array (Boolean) of List.Item;

  function Is_Available return Boolean;

  procedure Clear;

  procedure Clear (The_Information : in out Data_Lists);

  function Is_Empty (The_Information : Data_Lists) return Boolean;

  function Is_Ready (The_Information : Data_Lists) return Boolean;

  procedure Calculate (The_Information : in out Data_Lists);

  procedure Correct (Alt, Az    : in out Angle.Value;
                     Is_Inverse :        Boolean);

  function Actual_Offset return Earth.Direction;

end Matrix;
