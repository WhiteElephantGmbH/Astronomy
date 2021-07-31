-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Warnings ("H"); -- !!! workaround for GNAT 2020

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
  type Element (<>) is private;

  with function "=" (Left, Right : Element) return Boolean is <>;

package Indefinite_Doubly_Linked_Lists is
  pragma Preelaborate;

  package Private_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type => Element);

  type Item is new Private_Lists.List with private;

  Empty : constant Item;

  function "+" (Data : Element) return Item with Inline;

  function "+" (Left  : Item;
                Right : Element) return Item with Inline;

  function "+" (Left  : Element;
                Right : Item) return Item with Inline;

  function "+" (Left  : Item;
                Right : Item) return Item with Inline;

  function "-" (Left  : Item;
                Right : Element) return Item with Inline;

  function Count (List : Item) return Natural with Inline;

  generic
      with function "<" (Left, Right : Element) return Boolean is <>;
  package Generic_Sorting is

    function Is_Sorted (List : Item) return Boolean;

    procedure Sort (List : in out Item);

    procedure Merge (Target, Source : in out Item);

  end Generic_Sorting;

private

  type Item is new Private_Lists.List with null record;

  Empty : constant Item := (Private_Lists.Empty_List with null record);

end Indefinite_Doubly_Linked_Lists;
