-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;

generic
  type Element is private;

  with function "=" (Left, Right : Element) return Boolean is <>;

package Definite_Doubly_Linked_Lists is
  pragma Preelaborate;

  package Private_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Element);

  type Item is new Private_Lists.List with private;

  Empty : constant Item;

  subtype Cursor is Private_Lists.Cursor;

  No_Element : constant Cursor := Private_Lists.No_Element;

  function Element_At (Position : Cursor) return Element renames Private_Lists.Element;

  procedure Next (Position : in out Cursor) renames Private_Lists.Next;

  function Next (Position : Cursor) return Cursor renames Private_Lists.Next;

  procedure Previous (Position : in out Cursor) renames Private_Lists.Previous;

  function Previous (Position : Cursor) return Cursor renames Private_Lists.Previous;

  subtype Index is Positive range 1 .. Natural(Ada.Containers.Count_Type'last);

  type Element_List is array (Index range <>) of Element;

  function Elements (The_List : Item) return Element_List;

  function "+" (Data : Element_List) return Item;

  function "+" (Data : Element) return Item;

  function "+" (Left  : Item;
                Right : Element) return Item;

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

end Definite_Doubly_Linked_Lists;
