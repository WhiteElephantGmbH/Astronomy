-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Indefinite_Doubly_Linked_Lists is

  function "+" (Data : Element) return Item is
    The_List : Item;
  begin
    Append (The_List, Data);
    return The_List;
  end "+";

  function "+" (Left  : Item;
                Right : Element) return Item is
    The_List : Item := Left;
  begin
    Append (The_List, Right);
    return The_List;
  end "+";

  function "+" (Left  : Element;
                Right : Item) return Item is
    The_List : Item := Right;
  begin
    Prepend (The_List, Left);
    return The_List;
  end "+";

  function "+" (Left  : Item;
                Right : Item) return Item is
    The_List   : Item := Right;
    The_Source : Item := Left;
  begin
    Splice (Target => The_List,
            Before => Private_Lists.No_Element,
            Source => The_Source);
    return The_List;
  end "+";

  function "-" (Left  : Item;
                Right : Element) return Item is
    The_List   : Item := Left;
    The_Cursor : Private_Lists.Cursor;
  begin
    The_Cursor := Find (The_List, Right);
    Delete (The_List, The_Cursor);
    return The_List;
  end "-";

  function Count (List : Item) return Natural is
  begin
    return Natural(Length (List));
  end Count;

  package body Generic_Sorting is

    package List_Sorting is new Private_Lists.Generic_Sorting ("<");

    function Is_Sorted (List : Item) return Boolean is
    begin
      return List_Sorting.Is_Sorted (Private_Lists.List(List));
    end Is_Sorted;

    procedure Sort (List : in out Item) is
    begin
      List_Sorting.Sort (Private_Lists.List(List));
    end Sort;

    procedure Merge (Target, Source : in out Item) is
    begin
      List_Sorting.Merge (Private_Lists.List(Target), Private_Lists.List(Source));
    end Merge;

  end Generic_Sorting;

end Indefinite_Doubly_Linked_Lists;
