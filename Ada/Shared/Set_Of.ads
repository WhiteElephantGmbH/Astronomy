-- *********************************************************************************************************************
-- *                        (c) 2002 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                         *
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
pragma Style_Astronomy;

generic
  type Element is (<>);
package Set_Of is

  Overflow : exception;

  type Set is private;

  type List is array (Positive range <>) of Element;

  type Slice is
    record
      From : Element;
      To   : Element;
    end record;

  function Empty return Set;

  function Is_Empty (The_Set : Set) return Boolean;

  function Full return Set;

  function Is_Full (The_Set : Set) return Boolean;

  function Set_Of (The_Element : Element) return Set;

  function Set_Of (The_List : List) return Set;

  function Set_Of (The_Slice : Slice) return Set;

  function "+" (The_Slice : Slice) return Set renames Set_Of;

  function "+" (The_Element : Element) return Set;

  function "+" (The_Set     : Set;
                And_Set     : Set)     return Set;
  function "+" (The_Element : Element;
                And_Set     : Set)     return Set;
  function "+" (The_Set     : Set;
                And_Element : Element) return Set;
  function "+" (The_Element : Element;
                And_Element : Element) return Set;

  function "-" (The_Set      : Set;
                With_Set     : Set)     return Set;
  function "-" (The_Set      : Set;
                With_Element : Element) return Set;

  function "<" (The_Element : Element;
                In_Set      : Set) return Boolean;

  function "<" (The_Set : Set;
                In_Set  : Set) return Boolean;

private
  type Element_Array is array (Element) of Boolean with Pack;

  type Set is record
    Value: Element_Array := [others => False];
  end record with Pack;

end Set_Of;
