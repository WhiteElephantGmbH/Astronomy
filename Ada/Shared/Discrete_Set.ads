-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Strings.Text_Buffers;

generic
  type Element is (<>);
package Discrete_Set with Pure is

  Outside : constant := -1;

  type List is array (Positive range <>) of Element;

  subtype Position is Integer range Outside .. Element'pos(Element'last);

  type Set is private
    with Aggregate => (Empty       => Empty,
                       Add_Unnamed => Include),
         Iterable => (First       => First,
                      Next        => Next,
                      Has_Element => Has_Element,
                      Element     => Get_Element),
         Preelaborable_Initialization;

  Empty : constant Set;

  Full : constant Set;

  procedure Include (In_Set : in out Set;
                     Item   :        Element) with Inline;

  procedure Exclude (From_Set : in out Set;
                     Item     :        Element) with Inline;

  function First (In_Set : Set) return Position;

  function Next (In_Set         : Set;
                 After_Position : Position) return Position;

  function Has_Element (In_Set      : Set;
                        At_Position : Position) return Boolean with Inline;

  function Get_Element (Unused      : Set;
                        At_Position : Position) return Element is (Element'val(At_Position)) with Inline;

  function Last (In_Set : Set) return Position;

  function "**" (Left, Right : Element) return Set with Inline; -- a range of elements

  function "+" (Left  : Set;
                Right : Element) return Set with Inline;

  function "-" (Left  : Set;
                Right : Element) return Set with Inline;

  function "-" (Left, Right : Set) return Set with Inline;

  function "and" (Left, Right : Set) return Set with Inline;

  function "or" (Left, Right : Set) return Set with Inline; -- use instead of +

  function "xor" (Left, Right : Set) return Set with Inline;

  function "not" (The_Set : Set) return Set with Inline;

  function "<" (The_Element : Element;
                In_Set      : Set) return Boolean with Inline;

  function "<" (The_Set : Set;
                In_Set  : Set) return Boolean with Inline;

  function From (Item : List) return Set;

  function From (Item : Set) return List;

private
  type Bits is array (Element) of Boolean with Pack;

  type Set is record
    Data : Bits;
  end record
  with Pack, Suppress_Initialization, Put_Image => Put_Image;

  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V : Set);

  Empty : constant Set := (Data => [others => False]);

  Full : constant Set := (Data => [others => True]);

end Discrete_Set;
