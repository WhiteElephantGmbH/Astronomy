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

package body Discrete_Set is

  procedure Include (In_Set : in out Set;
                     Item   :        Element) is
  begin
    In_Set.Data(Item) := True;
  end Include;


  procedure Exclude (From_Set : in out Set;
                     Item     :        Element) is
  begin
    From_Set.Data(Item) := False;
  end Exclude;


  function First (In_Set : Set) return Position is
  begin
    return Next (In_Set, Outside);
  end First;


  function Next (In_Set         : Set;
                 After_Position : Position) return Position is
    The_Position : Position := After_Position;
  begin
    while The_Position < Element'pos(Element'last) loop
      The_Position := @ + 1;
      if In_Set.Data(Element'val(The_Position)) then
        return The_Position;
      end if;
    end loop;
    return Outside;
  end Next;


  function Has_Element (In_Set      : Set;
                        At_Position : Position) return Boolean is
  begin
    return At_Position > Outside and then In_Set.Data(Element'val(At_Position));
  end Has_Element;


  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V : Set) is

    L : constant List := From (V);
    Is_First : Boolean := True;

    function Cleaned (Text : String) return String is
    begin
      if Text(Text'first) = ' ' then
        return Text(Text'first + 1 .. Text'last);
      else
        return Text;
      end if;
    end Cleaned;

  begin -- Put_Image
    S.Put ("[");
    for I in L'range loop
      if not Is_First then
        S.Put (", ");
      end if;
      S.Put (Cleaned (L(I)'image));
      Is_First := False;
    end loop;
    S.Put ("]");
  end Put_Image;


  function Last (In_Set : Set) return Position is
  begin
    for The_Element in reverse Element'range loop
      if In_Set.Data(The_Element) then
        return Element'pos(The_Element);
      end if;
    end loop;
    return Outside;
  end Last;


  function "**" (Left, Right : Element) return Set is
    The_Bits : Bits := [others => False];
  begin
    The_Bits(Left .. Right) := [others => True];
    return (Data => The_Bits);
  end "**";


  function "+" (Left  : Set;
                Right : Element) return Set is
    The_Data : Bits := Left.Data;
  begin
    The_Data(Right) := True;
    return (Data => The_Data);
  end "+";


  function "-" (Left  : Set;
                Right : Element) return Set  is
    The_Data : Bits := Left.Data;
  begin
    The_Data(Right) := False;
    return (Data => The_Data);
  end "-";


  function "-" (Left, Right : Set) return Set  is
  begin
    return (Data => Left.Data and (not Right.Data));
  end "-";


  function "and" (Left, Right : Set) return Set is
  begin
    return (Data => Left.Data and Right.Data);
  end "and";


  function "or" (Left, Right : Set) return Set is
  begin
    return (Data => Left.Data or Right.Data);
  end "or";


  function "xor" (Left, Right : Set) return Set is
  begin
    return (Data => Left.Data xor Right.Data);
  end "xor";


  function "not" (The_Set : Set) return Set is
  begin
    return (Data => not The_Set.Data);
  end "not";


  function "<" (The_Element : Element;
                In_Set      : Set) return Boolean is
  begin
    return Has_Element (In_Set, Element'pos(The_Element));
  end "<";


  function "<" (The_Set : Set;
                In_Set  : Set) return Boolean is
  begin
    return (The_Set and In_Set) = The_Set;
  end "<";


  function From (Item : List) return Set is
    The_Set : Set := Empty;
  begin
    for The_Element of Item loop
      Include (The_Set, The_Element);
    end loop;
    return The_Set;
  end From;


  function From (Item : Set) return List is
    The_List : List(1 .. Set'size);
    The_Last : Natural := Natural'first;
  begin
    for The_Element of Item loop
      The_Last := @ + 1;
      The_List(The_Last) := The_Element;
    end loop;
    return The_List(The_List'first .. The_Last);
  end From;

end Discrete_Set;
