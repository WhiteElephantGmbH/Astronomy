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

package body Set_Of is

  function Empty return Set is
  begin
    return Set'(Value => [others => False]);
  end Empty;


  function Is_Empty (The_Set : Set) return Boolean is
  begin
    return The_Set = Empty;
  end Is_Empty;


  function Full return Set is
  begin
    return Set'(Value => [others => True]);
  end Full;


  function Is_Full (The_Set : Set) return Boolean is
  begin
    return The_Set = Full;
  end Is_Full;


  function Set_Of (The_Element : Element) return Set is
    The_Set : Set;
  begin
    The_Set.Value(The_Element) := True;
    return The_Set;
  end Set_Of;


  function Set_Of (The_List : List) return Set is
    The_Set : Set;
  begin
    for The_Index in The_List'range loop
      The_Set.Value(The_List(The_Index)) := True;
    end loop;
    return The_Set;
  end Set_Of;


  function Set_Of (The_Slice : Slice) return Set is
    The_Set : Set;
  begin
    for The_Index in The_Slice.From .. The_Slice.To loop
      The_Set.Value(The_Index) := True;
    end loop;
    return The_Set;
  end Set_Of;


  function "+" (The_Element : Element) return Set is
    The_Set : Set;
  begin
    The_Set.Value(The_Element) := True;
    return The_Set;
  end "+";


  function "+" (The_Set : Set;
                And_Set : Set) return Set is
  begin
    return Set'(Value => The_Set.Value or And_Set.Value);
  end "+";


  function "+" (The_Element : Element;
                And_Set     : Set) return Set is
  begin
    return +The_Element + And_Set;
  end "+";


  function "+" (The_Set     : Set;
                And_Element : Element) return Set is
  begin
    return And_Element + The_Set;
  end "+";


  function "+" (The_Element : Element;
                And_Element : Element) return Set is
  begin
    return +The_Element + And_Element;
  end "+";


  function "-" (The_Set  : Set;
                With_Set : Set) return Set is
  begin
    return Set'(Value => The_Set.Value and (not With_Set.Value));
  end "-";


  function "-" (The_Set      : Set;
                With_Element : Element) return Set is
  begin
    return The_Set - Set_Of (With_Element);
  end "-";


  function "<" (The_Element : Element;
                In_Set      : Set) return Boolean is
  begin
    return In_Set.Value(The_Element);
  end "<";


  function "<" (The_Set : Set;
                In_Set  : Set) return Boolean is
  begin
    return (The_Set.Value and In_Set.Value) = The_Set.Value;
  end "<";


end Set_Of;
