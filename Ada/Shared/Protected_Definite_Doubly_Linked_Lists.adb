-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Definite_Doubly_Linked_Lists;

package body Protected_Definite_Doubly_Linked_Lists is

  package List is new Definite_Doubly_Linked_Lists (Element);
  The_Event_List : List.Item;

  protected Control is
    entry Next_Event (The_Item : out Element);
    function List_Is_Empty return Boolean;
    procedure List_Append (Item : Element);
    procedure List_Clear;
    procedure List_Clear_And_Append (Item : Element);
  private
    Event_Available : Boolean := False;
  end Control;


  function Is_Empty return Boolean is
  begin
    return Control.List_Is_Empty;
  end Is_Empty;

  procedure Append  (Item : Element) is
  begin
    Control.List_Append (Item);
  end Append;

  procedure Clear is
  begin
    Control.List_Clear;
  end Clear;

  procedure Clear_And_Append (Item : Element) is
  begin
    Control.List_Clear_And_Append (Item);
  end Clear_And_Append;

  function Next return Element is
    The_Result : Element;
  begin
    Control.Next_Event (The_Result);
    return The_Result;
  end Next;


  protected body Control is

    entry Next_Event (The_Item : out Element) when Event_Available is
    begin
      The_Item := The_Event_List.First_Element;
      The_Event_List.Delete_First;
      Event_Available := not List.Is_Empty (The_Event_List);
    end Next_Event;

    function List_Is_Empty return Boolean is
    begin
      return not Event_Available;
    end List_Is_Empty;

    procedure List_Append (Item : Element) is
    begin
      The_Event_List.Append (Item);
      Event_Available := True;
    end List_Append;

    procedure List_Clear is
    begin
      List.Clear (The_Event_List);
      Event_Available := False;
    end List_Clear;

    procedure List_Clear_And_Append (Item : Element) is
    begin
      List.Clear (The_Event_List);
      The_Event_List.Append (Item);
      Event_Available := True;
    end List_Clear_And_Append;

  end Control;


end Protected_Definite_Doubly_Linked_Lists;
