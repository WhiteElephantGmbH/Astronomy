-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Unchecked_Deallocation;
with Log;

package body Gui.Radio_Menu_Of is

  type Item_Access is access Radio_Menu_Item;

  type Item_List is array (Element range <>) of Item_Access;

  type Item_List_Access is access Item_List;

  type Item_Lists is array (Group) of Item_List_Access;

  The_Menu   : Menu;
  The_Images : Images;
  The_Lists  : Item_Lists;

  Handle_For     : Handler;
  The_Group      : Group := Group'first;
  Is_Initialized : Boolean := False;


  procedure Define_Element (The_Value : Information) is
  begin
    Handle_For (The_Group, The_Images(The_Group)(Positive(The_Value + 1)));
  end Define_Element;


  procedure Initialize_Group is
    procedure Dispose is new Ada.Unchecked_Deallocation (Radio_Menu_Item, Item_Access);
    The_Element : Element;
  begin
    for The_Index in 1 .. The_Images(The_Group).Length loop
      The_Element := Element(The_Index) - 1;
      declare
        Item : Item_Access renames The_Lists(The_Group)(The_Element);
        Name : constant String := String'(The_Images(The_Group)(Positive(The_Index)));
      begin
        if Item /= null then
          --Gui.Dispose_Menu_Item (Item.all); -- !!! keep for Windows ?
          Dispose (Item);
        end if;
        Item := new Radio_Menu_Item'(Add_Menu_Item (Name,
                                                    The_Menu,
                                                    Define_Element'access,
                                                    Information(The_Element)));
      end;
    end loop;
    Set(The_Lists(The_Group)(0).all);
    Is_Initialized := True;
  end Initialize_Group;


  procedure Delete_Group is
  begin
    for Item of The_Lists(The_Group).all loop
      Gui.Delete_Menu_Item (Item.all);
    end loop;
  end Delete_Group;


  procedure Create (Menu_Name   : String;
                    Name_Lists  : Images;
                    The_Handler : Handler) is
  begin
    The_Menu := Add_Menu (Menu_Name);
    The_Images := Name_Lists;
    Handle_For := The_Handler;
    for The_Group in Group loop
      Log.Write ("Create Group:" & The_Group'image);
      The_Lists(The_Group) := new Item_List(0 .. Element(The_Images(The_Group).Length) - 1);
    end loop;
    Initialize_Group;
  end Create;


  procedure Set (To : Group) is
  begin
    if Is_Initialized and then To /= The_Group then
      Delete_Group;
      The_Group := To;
      Initialize_Group;
    end if;
  end Set;


  procedure Enable is
  begin
    Gui.Enable (The_Menu);
  end Enable;


  procedure Disable is
  begin
    Gui.Disable (The_Menu);
  end Disable;

end Gui.Radio_Menu_Of;
