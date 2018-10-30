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

package body Gui.Enumeration_Menu_Of is

  type Item_Access is access Menu_Item'class;

  type Item_List is array (Element) of Item_Access;

  The_Menu : Menu;
  The_Item : Item_List;

  Handle_For : Handler;


  procedure Enable is
  begin
    Gui.Enable (The_Menu);
  end Enable;


  procedure Disable is
  begin
    Gui.Disable (The_Menu);
  end Disable;


  procedure Enable (The_Element : Element) is
  begin
    Gui.Enable (The_Item(The_Element).all);
  end Enable;


  procedure Disable (The_Element : Element) is
  begin
    Gui.Disable (The_Item(The_Element).all);
  end Disable;


  procedure Set (The_Element : Element) is
  begin
    Set (Selection_Menu_Item'class(The_Item(The_Element).all));
  end Set;


  procedure Clear (The_Element : Element) is
  begin
    Clear (Checked_Menu_Item(The_Item(The_Element).all));
  end Clear;


  procedure Define_Element (The_Value : Information) is
    The_Element : constant Element := Element'val(The_Value);
  begin
    Handle_For (The_Element);
  end Define_Element;


  procedure Create (The_Menu_Name : String;
                    The_Handler   : Handler) is
  begin
    The_Menu := Add_Menu (The_Menu_Name);
    Handle_For := The_Handler;
    for The_Element in Element'range loop
      case The_Kind is
      when Checked =>
        The_Item(The_Element) := new Checked_Menu_Item'(Add_Menu_Item (Image_Of(The_Element),
                                                                       The_Menu,
                                                                       Define_Element'access,
                                                                       Element'pos(The_Element)));
      when Plain =>
        The_Item(The_Element) := new Plain_Menu_Item'(Add_Menu_Item (Image_Of(The_Element),
                                                                     The_Menu,
                                                                     Define_Element'access,
                                                                     Element'pos(The_Element)));
      when Radio =>
        The_Item(The_Element) := new Radio_Menu_Item'(Add_Menu_Item (Image_Of(The_Element),
                                                                     The_Menu,
                                                                     Define_Element'access,
                                                                     Element'pos(The_Element)));
      end case;
    end loop;
  end Create;

end Gui.Enumeration_Menu_Of;
