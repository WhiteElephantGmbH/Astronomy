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

generic
  type Element is (<>);
  The_Kind : Menu_Kind;
  with function Image_Of (The_Element : Element) return String;
package Gui.Enumeration_Menu_Of is

  type Handler is access procedure (The_Element : Element);

  procedure Create (The_Menu_Name : String;
                    The_Handler   : Handler);

  procedure Enable;

  procedure Disable;

  procedure Enable (The_Element : Element);

  procedure Disable (The_Element : Element);

  procedure Set (The_Element : Element);
  -- Precondition: The_Kind must be Gui.Checked or Gui.Radio

  procedure Clear (The_Element : Element);
  -- Precondition: The_Kind must be Gui.Checked

end Gui.Enumeration_Menu_Of;
