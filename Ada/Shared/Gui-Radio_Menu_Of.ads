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

with Text;

generic
  type Group is (<>);
package Gui.Radio_Menu_Of is

  type Element is new Natural;

  type Handler is access procedure (From : Group;
                                    Item : String);

  type Images is array (Group) of Text.Vector;

  procedure Create (Menu_Name   : String;
                    Name_Lists  : Images;
                    The_Handler : Handler);
  -- Postcondition: Set to first group

  procedure Set (To : Group);

  procedure Enable;

  procedure Disable;

end Gui.Radio_Menu_Of;
