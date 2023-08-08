-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Gui;

package Keys is

  type Command is (Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   Increase_Time,
                   Decrease_Time,
                   Move_Left_End,
                   Move_Right_End,
                   Move_Up_End,
                   Move_Down_End,
                   Change_Time_End,
                   Increase_Speed,
                   Decrease_Speed,
                   Enter);

  subtype End_Command is Command range Move_Left_End .. Change_Time_End;

  generic
    with procedure Put (The_Command : Command);
  procedure Handler (The_Event    : Gui.Key_Event;
                     The_Key_Code : Gui.Key_Code);

end Keys;
