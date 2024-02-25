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

package Input is

  type Command is (Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   End_Command,

                   Spiral_Offset_Center,
                   Spiral_Offset_Next,
                   Spiral_Offset_Previous,

                   Start_Time_Increase,
                   Start_Time_Decrease,
                   End_Time_Change,

                   Add_Point,

                   Next_Speed,
                   Previous_Speed,
                   Rotate,

                   Go_Back,
                   Stop);

  subtype Adjust_Command is Command range Move_Left .. Add_Point;

  subtype Setup_Command is Command range Next_Speed .. Rotate;

  subtype Time_Change is Command range Start_Time_Increase .. End_Time_Change;

  subtype Mount_Command is Command range Move_Left .. Rotate;


  procedure Open (Execute : access procedure (Item : Command));

  type Source is (Handbox, Server);

  procedure Put (The_Command : Command;
                 From        : Source);

  procedure Close;

end Input;
