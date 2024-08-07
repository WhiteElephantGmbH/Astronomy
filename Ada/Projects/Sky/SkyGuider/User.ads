-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Error;
with Name;
with Telescope;

package User is

  type Action is (Define_Catalog, Define_Target, Update,
                  Start, Initialize, Stop, Align, Synch, Go_To,
                  Move_Up, Move_Down, Move_Left, Move_Right,
                  End_Move_Up, End_Move_Down, End_Move_Left, End_Move_Right,
                  Increase_Moving_Rate, Decrease_Moving_Rate,
                  Close);

  subtype Start_Moving is Action range Move_Up .. Move_Right;

  subtype Stop_Moving is Action range End_Move_Up .. End_Move_Right;

  subtype Command_Action is Action range Start .. Close;

  subtype Button_Action is Action range Start .. Go_To;

  type Action_Handler is access procedure (The_Action : Action);

  type Setup_Object is (Skyline, Pole_Top, Pole_Left, Pole_Right);

  procedure Show_Error (The_Text : String := Error.Message);

  procedure Show (Information : Telescope.Data);

  procedure Set (The_Target : Name.Id);

  procedure Clear_Target;

  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Action_Handler      : Action_Handler;
                     The_Termination_Handler : not null access procedure);

  procedure Perform_Synch;

  procedure Perform_Goto;

  procedure Perform_Stop;

  procedure Clear_Targets;

  procedure Define (New_Targets : Name.Id_List_Access);

  procedure Update_Targets;

  procedure Enable_Align_On_Picture;

  function In_Setup_Mode return Boolean;

  function Setup_Kind return Setup_Object;

  function Target_Name return String;

  procedure Show_Description (Image : String);

end User;
