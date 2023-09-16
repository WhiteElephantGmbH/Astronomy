-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Name;
with Telescope;

package User is

  type Action is (Define_Catalog, Define_Target, Update,
                  Go_To, Go_To_Left, Go_To_Right, Go_To_Top, Go_To_Next, Align, Park, Stop, Unpark,
                  Close);

  subtype Command_Action is Action range Go_To .. Close;

  subtype Button_Action is Action range Go_To .. Unpark;

  type Action_Handler is access procedure (The_Action : Action);

  type Setup_Object is (Pole_Top, Pole_Left, Pole_Right, Align_Stars);

  procedure Show (Visible_In : Duration);

  procedure Show (Information : Telescope.Data);

  procedure Set (The_Target : Name.Id);

  procedure Clear_Target;

  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Action_Handler      : Action_Handler;
                     The_Termination_Handler : not null access procedure);

  procedure Perform_Goto;

  procedure Perform_Goto_Next;

  procedure Clear_Targets;

  procedure Define (New_Targets : Name.Id_List_Access);

  procedure Update_Targets;
  -- PRECONDITION: Targets defined

  procedure Enable_Align_On_Picture;

  function In_Setup_Mode return Boolean;

  function Target_Name return String;

  procedure Show_Description (Image : String);

  procedure Show_Error (Image : String);

end User;
