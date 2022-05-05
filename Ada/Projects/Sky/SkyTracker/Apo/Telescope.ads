-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Space;
with Ten_Micron;
with Time;

package Telescope is

  type State is new Ten_Micron.State;

  type Data is record
    Status             : State := Disconnected;
    Target_Direction   : Space.Direction;
    Actual_Direction   : Space.Direction;
    Universal_Time     : Time.Ut;
  end record;

  type Information_Update_Handler is access procedure;

  procedure Start (Update_Handler : Information_Update_Handler);

  type Get_Space_Access is new Name.Get_Space_Access;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id);
  procedure Go_To;

  procedure Park;

  procedure Stop;

  procedure Unpark;

  function Information return Data;

  procedure Close;

end Telescope;
