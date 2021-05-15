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

with Angle;
with Earth;
with M_Zero;
with Name;
with Space;
with Time;

package Telescope is

  type Moving_Rate is new M_Zero.Moving_Rate;

  type Moving_Direction is new M_Zero.Moving_Direction;

  type State is new M_Zero.State;

  type Data is record
    Status             : State;
    Target_Direction   : Space.Direction;
    Actual_Direction   : Space.Direction;
    Picture_Direction  : Space.Direction;
    Actual_Moving_Rate : Moving_Rate;
    Cone_Error         : Angle.Value := Angle.Zero;
    Pole_Offsets       : Earth.Direction;
    Universal_Time     : Time.Ut;
  end record;

  type Get_Space_Access is access function (Id : Name.Id;
                                            Ut : Time.Ut) return Space.Direction;

  type Information_Update_Handler is access procedure;

  procedure Start (Update_Handler : Information_Update_Handler);

  procedure Start;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id);

  procedure Increase_Moving_Rate;

  procedure Decrease_Moving_Rate;

  procedure Start_Moving (Direction : Moving_Direction);

  procedure Stop_Moving (Direction : Moving_Direction);

  procedure Stop_Moving;

  procedure Initialize;

  procedure Go_To;

  procedure Align;

  procedure Synch;

  function Information return Data;

  function Error_Message return String renames M_Zero.Error_Message;

  procedure Close;

end Telescope;
