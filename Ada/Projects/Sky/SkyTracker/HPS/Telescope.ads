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

with Alignment;
with Angle;
with Earth;
with Name;
with Space;
with Ten_Micron;
with Time;

package Telescope is

  subtype State is Ten_Micron.State;

  subtype Error_State is Ten_Micron.Error_State;

  subtype Transit_State is Ten_Micron.Transit_State;

  use all type State;

  subtype Command is Ten_Micron.Command;

  type Update_Command is (Start_Time_Increase, Start_Time_Decrease, End_Time_Change);

  subtype Time_Offset is Ten_Micron.Time_Offset;

  function Image_Of (Time_Delta : Time_Offset) return String renames Ten_Micron.Image_Of;

  type Data is record
    Status             : State := Disconnected;
    Target_Direction   : Space.Direction;
    Actual_Direction   : Space.Direction;
    Actual_Position    : Space.Direction;
    Picture_Direction  : Space.Direction;
    Mount_Pier_Side    : Character := Ten_Micron.Undefined_Pier;
    Align_Points       : Natural := 0;
    Alignment_Info     : Alignment.Information;
    Cone_Error         : Angle.Value := Angle.Zero;
    Pole_Offsets       : Earth.Direction;
    Universal_Time     : Time.Ut := Time.Unknown;
    Time_Delta         : Time_Offset;
  end record;

  type Information_Update_Handler is access procedure;

  procedure Start (Update_Handler : Information_Update_Handler);

  type Get_Space_Access is new Name.Get_Space_Access;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id);

  procedure Align;

  procedure Go_To;

  procedure Go_To_Left;

  procedure Go_To_Right;

  procedure Go_To_Top;

  procedure Go_To_Next;

  procedure Prepare_Tle;

  procedure Park;

  procedure Stop;

  procedure Unpark;

  procedure Execute (The_Command : Command);

  procedure Update (The_Command : Update_Command);

  function Information return Data;

  procedure Close;

end Telescope;
