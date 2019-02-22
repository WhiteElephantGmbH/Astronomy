-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Name;
with Space;
with Time;

package Telescope is

  type Command is (Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   End_Move,
                   Increase,
                   Decrease,
                   End_Change,
                   Set_Guiding_Rate,
                   Set_Centering_Rate,
                   Set_Finding_Rate,
                   Set_Slewing_Rate);

  type State is (Disconnected, -- no connection to the motor controller
                 Startup,      -- controller startup
                 Ready,        -- controller ready (parameters initialized)
                 Parked,       -- stopped (at park position)
                 Parking,      -- parking
                 Stopped,      -- stopped (not waiting)
                 Stopping,     -- stopping
                 Directing,    -- moves the telescope in a direction
                 Positioning,  -- moves the telescope to a landmark
                 Preparing,    -- moving to the targets rising direction
                 Waiting,      -- stopped at the targets rising direction
                 Approaching,  -- approaching to target
                 Tracking,     -- following the target
                 Adjusting);   -- corrrect the target diretion

  type Orientation is (Correct, Upside_Down, Backwards, Rotated);

  type Data is record
    Status            : State;
    Target_Lost       : Boolean;
    Target_Direction  : Space.Direction;
    Actual_Direction  : Space.Direction;
    Local_Direction   : Earth.Direction;
    Adjustment        : Earth.Direction;
    Alignment_Offsets : Earth.Direction;
    Pole_Offsets      : Earth.Direction;
    Rotations         : Space.Direction;
    System_Error      : Angle.Value;
    Arriving_Time     : Time.Ut;
    Universal_Time    : Time.Ut;
    Time_Adjustment   : Duration;
  end record;

  type Get_Space_Access is access function (Id : Name.Id;
                                            Ut : Time.Ut) return Space.Direction;

  type Information_Update_Handler is access procedure;

  procedure Start (Update_Handler : Information_Update_Handler);

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id);

  procedure Execute (The_Command : Command);

  procedure Set (The_Orientation : Orientation);

  procedure Halt;

  procedure Follow (Arriving_Time : Time.Ut);

  procedure Position_To (Landmark : Name.Id);

  procedure Align;

  procedure Synch_On_Target;

  procedure Synch_Park_Position;

  procedure Park;

  function Information return Data;

  procedure Close;

end Telescope;
