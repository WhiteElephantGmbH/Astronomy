-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Device;
with Earth;
with Name;
with Space;
with Time;

package Telescope is

  package Fans renames Device.Fans;

  package M3 renames Device.M3;

  package Focuser renames Device.Focuser;

  package Rotator renames Device.Rotator;

  type Command is (Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   End_Move,
                   Decrease_Time,
                   Increase_Time,
                   End_Change,
                   Decrease_Speed,
                   Increase_Speed,
                   Set_Guiding_Rate,
                   Set_Centering_Rate,
                   Set_Finding_Rate,
                   Set_Slewing_Rate);

  subtype Adjust is Command range Move_Left .. End_Change;

  subtype Setup is Command range Decrease_Speed .. Set_Slewing_Rate;

  type State is (Unknown,    -- PWI server not available
                 Restarting, -- restarting CDK 700 PC
                 Disconnected,
                 Disconnecting,
                 Mount_Error,
                 Connecting,
                 Connected,
                 Disabling,
                 Enabling,
                 Enabled,
                 Homing,
                 Synchronised,
                 Initializing,
                 Stopped,
                 Stopping,
                 Parking,
                 Positioning,
                 Positioned,
                 Preparing,
                 Waiting,
                 Approaching,
                 Following,
                 Tracking);

  subtype Is_Tracking is State range Following .. Tracking;

  subtype Startup_State is State range Unknown .. Initializing;

  type Orientation is (Correct, Upside_Down, Backwards, Rotated);

  type Time_Delta is delta 0.00001 range -100.0 .. 100.0;

  type Data is record
    Status                 : State;
    Target_Lost            : Boolean;
    Target_Direction       : Space.Direction;
    Actual_J2000_Direction : Space.Direction;
    Actual_Direction       : Space.Direction;
    Local_Direction        : Earth.Direction;
    Local_Offset           : Earth.Direction;
    Encoder                : Device.Mount.Encoder_Data;
    Moving_Speed           : Angle.Value;
    Fans_State             : Fans.State;
    M3_Position            : M3.Position;
    Focuser_Position       : Device.Microns;
    Completion_Time        : Time.Ut;
    Universal_Time         : Time.Ut;
    Time_Adjustment        : Time_Delta;
  end record;

  type Information_Update_Handler is access procedure;

  procedure Start (Update_Handler : Information_Update_Handler);

  type Get_Space_Access is new Name.Get_Space_Access;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id);

  procedure Define_Park_Position (The_Position : Earth.Direction);

  function Park_Position_Defined return Boolean;

  procedure Execute (The_Command : Command);

  procedure Set (The_Orientation : Orientation);

  procedure Startup;

  procedure Shutdown;

  procedure Halt;

  procedure Back;

  procedure Follow (Tracking_Period : Time.Period);

  procedure Position_To (Landmark : Name.Id);

  function Information return Data;

  procedure Close;

end Telescope;
