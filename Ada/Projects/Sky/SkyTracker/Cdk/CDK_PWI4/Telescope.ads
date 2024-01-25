-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

  type Command is (Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   End_Command,
                   Spiral_Offset_Center,
                   Spiral_Offset_Next,
                   Spiral_Offset_Previous,
                   Add_Point,
                   Next_Speed,
                   Previous_Speed,
                   Rotate);

  subtype Adjust is Command range Move_Left .. Add_Point;

  subtype Setup is Command range Next_Speed .. Rotate;

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

  subtype Startup_State is State range Unknown .. Homing;

  type Time_Delta is delta 0.00001 range -100.0 .. 100.0;

  type Mount_Data is record
    Axis0        : Device.Degrees;
    Axis1        : Device.Degrees;
    Model_Points : Device.Points;
  end record;

  type M3_Data is record
    Exists   : Boolean;
    Position : Device.M3.Position;
  end record;

  type Focuser_Data is record
    Exists       : Boolean;
    Moving       : Boolean;
    Max_Position : Device.Microns;
    Position     : Device.Microns;
  end record;

  type Rotator_Data is record
    Moving        : Boolean;
    Slewing       : Boolean;
    Mech_Position : Device.Degrees;
    Field_Angle   : Device.Degrees;
  end record;

  type Data is record
    Status           : State;
    Target_Lost      : Boolean;
    Actual_Direction : Space.Direction;
    Moving_Speed     : Angle.Value;
    Mount            : Mount_Data;
    M3               : M3_Data;
    Focuser          : Focuser_Data;
    Rotator          : Rotator_Data;
    Completion_Time  : Time.Ut;
  end record;

  type Information_Update_Handler is access procedure;

  procedure Start (Update_Handler : Information_Update_Handler);

  type Get_Space_Access is new Name.Get_Space_Access;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id);

  procedure Define_Park_Position (The_Position : Earth.Direction);

  function Park_Position_Defined return Boolean;

  procedure Define_Max_Focuser_Position (The_Position : Device.Microns);

  procedure Execute (The_Command : Command);

  procedure Startup;

  procedure Shutdown;

  procedure Halt;

  procedure Back;

  procedure Follow (Tracking_Period : Time.Period);

  procedure Position_To (Landmark : Name.Id);

  procedure Focuser_Goto (Position : Device.Microns);

  procedure Rotator_Goto_Field_Angle (Item : Device.Degrees);

  procedure Rotator_Goto_Mech_Position (Item : Device.Degrees);

  procedure Rotator_Goto_Offset (Item : Device.Degrees);

  function Information return Data;

  procedure Close;

end Telescope;
