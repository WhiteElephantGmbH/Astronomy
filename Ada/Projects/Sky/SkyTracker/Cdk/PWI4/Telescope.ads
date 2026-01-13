-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

  type State is (Unknown, -- PWI server not available
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
                 Tracking,
                 Focusing,
                 Solving);

  subtype Is_Tracking is State range Following .. Tracking;

  subtype Startup_State is State range Unknown .. Homing;

  type Time_Delta is delta 0.00001 range -100.0 .. 100.0;

  type Mount_Data is record
    Axis0        : Device.Degrees;
    Axis1        : Device.Degrees;
    Model_Points : Device.Points;
  end record;

  subtype M3_Position is Device.M3.Position;

  type M3_Data is record
    Position : M3_Position;
  end record;

  type Focuser_Data is record
    Exists       : Boolean;
    Moving       : Boolean;
    Max_Position : Device.Microns;
    Zoom_Size    : Device.Microns;
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
                                 The_Id        : Name.Id := Name.No_Id);

  procedure Define_Park_Position (The_Position : Earth.Direction);

  function Park_Position_Defined return Boolean;

  procedure Define_Max_Focuser_Position (The_Position : Device.Microns);

  procedure Define_Fucuser_Zoom_Size (The_Size : Device.Microns);

  procedure Evaluate_Focus;

  procedure Startup;

  procedure Shutdown;

  procedure Halt;

  procedure Follow (Tracking_Period : Time.Period);

  procedure Position_To (Landmark : Name.Id);

  function Information return Data;

  procedure Close;

end Telescope;
