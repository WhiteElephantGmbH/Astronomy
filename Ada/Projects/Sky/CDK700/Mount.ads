-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Device;
with Space;

package Mount is

  type State is (Disconnected, Fault, Startup, Ready, Directing, Positioned, Positioning, Terminated);

  type State_Handler_Access is access procedure (The_State : State);

  procedure Connect_Communication;

  procedure Open_Communication (State_Handler : State_Handler_Access);

  procedure Initialize;

  procedure Goto_Target (Direction : Space.Direction);

  procedure Goto_Mark (Direction : Earth.Direction);

  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed);
  -- move drive

  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed);
  -- adjust drive

  procedure Stop;
  -- stoppes the motors

  function Actual_Direction return Space.Direction;

  procedure Close_Communication;

end Mount;
