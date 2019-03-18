-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

private package Motor.Io.Protocol.Serial is

  procedure Connect_Device;

  procedure Start;

  function Actual_Stepper_State return Device.State;

  function Step_Position_Known return Boolean;

  function Actual_Step_Data return Step_Information;

  procedure Set_Initial_Count (C0_1 : Natural;
                               C0_2 : Natural);

  procedure Set_Step_Positions (P : Step_Positions);

  procedure Update_Step_Positions (Offsets : Step_Positions);

  procedure Synchronize_Start_Time (The_Time : out Time.Ut);

  procedure Transfer_Actions (M1 : Action_List := No_Actions;
                              M2 : Action_List := No_Actions);

  procedure Adjust (The_Drive         : Device.Drive;
                    Offset_Per_Action : Step_Count);

  procedure Stop_All;

  procedure Finish;

end Motor.Io.Protocol.Serial;
