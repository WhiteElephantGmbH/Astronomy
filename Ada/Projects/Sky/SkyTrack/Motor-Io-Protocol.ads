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

private package Motor.Io.Protocol is

  procedure Do_Connect;

  function Connected_Device_Version return Hardware_Version;

  procedure Do_Open_Communication;

  procedure Initialize (C0_1 : Natural;
                        C0_2 : Natural);

  function Actual_Device_State return Device.State;

  function Actual_Device_Synch_State return Device.Time_Synch_State;

  procedure Define_Positions (The_Positions : Step_Positions);

  procedure Update_Positions (Offsets : Step_Positions);

  function Position_Known return Boolean;

  function Stepper_Data return Step_Information;

  function Hardware_Board_Temperature return Celsius;

  procedure Do_Synchronize_Time (The_Time : out Time.Ut);

  procedure Transfer (M1 : Action_List := No_Actions;
                      M2 : Action_List := No_Actions);

  procedure Do_Adjust (The_Drive        : Device.Drive;
                       Steps_Per_Update : Step_Count);

  procedure Do_Stop;

  procedure Do_Close_Communication;

end Motor.Io.Protocol;
