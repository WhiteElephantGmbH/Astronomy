-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with PWI4.Protocol;

package body PWI4.Focuser is

  function Exists return Boolean is
  begin
    return Protocol.Focuser.Info.Exists;
  end Exists;


  function Connected return Boolean is
  begin
    return Protocol.Focuser.Info.Is_Connected;
  end Connected;


  function Moving return Boolean is
  begin
    return Protocol.Focuser.Info.Is_Moving;
  end Moving;


  function Actual_Position return Microns is
  begin
    return Protocol.Focuser.Info.Position;
  end Actual_Position;


  procedure Execute (Command_Name  : String;
                     Parameters    : Parameter := "";
                     Device_Number : Device_Index := Default_Device) is
  begin
    Execute (Device     => "focuser",
             Command    => Command_Name,
             Parameters => Parameters +
                           (if Device_Number = Default_Device then "" else "index" / Image_Of (Device_Number)));
  end Execute;


  procedure Connect (Device : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "connect",
             Device_Number => Device);
  end Connect;


  procedure Disconnect (Device : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "disconnect",
             Device_Number => Device);
  end Disconnect;


  procedure Find_Home (Device : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "find_home",
             Device_Number => Device);
  end Find_Home;


  procedure Go_To (Position : Microns;
                   Device   : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "goto",
             Parameters    => "target" / Image_Of (Position),
             Device_Number => Device);
  end Go_To;


  procedure Stop (Device : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "stop",
             Device_Number => Device);
  end Stop;

end PWI4.Focuser;
