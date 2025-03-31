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

package body PWI4.Rotator is

  function Index return Device_Index is
  begin
    return Protocol.Rotator.Info.Index;
  end Index;


  function Moving return Boolean is
  begin
    return Protocol.Rotator.Info.Is_Moving;
  end Moving;


  function Slewing return Boolean is
  begin
    return Protocol.Rotator.Info.Is_Slewing;
  end Slewing;


  function Field_Angle return Degrees is
  begin
    return Protocol.Rotator.Info.Field_Angle;
  end Field_Angle;


  function Mech_Position return Degrees is
  begin
    return Protocol.Rotator.Info.Mech_Position;
  end Mech_Position;


  procedure Execute (Command_Name  : String;
                     Parameters    : Parameter := "";
                     Device_Number : Device_Index := Default_Device) is
  begin
    Execute (Device     => "rotator",
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


  Last_Mech_Position : Degrees := Degrees'last;

  procedure Find_Home (Device : Device_Index := Default_Device) is
  begin
    Last_Mech_Position := Degrees'last;
    Execute (Command_Name  => "find_home",
             Device_Number => Device);
  end Find_Home;


  function Is_Homed return Boolean is
    New_Position : constant Degrees := Mech_Position;
  begin
    if abs (Last_Mech_Position - New_Position) < 0.001 then
      Last_Mech_Position := Degrees'last;
      return True;
    end if;
    Last_Mech_Position := New_Position;
    return False;
  end Is_Homed;


  procedure Goto_Mech (Position : Degrees;
                       Device   : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "goto_mech",
             Parameters    => "degs" / Image_Of (Position),
             Device_Number => Device);
  end Goto_Mech;


  procedure Goto_Field (Position : Degrees;
                        Device   : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "goto_field",
             Parameters    => "degs" / Image_Of (Position),
             Device_Number => Device);
  end Goto_Field;


  procedure Goto_Offset (Distance : Degrees;
                         Device   : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  => "offset",
             Parameters    => "degs" / Image_Of (Distance),
             Device_Number => Device);
  end Goto_Offset;


  procedure Stop (Device : Device_Index := Default_Device) is
  begin
    Execute (Command_Name  =>"stop",
             Device_Number => Device);
  end Stop;

end PWI4.Rotator;
