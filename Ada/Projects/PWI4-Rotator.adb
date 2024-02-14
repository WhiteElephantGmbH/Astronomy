-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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


  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "rotator",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Find_Home is
  begin
    Execute ("find_home");
  end Find_Home;


  procedure Goto_Mech (Position : Degrees) is
  begin
    Execute (Command_Name => "goto_mech",
             Parameters   => "degs=" & Image_Of (Position));
  end Goto_Mech;


  procedure Goto_Field (Position : Degrees) is
  begin
    Execute (Command_Name => "goto_field",
             Parameters   => "degs=" & Image_Of (Position));
  end Goto_Field;


  procedure Goto_Offset (Distance : Degrees) is
  begin
    Execute (Command_Name => "offset",
             Parameters   => "degs=" & Image_Of (Distance));
  end Goto_Offset;


  procedure Stop is
  begin
    Execute ("stop");
  end Stop;

end PWI4.Rotator;
