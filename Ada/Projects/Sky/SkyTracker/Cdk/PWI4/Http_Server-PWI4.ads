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

with Angle;

package Http_Server.PWI4 is

  type Degrees is delta 0.00001 range -999.0 .. 999.0;

  subtype Distance is Integer;

  type Mount_Data is record
    Exists       : Boolean := False;
    Axis0        : Degrees := 0.0;
    Axis1        : Degrees := 0.0;
    Model_Points : Natural := 0;
  end record;

  type Mirror_Position is (Unknown, Between, Ocular, Camera);

  type Focuser_Data is record
    Exists       : Boolean := False;
    Moving       : Boolean := False;
    Max_Position : Distance := 0;
    Zoom_Size    : Distance := 0;
    Position     : Distance := 0;
    Set_Position : access procedure (Item : Distance);
  end record;

  type Rotator_Data is record
    Exists             : Boolean := False;
    Moving             : Boolean := False;
    Slewing            : Boolean := False;
    Mech_Position      : Degrees := 0.0;
    Field_Angle        : Degrees := 0.0;
    Goto_Field_Angle   : access procedure (Item : Degrees);
    Goto_Mech_Position : access procedure (Item : Degrees);
    Goto_Offset        : access procedure (Item : Degrees);
  end record;

  procedure Start;

  procedure Set_State (Image : String);

  procedure Set_Moving (Speed : Angle.Value); -- per second

  procedure Set (Data : Mount_Data);

  procedure Set (Position : Mirror_Position);

  procedure Set (Data : Focuser_Data);

  procedure Set (Data : Rotator_Data);

end Http_Server.PWI4;
