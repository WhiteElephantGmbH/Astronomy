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

package PWI4.Rotator is

  function Index return Device_Index;

  function Moving return Boolean;

  function Slewing return Boolean;

  function Field_Angle return Degrees;

  function Mech_Position return Degrees;

  procedure Connect (Device : Device_Index := Default_Device);

  procedure Disconnect (Device : Device_Index := Default_Device);

  procedure Find_Home (Device : Device_Index := Default_Device);

  function Is_Homed return Boolean;

  procedure Goto_Mech (Position : Degrees;
                       Device   : Device_Index := Default_Device);

  procedure Goto_Field (Position : Degrees;
                        Device   : Device_Index := Default_Device);

  procedure Goto_Offset (Distance : Degrees;
                         Device   : Device_Index := Default_Device);

  procedure Stop (Device : Device_Index := Default_Device);

end PWI4.Rotator;
