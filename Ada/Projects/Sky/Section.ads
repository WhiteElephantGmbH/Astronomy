-- *********************************************************************************************************************
-- *                       (c) 2024 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Angle;
with Configuration;
with Earth;
with Network;

package Section is

  use type Angle.Value;

  package Angles is new Ada.Containers.Doubly_Linked_Lists (Angle.Value);

  Ip_Address_Key  : constant String := "IP Address";
  Port_Key        : constant String := "Port";

  procedure Set (Handle : Configuration.Section_Handle);

  function Exists return Boolean;

  function Angles_Of (Key     : String;
                      Maximum : Natural; -- in degrees
                      Unit    : String := "") return Angles.List;

  function Degrees_Of (Key     : String;
                       Maximum : Angle.Degrees;
                       Minimum : Angle.Degrees := 0.0) return Angle.Degrees;

  function Direction_Of (Key : String) return Earth.Direction;

  function Duration_Of (Key         : String;
                        Lower_Limit : Duration := 0.0;
                        Upper_Limit : Duration) return Duration;

  function Filename_Of (Key  : String;
                        Name : String := "") return String;

  function Image_Of (Item : String;
                     Unit : String := "") return String;

  function Ip_Address_For (Name : String) return Network.Ip_Address;

  function Port_For (Name : String) return Network.Port_Number;

  function String_Of (Key  : String;
                      Name : String := "") return String;

  function String_Value_Of (Key : String) return String;

  function Value_Of (Key  : String;
                     Name : String := "") return Integer;

end Section;
