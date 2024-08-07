-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with PWI2;
with Network;

package Parameter is

  Speed_Unit : constant String := "/s";

  procedure Read;

  procedure Shutdown;

  ------------
  -- Device --
  ------------

  function Is_Expert_Mode return Boolean;

  function Is_Simulation_Mode return Boolean;

  function M3_Ocular_Port return PWI2.Port;

  function M3_Camera_Port return PWI2.Port;

  function M3_Default_Place return Device.M3.Place;

  function Turn_Fans_On return Boolean;

  function Pointing_Model return String;

  function Pole_Height return Angle.Value;

  function Is_Azimuthal_Mount return Boolean;

  function Moving_Speeds return Angle.Values; -- in angle / s

  function Cwe_Distance return Angle.Degrees;

  function Time_Adjustment return Duration;

  -----------
  -- Lx200 --
  -----------

  function Lx200_Port return Network.Port_Number;

end Parameter;
