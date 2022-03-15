-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with PWI;
with Network;
with Space;

package Parameter is

  Speed_Unit : constant String := "/s";

  procedure Read;

  procedure Shutdown;

  ------------
  -- Device --
  ------------

  function Is_Expert_Mode return Boolean;

  function Is_Simulation_Mode return Boolean;

  function M3_Ocular_Port return PWI.Port;

  function M3_Camera_Port return PWI.Port;

  function M3_Default_Place return Device.M3.Place;

  function Turn_Fans_On return Boolean;

  function Pointing_Model return String;

  function Pole_Height return Angle.Value;

  function Is_Azimuthal_Mount return Boolean;

  function Moving_Speeds return Angle.Values; -- in angle / s

  function Cwe_Distance return Angle.Degrees;

  -----------
  -- Lx200 --
  -----------

  function Lx200_Port return Network.Port_Number;

  ------------
  -- Remote --
  ------------

  function Remote_Configured return Boolean;

  function Telescope_Name return String;

  function Remote_Address return Network.Ip_Address;

  function Remote_Port return Network.Port_Number;
  
  ----------------
  -- Stellarium --
  ----------------

  function Stellarium_Port return Network.Port_Number;

  function Search_Tolerance return Space.Distance;

end Parameter;
