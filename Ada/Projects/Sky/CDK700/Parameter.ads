-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Network;

package Parameter is

  Speed_Unit : constant String := "/s";

  procedure Read;


  ----------
  -- Site --
  ----------

  function Latitude return Angle.Value;

  function Longitude return Angle.Value;

  function Altitude return Integer; -- in meters above see level


  ------------
  -- Device --
  ------------

  function Telescope_Name return String;

  function Is_Expert_Mode return Boolean;

  function Is_Simulation_Mode return Boolean;

  function Turn_Fans_On return Boolean;

  function Pointing_Model return String;

  function Pole_Height return Angle.Value;

  function Is_Azimuthal_Mount return Boolean;

  function Moving_Speeds return Angle.Values; -- in angle / s


  -------------
  -- Servers --
  -------------

  function Lx200_Port return Network.Port_Number;

  function Stellarium_Port return Network.Port_Number;

end Parameter;
