-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Network.Udp;
with Space;

package Parameter is

  type Udp_Connection is record
    Socket  : Network.Udp.Socket;
    Address : Network.Address;
  end record;


  procedure Read;

  --------------
  -- 10micron --
  --------------

  function Is_Expert_Mode return Boolean;

  function Ten_Micron_Ip_Address return Network.Ip_Address;

  function Ten_Micron_Port return Network.Port_Number;

  ------------
  -- Remote --
  ------------

  function Remote_Configured return Boolean;

  function Telescope_Name return String;

  function Remote_Address return Network.Ip_Address;

  function Remote_Port return Network.Port_Number;

  -----------
  -- Clock --
  -----------

  function Clock_Configured return Boolean;

  function Clock_Socket return Network.Udp.Socket;

  ----------------
  -- Stellarium --
  ----------------

  function Stellarium_Port return Network.Port_Number;

  function Search_Tolerance return Space.Distance;

end Parameter;