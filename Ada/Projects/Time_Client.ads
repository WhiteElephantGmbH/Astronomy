-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

with Time.Server;
private with Network;

package Time_Client is

  function Actual_Information return Time.Server.Information;

  function Synchronize_Mount return Boolean;

  function Set (Item : Time.Unix_JD) return Boolean;

  procedure Shutdown;

  Server_Not_Available : exception;

private

  Id : constant String := "Time_Client";

  The_Client_Address : Network.Ip_Address := Network.Ip_Address_Of_Host ("localhost");
  The_Client_Port    : Network.Port_Number := Time.Server.Port;

end Time_Client;
