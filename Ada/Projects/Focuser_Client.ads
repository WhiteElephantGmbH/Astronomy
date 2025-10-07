-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Celestron.Focuser;
private with Network;

package Focuser_Client is

  package Focuser renames Celestron.Focuser;

  function Server_Exists return Boolean;

  procedure Initialize;

  function Actual_Data return Focuser.Data;

  function Execute (Command : Focuser.Command) return Focuser.Data;

  function Move_To (Position : Focuser.Distance) return Focuser.Data;

  procedure Shutdown;

  Server_Not_Available : exception;

private

  Id : constant String := "Focuser_Client";

  The_Server_Exists  : Boolean := False;
  The_Client_Address : Network.Ip_Address := Network.Ip_Address_Of_Host ("localhost");
  The_Client_Port    : Network.Port_Number := Focuser.Default_Port_Number;
  The_Home_Position  : Focuser.Distance := Focuser.Default_Home_Position;
  The_Backlash       : Focuser.Lash := Focuser.Default_Backlash;

end Focuser_Client;
