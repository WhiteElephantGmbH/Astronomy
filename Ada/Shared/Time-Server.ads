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

package Time.Server is

  Port : constant := 10000;

  --Commands
  Shutdown          : constant String := "shutdown";
  Synchronize_Mount : constant String := "synchronize_mount";
  Get_Information   : constant String := "get_information";

  --Respose
  Response_Ok     : constant String := "Ok";
  Response_Failed : constant String := "Failed";

  --Information Fields
  Clock_Exists          : constant String := "clock_exists";
  Clock_Synchronized    : constant String := "clock_synchronized";
  Date_Time             : constant String := "date_time";
  Mount_Connected       : constant String := "mount_connected";
  Mount_Synchronized    : constant String := "mount_synchronized";

end Time.Server;
