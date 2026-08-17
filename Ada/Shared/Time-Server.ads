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
  Get_Information   : constant String := "get_information";
  Set_Date_Time     : constant String := "set_date_time";
  Synchronize_Mount : constant String := "synchronize_mount";
  Shutdown          : constant String := "shutdown";

  --Respose
  Response_Ok     : constant String := "Ok";
  Response_Failed : constant String := "Failed";

  --Information Fields
  Clock_Set          : constant String := "clock_set";
  Clock_Set_From_Pc  : constant String := "clock_set_from_pc";
  Clock_Synchronized : constant String := "clock_synchronized";
  Clock_Time         : constant String := "clock_time";
  Mount_Connected    : constant String := "mount_connected";
  Mount_Synchronized : constant String := "mount_synchronized";

  type Information is record
    Clock_Set          : Boolean;
    Clock_Set_From_Pc  : Boolean;
    Clock_Synchronized : Boolean;
    Clock_Time         : Time.JD;
    Mount_Connected    : Boolean;
    Mount_Synchronized : Boolean;
  end record;

end Time.Server;
