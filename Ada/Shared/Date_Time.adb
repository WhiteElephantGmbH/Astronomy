-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;

package body Date_Time is

  function Image return String is
  begin
    declare
      Time_Zone : constant Ada.Calendar.Time_Zones.Time_Offset := Ada.Calendar.Time_Zones.UTC_Time_Offset;
    begin
      return Ada.Calendar.Formatting.Image (Date      => Ada.Calendar.Clock,
                                            Time_Zone => Time_Zone);
    end;
  exception
  when others =>
    return "";
  end Image;

end Date_Time;
