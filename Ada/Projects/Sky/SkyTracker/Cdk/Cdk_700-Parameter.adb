-- *********************************************************************************************************************
-- *                               (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Section;

package body Cdk_700.Parameter is

  Id                   : constant String := "Controller";
  Ip_Address_Key       : constant String := Section.Ip_Address_Key;
  Restart_Duration_Key : constant String := "Restart Duration";


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    The_Ip_Address := Section.Ip_Address_For (Id);
    The_Restart_Duration := Section.Duration_Of (Restart_Duration_Key, Upper_Limit => 60.0);
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Ip_Address_Key & "       = 192.168.10.160");
    Put (Restart_Duration_Key & " = 10s");
  end Defaults;

end Cdk_700.Parameter;
