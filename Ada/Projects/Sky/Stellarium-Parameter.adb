-- *********************************************************************************************************************
-- *                           (c) 2024 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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

with Os.System;
with Section;
with Targets;

package body Stellarium.Parameter is

  Program_Key           : constant String := "Program";
  Port_Key              : constant String := "Port";
  Remote_Ip_Address_Key : constant String := "Remote Ip Address";
  Remote_Port_Key       : constant String := "Remote Port";
  Search_Tolerance_Key  : constant String := "Search Tolerance";
  Cntl_Goto_Enabled_Key : constant String := "Cntl Goto Enabled";


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    The_Filename := [Section.String_Value_Of (Program_Key)];
    The_Port_Number := Section.Port_For (Id);
    The_Remote_Ip_Address := Section.Ip_Address_For (Id, Remote_Ip_Address_Key);
    The_Remote_Port_Number := Section.Port_For (Id, Remote_Port_Key);
    The_Search_Tolerance := Section.Degrees_Of (Search_Tolerance_Key, Targets.Maximum_Search_Tolerance);
    Cntl_Goto_Is_Enabled := Section.Value_Of (Cntl_Goto_Enabled_Key);
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Program_Key & "           = " & Os.System.Program_Files_Folder & "Stellarium\Stellarium.exe");
    Put (Port_Key & "              = 10001");
    Put (Remote_Ip_Address_Key & " = localhost");
    Put (Remote_Port_Key & "       = 8090");
    Put (Search_Tolerance_Key & "  = 10""");
    Put (Cntl_Goto_Enabled_Key & " = False");
  end Defaults;

end Stellarium.Parameter;
