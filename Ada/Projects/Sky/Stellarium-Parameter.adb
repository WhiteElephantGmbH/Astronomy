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

  Port_Key             : constant String := "Port";
  Program_Key          : constant String := "Program";
  Search_Tolerance_Key : constant String := "Search Tolerance";


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    The_Port_Number := Section.Port_For (Id);
    The_Search_Tolerance := Section.Degrees_Of (Search_Tolerance_Key, Targets.Maximum_Search_Tolerance);
    The_Filename := [Section.String_Value_Of (Program_Key)];
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Port_Key & "             = 10001");
    Put (Program_Key & "          = " & Os.System.Program_Files_Folder & "Stellarium\Stellarium.exe");
    Put (Search_Tolerance_Key & " = 10""");
  end Defaults;

end Stellarium.Parameter;
