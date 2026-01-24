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
pragma Style_White_Elephant;

with Error;
with File;
with Os.System;
with Section;
with Targets;

package body Stellarium.Parameter is

  Magnitude_Key        : constant String := "Magnitude";
  Port_Key             : constant String := "Port";
  Program_Key          : constant String := "Program";
  Satellite_Group_Key  : constant String := "Satellite Group";
  Search_Tolerance_Key : constant String := "Search Tolerance";


  procedure Define (Handle          : Configuration.File_Handle;
                    With_Satellites : Boolean := True) is

    procedure Startup_Stellarium is
      Stellarium_Filename : constant String := Section.String_Value_Of (Program_Key);
    begin
      if Stellarium_Filename = "" then
        return;
      end if;
      Log.Write ("Stellarium program file: """ & Stellarium_Filename & """");
      if not File.Exists (Stellarium_Filename) then
        Error.Raise_With ("Stellarium program file """ & Stellarium_Filename & """ not found");
      end if;
      if not Startup (Stellarium_Filename, Port_Number) then
        Error.Raise_With ("Stellarium not started");
      end if;
    end Startup_Stellarium;

  begin -- Define
    Section.Set (Configuration.Handle_For (Handle, Id));
    The_Port_Number := Section.Port_For (Id);
    The_Search_Tolerance := Section.Degrees_Of (Search_Tolerance_Key, Targets.Maximum_Search_Tolerance);
    Has_Satellites := With_Satellites;
    if Has_Satellites then
      declare
        Image         : constant String := Section.String_Of (Magnitude_Key, Id);
        The_Magnitude : Stellarium.Magnitude;
      begin
        The_Magnitude := Stellarium.Magnitude'value(Image);
        Log.Write ("Magnitude Maximum:" & The_Magnitude'image);
        Stellarium.Set_Maximum (The_Magnitude);
      exception
      when others =>
        Error.Raise_With ("Magnitude out of range");
      end;
      Stellarium.Set_Satellite_Group (Section.String_Of (Satellite_Group_Key, Id));
    end if;
    Startup_Stellarium;
  end Define;


  procedure Defaults (Put        : access procedure (Item : String);
                      Satellites : String := "") is
  begin
    Put ("[" & Id & "]");
    Put (Port_Key & "             = 10001");
    Put (Program_Key & "          = " & Os.System.Program_Files_Folder & "Stellarium\Stellarium.exe");
    Put (Search_Tolerance_Key & " = 10""");
    if Satellites /= "" then
      Put (Magnitude_Key & "        = 8.0");
      Put (Satellite_Group_Key & "  = " & Satellites);
    end if;
  end Defaults;

end Stellarium.Parameter;
