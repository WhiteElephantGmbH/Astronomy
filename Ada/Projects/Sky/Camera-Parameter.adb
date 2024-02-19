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

with Os.System;
with Picture;
with Section;

package body Camera.Parameter is

  Command_Key    : constant String := "Command";
  Parameters_Key : constant String := "Parameters";


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    declare
      Command : constant String := Section.String_Value_Of (Command_Key);
    begin
      if Command /= "" then
        Define (Command    => Section.Filename_Of (Command_Key, Id),
                Parameters => Section.String_Of (Parameters_Key, Id),
                Picture    => Picture.Filename);
      end if;
    end;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Command_Key & "    = " & Os.System.Program_Files_X86_Folder & "digiCamControl\CameraControlCmd.exe");
    Put (Parameters_Key & " = " & "/capturenoaf /filename %picture% /iso 6400 /shutter 10");
  end Defaults;

end Camera.Parameter;
