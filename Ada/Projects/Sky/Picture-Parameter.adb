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
with Section;

package body Picture.Parameter is

  Astap_Key      : constant String := "ASTAP";
  Filename_Key   : constant String := "Filename";
  Height_Key     : constant String := "Height";
  Width_Key      : constant String := "Width";


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    Astap.Define (Executable => Section.Filename_Of (Astap_Key));
    declare
      Picture_Filename : constant String := Section.String_Of (Filename_Key);
    begin
      Define (Name   => Picture_Filename,
              Height => Section.Degrees_Of (Height_Key, Maximum_Heigth),
              Width  => Section.Degrees_Of (Width_Key, Maximum_Width));
    end;
  end Define;


  function Default_Astap_Executable return String is
  begin
    return Os.System.Program_Files_Folder & "astap\astap.exe";
  end Default_Astap_Executable;


  function Default_Picture_Filename return String is
  begin
    return "D:\Picture\Image.CR2";
  end Default_Picture_Filename;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Astap_Key & "    = " & Default_Astap_Executable);
    Put (Filename_Key & " = " & Default_Picture_Filename);
    Put (Height_Key & "   = 0.51" & Angle.Degree);
    Put (Width_Key & "    = 0.74" & Angle.Degree);
  end Defaults;

end Picture.Parameter;
