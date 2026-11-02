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

with Directory;
with Error;
with File;
with Section;

package body Picture.Parameter is

  Astap_Key      : constant String := "ASTAP";
  Filename_Key   : constant String := "Filename";
  Height_Key     : constant String := "Height";
  Width_Key      : constant String := "Width";

  Processor_Id    : constant String := "Processor";
  Ip_Address_Key  : constant String := Section.Ip_Address_Key;
  Port_Key        : constant String := Section.Port_Key;
  Darks_Count_Key : constant String := "Darks Count";


  function Number_Of_Darks return Darks_Count is
    Minimum : constant Integer := Integer(Darks_Count'first);
    Maximum : constant Integer := Integer(Darks_Count'last);
  begin
    return Darks_Count(Section.Value_Of (Darks_Count_Key, Id, Minimum, Maximum));
  end Number_Of_Darks;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    Astap.Define (Executable => Section.Filename_Of (Astap_Key));
    declare
      Picture_Filename : constant String := Section.String_Of (Filename_Key);
      Folder           : constant String := File.Containing_Directory_Of (Picture_Filename);
    begin
      if not Directory.Exists (Folder) then
        Error.Raise_With ("Unknown picture directory " & Folder);
      end if;
      Define (Default_Name => Picture_Filename,
              Height       => Section.Degrees_Of (Height_Key, Maximum_Heigth),
              Width        => Section.Degrees_Of (Width_Key, Maximum_Width));
    end;
    Section.Set (Configuration.Handle_For (Handle, Processor_Id));
    if Section.Exists then
      Log.Write ("Processing is enabled");
      Processing_Is_Enabled := True;
      The_Processor_Address := Section.Ip_Address_For (Processor_Id);
      The_Processor_Port := Section.Port_For (Processor_Id);
      The_Number_Of_Darks := Number_Of_Darks;
    end if;
  end Define;


  function Default_Astap_Executable return String is
  begin
    return Os.System.Program_Files_Folder & "astap\astap.exe";
  end Default_Astap_Executable;


  procedure Defaults (Put          : access procedure (Item : String);
                      Default_Name : String := Temp_Filename;
                      Height       : String := Default_Height;
                      Width        : String := Default_Width) is
  begin
    Put ("[" & Id & "]");
    Put (Astap_Key & "    = " & Default_Astap_Executable);
    Put (Filename_Key & " = " & Default_Name);
    Put (Height_Key & "   = " & Height & Angle.Degree);
    Put (Width_Key & "    = " & Width & Angle.Degree);
    Put ("");
    Put ("[" & Processor_Id & "]");
    Put (Ip_Address_Key & "  = localhost");
    Put (Port_Key & "        = 11000");
    Put (Darks_Count_Key & " = 20");
  end Defaults;

end Picture.Parameter;
