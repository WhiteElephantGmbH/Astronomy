-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Text_IO;
with Angle;
with Application;
with Astap;
with Configuration;
with Error;
with File;
with Language;
with Os;
with Picture;
with Stellarium;
with Strings;
with Targets;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

  Localization_Id : constant String := "Localization";
  Language_Key    : constant String := "Language";

  Stellarium_Id        : constant String := "Stellarium";
  Search_Tolerance_Key : constant String := "Search Tolerance";

  M_Zero_Id      : constant String := "M-Zero";
  Ip_Address_Key : constant String := "IP Address";
  Port_Key       : constant String := "Port";
  Program_Key    : constant String := "Program";

  Picture_Id     : constant String := "Picture";
  Astap_Key      : constant String := "ASTAP";
  Filename_Key   : constant String := "Filename";
  Height_Key     : constant String := "Height";
  Width_Key      : constant String := "Width";

  The_Section : Configuration.Section_Handle;

  -- M_Zero
  The_M_Zero_Ip_Address : Network.Ip_Address;
  The_M_Zero_Port       : Network.Port_Number;

  -- Stellarium
  The_Stellarium_Port  : Network.Port_Number;
  The_Search_Tolerance : Space.Distance;


  function Default_Astap_Executable return String is
  begin
    case Os.Family is
    when Os.Osx =>
      return "/Applications/ASTAP.app/Contents/MacOS/astap";
    when Os.Windows =>
      return "C:\Program Files\astap\astap.exe";
    when Os.Linux =>
      return "astap.exe";
    end case;
  end Default_Astap_Executable;


  function Default_Picture_Filename return String is
  begin
    case Os.Family is
    when Os.Osx =>
      return "/Users/" & Os.User_Name & "/Downloads/getframe.php.jpeg";
    when Os.Windows =>
      return "D:\temp\Picture.jpeg";
    when Os.Linux =>
      return "Picture.jpeg";
    end case;
  end Default_Picture_Filename;


  procedure Set (Section : Configuration.Section_Handle) is
  begin
    The_Section := Section;
  end Set;


  function String_Value_Of (Key : String) return String is
  begin
    return Configuration.Value_Of (The_Section, Key);
  exception
  when others =>
    return "";
  end String_Value_Of;


  function String_Of (Key : String) return String is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      Error.Raise_With ("Parameter <" & Key & "> not defined");
    end if;
    return Image;
  end String_Of;


  function Language return Language.Kind is
    Image : constant String := String_Value_Of (Language_Key);
  begin
    if Image = "" then
      Error.Raise_With ("No language defined");
    end if;
    Log.Write ("Language: " & Image);
    begin
      return Standard.Language.Kind'value(Image);
    exception
    when others =>
      Error.Raise_With ("Incorrect " & Language_Key & ": <" & Image & ">");
    end;
  end Language;


  function Degrees_Of (Key     : String;
                       Maximum : Angle.Degrees) return Angle.Degrees is
    Item : constant String := String_Of (Key);
  begin
    Log.Write (Key & ": " & Item);
    return Angle.Degrees_Of (Item, Maximum);
  exception
  when others =>
    Error.Raise_With ("Incorrect value of " & Key & ": <" & Item & ">");
  end Degrees_Of;


  function Filename_Of (Key : String) return String is
    Name : constant String := String_Of (Key);
  begin
    if not File.Exists (Name) then
      Error.Raise_With ("Filename " & Name & " not found for " & Key);
    end if;
    return Name;
  end Filename_Of;


  function Value_Of (Key  : String) return Integer is
    Item : constant String := String_Of (Key);
  begin
    return Integer'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  procedure Read is

    procedure Create_Default_Parameters is

      The_File : Ada.Text_IO.File_Type;

      procedure Put (Line : String) is
      begin
        Ada.Text_IO.Put_Line (The_File, Line);
      end Put;

    begin -- Create_Default_Parameters
      begin
        Ada.Text_IO.Create (The_File, Name => Filename);
      exception
      when others =>
        Error.Raise_With ("Can't create " & Filename);
      end;
      Put (Strings.Bom_8 & "[" & Localization_Id & "]");
      Put (Language_Key & " = " & Strings.Legible_Of (Stellarium.Language'img));
      Put ("");
      Put ("[" & M_Zero_Id & "]");
      Put (Ip_Address_Key & " = 192.168.4.1");
      Put (Port_Key & "       = 4040");
      Put ("");
      Put ("[" & Picture_Id & "]");
      Put (Astap_Key & "    = " & Default_Astap_Executable);
      Put (Filename_Key & " = " & Default_Picture_Filename);
      Put (Height_Key & "   = 2.97" & Angle.Degree);
      Put (Width_Key & "    = 4.46" & Angle.Degree);
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "             = 10001");
      case Os.Family is
      when Os.Windows =>
        Put (Program_Key & "          = C:\Program Files\Stellarium\Stellarium.exe");
      when Os.Osx =>
        Put (Program_Key & "          = /Applications/Stellarium.app/Contents/MacOS/stellarium");
      when Os.Linux =>
        null;
      end case;
      Put (Search_Tolerance_Key & " = 6'");
      Ada.Text_IO.Close (The_File);
    exception
    when Error.Occurred =>
      raise;
    when Item: others =>
      Log.Termination (Item);
      Ada.Text_IO.Delete (The_File);
      Error.Raise_With ("Internal Error - creating default parameters");
    end Create_Default_Parameters;


    procedure Read_Values is

      Handle              : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      M_Zero_Handle      : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, M_Zero_Id);
      Picture_Handle      : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Picture_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);

      procedure Startup_Stellarium is
        Stellarium_Filename : constant String := String_Value_Of (Program_Key);
      begin
        if Stellarium_Filename = "" then
          return;
        end if;
        Log.Write ("Stellarium program file: """ & Stellarium_Filename & """");
        if not File.Exists (Stellarium_Filename) then
          Error.Raise_With ("Stellarium program file """ & Stellarium_Filename & """ not found");
        end if;
        if not Stellarium.Startup (Stellarium_Filename, Stellarium_Port) then
          Error.Raise_With ("Stellarium not started");
        end if;
      end Startup_Stellarium;

    begin -- Read_Values
      Set (Localization_Handle);
      Standard.Language.Define (Language);

      Set (M_Zero_Handle);
      declare
        IP_Address_Image : constant String := String_Of (Ip_Address_Key);
      begin
        The_M_Zero_Ip_Address := Network.Ip_Address_Of (IP_Address_Image);
        Log.Write ("M-Zero IP Address: " & IP_Address_Image);
      exception
      when others =>
        Error.Raise_With ("Incorrect M-Zero IP Address: " & IP_Address_Image);
      end;
      begin
        The_M_Zero_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("M-Zero Port:" & The_M_Zero_Port'img);
      exception
      when others =>
        Error.Raise_With ("M-Zero port number out of range");
      end;

      Set (Picture_Handle);
      Astap.Define (Executable => Filename_Of (Astap_Key));
      Picture.Define (Name   => String_Of (Filename_Key),
                      Height => Degrees_Of (Height_Key, Picture.Maximum_Heigth),
                      Width  => Degrees_Of (Width_Key, Picture.Maximum_Width));

      Set (Stellarium_Handle);
      begin
        The_Stellarium_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Stellarium Port:" & The_Stellarium_Port'img);
      exception
      when others =>
        Error.Raise_With ("Stellarium port number out of range");
      end;
      The_Search_Tolerance := Degrees_Of (Search_Tolerance_Key, Maximum => Targets.Maximum_Search_Tolerance);
      Startup_Stellarium;
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  ------------
  -- M_Zero --
  ------------

  function M_Zero_Ip_Address return Network.Ip_Address is
  begin
    return The_M_Zero_Ip_Address;
  end M_Zero_Ip_Address;


  function M_Zero_Port return Network.Port_Number is
  begin
    return The_M_Zero_Port;
  end M_Zero_Port;


  ----------------
  -- Stellarium --
  ----------------

  function Stellarium_Port return Network.Port_Number is
  begin
    return The_Stellarium_Port;
  end Stellarium_Port;

  function Search_Tolerance return Space.Distance is
  begin
    return The_Search_Tolerance;
  end Search_Tolerance;

end Parameter;
