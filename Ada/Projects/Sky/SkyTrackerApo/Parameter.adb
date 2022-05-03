-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Application;
with Configuration;
with Error;
with File;
with Language;
with Stellarium;
with Strings;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

  Localization_Id : constant String := "Localization";
  Language_Key    : constant String := "Language";

  Ten_Micron_Id  : constant String := "10micron";
  Stellarium_Id  : constant String := "Stellarium";
  Ip_Address_Key : constant String := "IP Address";
  Port_Key       : constant String := "Port";
  Program_Key    : constant String := "Program";

  The_Section : Configuration.Section_Handle;

  -- 10micron
  The_10_Micron_Ip_Address : Network.Ip_Address;
  The_10_Micron_Port       : Network.Port_Number;

  -- Stellarium
  The_Stellarium_Port : Network.Port_Number;


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


  function Image_Of (Item : String;
                     Unit : String := "") return String is
  begin
    if Unit /= "" then
      if Item(Item'last - Unit'length + 1 .. Item'last) /= Unit then
        raise Error.Occurred;
      end if;
    end if;
    return Item(Item'first .. Item'last - Unit'length);
  end Image_Of;


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


  function Value_Of (Key  : String;
                     Unit : String := "") return Integer is
    Item : constant String := String_Of (Key);
  begin
    return Integer'value(Image_Of(Item, Unit));
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
      Put ("[" & Ten_Micron_Id & "]");
      Put (Ip_Address_Key & " = 192.168.26.180");
      Put (Port_Key & "       = 3490");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "    = 10001");
      Put (Program_Key & " = C:\Program Files\Stellarium\Stellarium.exe");
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
      Ten_Micron_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Ten_Micron_Id);
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

      Set (Ten_Micron_Handle);
      declare
        IP_Address_Image : constant String := String_Of (Ip_Address_Key);
      begin
        The_10_Micron_Ip_Address := Network.Ip_Address_Of (IP_Address_Image);
        Log.Write ("10micron IP Address: " & IP_Address_Image);
      exception
      when others =>
        Error.Raise_With ("Incorrect 10micron IP Address: " & IP_Address_Image);
      end;
      begin
        The_10_Micron_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("10micron Port:" & The_10_Micron_Port'img);
      exception
      when others =>
        Error.Raise_With ("10micron port number out of range");
      end;

      Set (Stellarium_Handle);
      begin
        The_Stellarium_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Stellarium Port:" & The_Stellarium_Port'img);
      exception
      when others =>
        Error.Raise_With ("Stellarium port number out of range");
      end;
      Startup_Stellarium;
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  --------------
  -- 10micron --
  --------------

  function Ten_Micron_Ip_Address return Network.Ip_Address is
  begin
    return The_10_Micron_Ip_Address;
  end Ten_Micron_Ip_Address;


  function Ten_Micron_Port return Network.Port_Number is
  begin
    return The_10_Micron_Port;
  end Ten_Micron_Port;


  ----------------
  -- Stellarium --
  ----------------

  function Stellarium_Port return Network.Port_Number is
  begin
    return The_Stellarium_Port;
  end Stellarium_Port;

end Parameter;
