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
with Angle;
with Application;
with Configuration;
with Error;
with File;
with Language;
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
  Program_Key          : constant String := "Program";
  Search_Tolerance_Key : constant String := "Search Tolerance";

  Ten_Micron_Id   : constant String := "10micron";
  Expert_Mode_Key : constant String := "Expert Mode";
  Remote_Id       : constant String := "Remote";
  Telescope_Key   : constant String := "Telescope";
  Ip_Address_Key  : constant String := "IP Address";
  Port_Key        : constant String := "Port";

  The_Section : Configuration.Section_Handle;

  -- 10micron
  Is_In_Expert_Mode        : Boolean;
  The_10_Micron_Ip_Address : Network.Ip_Address;
  The_10_Micron_Port       : Network.Port_Number;

  --Remote
  The_Telescope_Name : Strings.Element;
  The_Remote_Address : Network.Ip_Address;
  The_Remote_Port    : Network.Port_Number;

  -- Stellarium
  The_Stellarium_Port  : Network.Port_Number;
  The_Search_Tolerance : Space.Distance;


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


  function String_Of (Key     : String;
                      Section : String := "") return String is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      Error.Raise_With ("Parameter <" & Key & (if Section = "" then "" else "> for <") & Section & "> not defined");
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


  function Value_Of (Key     : String;
                      Section : String := "") return Integer is
    Item : constant String := String_Of (Key);
  begin
    return Integer'value(Image_Of(Item));
  exception
  when others =>
    Error.Raise_With ("Incorrect " & (if Section = "" then "" else Section & " ") & Key & ": <" & Item & ">");
  end Value_Of;


  function Ip_Address_For (Section : String) return Network.Ip_Address is
    Server      : constant String := String_Of (Ip_Address_Key, Section);
    The_Address : Network.Ip_Address;
  begin
    begin
      The_Address := Network.Ip_Address_Of (Server);
    exception
    when others =>
      The_Address := Network.Ip_Address_Of_Host (Server);
    end;
    Log.Write (Section & " " & Ip_Address_Key & ": " & Network.Image_Of (The_Address));
    return The_Address;
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Section & " IP address " & Server);
  end Ip_Address_For;


  function Port_For (Section : String) return Network.Port_Number is
    Value : constant Integer := Value_Of (Port_Key, Remote_Id);
  begin
    Log.Write (Section & " port number:" & Value'image);
    return Network.Port_Number (Value);
  exception
  when others =>
    Error.Raise_With (Section & " port number" & Value'image & " out of range");
  end Port_For;


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
      Put (Expert_Mode_Key & " = False");
      Put (Ip_Address_Key & "  = 192.168.26.180");
      Put (Port_Key & "        = 3490");
      Put ("");
      Put ("[" & Remote_Id & "]");
      Put (Telescope_Key & "  = apo");
      Put (Ip_Address_Key & " = 217.160.64.198");
      Put (Port_Key & "       = 5000");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "             = 10001");
      Put (Program_Key & "          = C:\Program Files\Stellarium\Stellarium.exe");
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
      Ten_Micron_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Ten_Micron_Id);
      Remote_Handle       : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Remote_Id);
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
      Is_In_Expert_Mode := Strings.Is_Equal (String_Value_Of (Expert_Mode_Key), "True");
      The_10_Micron_Ip_Address := Ip_Address_For (Ten_Micron_Id);
      The_10_Micron_Port := Port_For (Ten_Micron_Id);

      Set (Remote_Handle);
      The_Telescope_Name := [String_Value_Of (Telescope_Key)];
      if Remote_Configured then
        Log.Write ("Telescope Name: " & Telescope_Name);
        The_Remote_Address := Ip_Address_For (Remote_Id);
        The_Remote_Port := Port_For (Remote_Id);
      end if;

      Set (Stellarium_Handle);
      The_Stellarium_Port := Port_For (Stellarium_Id);
      The_Search_Tolerance := Degrees_Of (Search_Tolerance_Key, Targets.Maximum_Search_Tolerance);
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

  function Is_Expert_Mode return Boolean is
  begin
    return Is_In_Expert_Mode;
  end Is_Expert_Mode;


  function Ten_Micron_Ip_Address return Network.Ip_Address is
  begin
    return The_10_Micron_Ip_Address;
  end Ten_Micron_Ip_Address;


  function Ten_Micron_Port return Network.Port_Number is
  begin
    return The_10_Micron_Port;
  end Ten_Micron_Port;


  ------------
  -- Remote --
  ------------

  function Remote_Configured return Boolean is
  begin
    return not (Strings.Is_Equal (Telescope_Name, "none") or Telescope_Name = "");
  end Remote_Configured;


  function Telescope_Name return String is
    use type Strings.Element;
  begin
    return +The_Telescope_Name;
  end Telescope_Name;


  function Remote_Address return Network.Ip_Address is
  begin
    return The_Remote_Address;
  end Remote_Address;


  function Remote_Port return Network.Port_Number is
  begin
    return The_Remote_Port;
  end Remote_Port;


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
