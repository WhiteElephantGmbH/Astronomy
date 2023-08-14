-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Os.System;
with Picture;
with Stellarium;
with Strings;
with Sun;
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
  Magnitude_Key        : constant String := "Magnitude";
  Satellite_Group_Key  : constant String := "Satellite Group";

  Ten_Micron_Id   : constant String := "10micron";
  Expert_Mode_Key : constant String := "Expert Mode";
  Remote_Id       : constant String := "Remote";
  Telescope_Key   : constant String := "Telescope";
  Clock_Id        : constant String := "Clock";
  Ip_Address_Key  : constant String := "IP Address";
  Port_Key        : constant String := "Port";

  Sun_Id           : constant String := "Sun";
  Safety_Angle_Key : constant String := "Safety Angle";

  Astap_Key      : constant String := "ASTAP";
  Filename_Key   : constant String := "Filename";
  Picture_Id     : constant String := "Picture";
  Height_Key     : constant String := "Height";
  Width_Key      : constant String := "Width";

  The_Section : Configuration.Section_Handle;

  -- 10micron
  Is_In_Expert_Mode        : Boolean;
  The_10_Micron_Ip_Address : Network.Ip_Address;
  The_10_Micron_Port       : Network.Port_Number;

  -- Remote
  The_Telescope_Name : Strings.Element;
  The_Remote_Address : Network.Ip_Address;
  The_Remote_Port    : Network.Port_Number;

  -- Clock
  The_Udp_Socket : Network.Udp.Socket := Network.Udp.No_Socket;

  -- Stellarium
  The_Stellarium_Port  : Network.Port_Number;
  The_Search_Tolerance : Space.Distance;


  function Default_Astap_Executable return String is
  begin
    return Os.System.Program_Files_Folder & "astap\astap.exe";
  end Default_Astap_Executable;


  function Default_Picture_Filename return String is
  begin
    return "D:\temp\Picture.jpeg";
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
    Item : constant String := String_Of (Key, Section);
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
    Value : constant Integer := Value_Of (Port_Key, Section);
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


  function Filename_Of (Key : String) return String is
    Name : constant String := String_Of (Key);
  begin
    if not File.Exists (Name) then
      Error.Raise_With ("Filename " & Name & " not found for " & Key);
    end if;
    return Name;
  end Filename_Of;


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
      Put ("[" & Sun_Id & "]");
      Put (Safety_Angle_Key & " = 60" & Angle.Degree);
      Put ("");
      Put ("[" & Remote_Id & "]");
      Put (Telescope_Key & "  = apo");
      Put (Ip_Address_Key & " = 217.160.64.198");
      Put (Port_Key & "       = 5000");
      Put ("");
      Put ("[" & Clock_Id & "]");
      Put (Ip_Address_Key & " = 169.254.42.43"); -- or TimeServer
      Put (Port_Key & "       = 44422");
      Put ("");
      Put ("[" & Picture_Id & "]");
      Put (Astap_Key & "    = " & Default_Astap_Executable);
      Put (Filename_Key & " = " & Default_Picture_Filename);
      Put (Height_Key & "   = 2.97" & Angle.Degree);
      Put (Width_Key & "    = 4.46" & Angle.Degree);
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "             = 10001");
      Put (Program_Key & "          = " & Os.System.Program_Files_Folder & "Stellarium\Stellarium.exe");
      Put (Search_Tolerance_Key & " = 3'");
      Put (Magnitude_Key & "        = 8.0");
      Put (Satellite_Group_Key & "  = 10micron");
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
      Sun_Handle          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Sun_Id);
      Picture_Handle      : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Picture_Id);
      Clock_Handle        : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Clock_Id);
      Remote_Handle       : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Remote_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);

      procedure Connect_Clock is

        Name_Or_Address  : constant String := String_Value_Of (Ip_Address_Key);
        Datagram_Timeout : constant Duration := 0.3;

      begin -- Connect_Clock
        if Name_Or_Address /= "" then
          The_Udp_Socket := Network.Udp.Socket_For (Name_Or_Address => Name_Or_Address,
                                                    Port            => Port_For (Clock_Id),
                                                    Receive_Timeout => Datagram_Timeout);
          Log.Write ("Clock connected to " & Name_Or_Address);
        end if;
      exception
      when Network.Not_Found =>
        Error.Raise_With ("Clock not connected to " & Name_Or_Address);
      end Connect_Clock;


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

      Set (Sun_Handle);
      Sun.Define (Degrees_Of (Safety_Angle_Key, 180.0));

      Set (Remote_Handle);
      The_Telescope_Name := [String_Value_Of (Telescope_Key)];
      if Remote_Configured then
        Log.Write ("Telescope Name: " & Telescope_Name);
        The_Remote_Address := Ip_Address_For (Remote_Id);
        The_Remote_Port := Port_For (Remote_Id);
      end if;

      Set (Clock_Handle);
      Connect_Clock;

      Set (Picture_Handle);
      Astap.Define (Executable => Filename_Of (Astap_Key));
      Picture.Define (Name   => String_Of (Filename_Key),
                      Height => Degrees_Of (Height_Key, Picture.Maximum_Heigth),
                      Width  => Degrees_Of (Width_Key, Picture.Maximum_Width));

      Set (Stellarium_Handle);
      The_Stellarium_Port := Port_For (Stellarium_Id);
      The_Search_Tolerance := Degrees_Of (Search_Tolerance_Key, Targets.Maximum_Search_Tolerance);
      declare
        Image     : constant String := String_Of (Magnitude_Key, Stellarium_Id);
        Magnitude : Stellarium.Magnitude;
      begin
        Magnitude := Stellarium.Magnitude'value(Image);
        Log.Write ("Magnitude Maximum:" & Magnitude'image);
        Stellarium.Set_Maximum (Magnitude);
      exception
      when others =>
        Error.Raise_With ("Magnitude out of range");
      end;
      Stellarium.Set_Satellite_Group (String_Of (Satellite_Group_Key, Stellarium_Id));
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


  -----------
  -- Clock --
  -----------

  function Clock_Configured return Boolean is
    use type Network.Udp.Socket;
  begin
    return The_Udp_Socket /= Network.Udp.No_Socket;
  end Clock_Configured;


  function Clock_Socket return Network.Udp.Socket is
  begin
    return The_Udp_Socket;
  end Clock_Socket;


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
