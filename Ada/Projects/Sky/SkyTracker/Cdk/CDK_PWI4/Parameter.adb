-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Application;
with Cdk_700;
with Configuration;
with Cwe;
with Device;
with Doubly_Linked_Lists_Extension;
with Earth;
with Error;
with File;
with Language;
with Network.Tcp;
with Os.Process;
with Os.System;
with PWI4.Mount;
with Stellarium;
with Strings;
with Sun;
with Targets;
with Telescope;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  use type Angle.Value;

  package Angles is new Ada.Containers.Doubly_Linked_Lists (Angle.Value);

  use type Strings.Element;

  Filename : constant String := Application.Composure (Application.Name, "ini");

  Localization_Id : constant String := "Localization";
  Language_Key    : constant String := "Language";

  PWI_Id                : constant String := "PWI";
  Program_Key           : constant String := "Program";
  Shutdown_Key          : constant String := "Shutdown";
  Fans_Key              : constant String := "Fans";
  M3_Ocular_Port_Key    : constant String := "M3 Ocular Port";
  Port_Key              : constant String := "Port";
  Moving_Speed_List_Key : constant String := "Moving Speed List";
  Cwe_Distance_Key      : constant String := "CWE Distance";
  Park_Position_Az_Key  : constant String := "Park Position Az";
  Focuser_Maximum_Key   : constant String := "Focuser Maximum";

  Controller_Id        : constant String := "Controller";
  Ip_Address_Key       : constant String := "IP Address";
  Restart_Duration_Key : constant String := "Restart Duration";

  Server_Id      : constant String := "Server";
  Gui_Client_Key : constant String := "GUI Client";

  Sun_Id           : constant String := "Sun";
  Safety_Angle_Key : constant String := "Safety Angle";

  Remote_Id     : constant String := "Remote";
  Telescope_Key : constant String := "Telescope";

  Stellarium_Id        : constant String := "Stellarium";
  Search_Tolerance_Key : constant String := "Search Tolerance";
  Magnitude_Key        : constant String := "Magnitude";
  Satellite_Group_Key  : constant String := "Satellite Group";

  The_Section : Configuration.Section_Handle;

  Is_In_Shutdown_Mode : Boolean := False;
  The_M3_Ocular_Port  : PWI4.Port;
  Fans_On             : Boolean;

  The_Moving_Speeds : Angles.List;
  The_Cwe_Distance  : Angle.Degrees;
  The_PWI_Address   : Network.Ip_Address;
  The_PWI_Port      : Network.Port_Number;

  -- Server
  The_Server_Port : Network.Port_Number;

  --Remote
  The_Telescope_Name : Strings.Element;
  The_Remote_Address : Network.Ip_Address;
  The_Remote_Port    : Network.Port_Number;

  --Stellarium
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


  function Image_Of (The_Ordinal : Positive) return String is
    Image : constant String := Strings.Trimmed (The_Ordinal'img);
  begin
    if not (The_Ordinal in 4 .. 20) then
      case The_Ordinal mod 10 is
      when 1 =>
        return Image & "st";
      when 2 =>
        return Image & "nd";
      when 3 =>
        return Image & "rd";
      when others =>
        null;
      end case;
    end if;
    return Image & "th";
  end Image_Of;


  function Angles_Of (Key     : String;
                      Maximum : Natural; -- in degrees
                      Unit    : String := "") return Angles.List is
    Item     : constant String := String_Of (Key);
    Images   : constant Strings.Item := Strings.Item_Of (Item, ',');
    The_List : Angles.List;
  begin
    Log.Write (Key & ": " & Item);
    for Index in Strings.First_Index .. Images.Count loop
      begin
        declare
          Image : constant String := Image_Of (Images(Index), Unit);
          Value : constant Angle.Value := Angle.Value_Of (Image);
        begin
          if Value < (Angle.Value'(+Angle.Degrees(Maximum)) + Angle.Epsilon) then
            The_List.Append (Value);
          else
            Error.Raise_With ("value greater than" & Maximum'img & "°" & Unit);
          end if;
        end;
      exception
      when others =>
        Error.Raise_With ("Incorrect " & Image_Of (Index) & " value of " & Key & ": <" & Item & ">");
      end;
    end loop;
    return The_List;
  end Angles_Of;


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


  function Duration_Of (Key         : String;
                        Lower_Limit : Duration := 0.0;
                        Upper_Limit : Duration) return Duration is
    Image : constant String := String_Value_Of (Key);

    function Image_Of (Item : Duration) return String is
      type Value is delta 0.1 range Duration'first .. Duration'last;
    begin
      return Strings.Trimmed (Value(Item)'image) & 's';
    end Image_Of;

  begin -- Duration_Of
    if Image = "" then
      return 0.0;
    end if;
    Log.Write (Key & ": " & Image);
    if Image(Image'last) /= 's' then
      Error.Raise_With ("Unit s (seconds) missing at end of value for " & Key & ": <" & Image & ">");
    end if;
    begin
      declare
        Value : constant Duration := Duration'value(Image(Image'first .. Image'last - 1));
      begin
        if Value >= Lower_Limit and Value <= Upper_Limit then
          return Value;
        else
          Error.Raise_With ("value not in range " & Image_Of (Lower_Limit) & " .. " & Image_Of (Upper_Limit));
        end if;
      end;
    exception
    when others =>
      Error.Raise_With ("Incorrect value for " & Key & ": <" & Image & ">");
    end;
  end Duration_Of;


  function Direction_Of (Key : String) return Earth.Direction is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      return Earth.Unknown_Direction;
    end if;
    begin
      declare
        Az : constant Angle.Degrees := Degrees_Of (Key, 360.0);
      begin
        return Earth.Direction_Of (Az=> +Az, Alt => +Angle.Degrees(5.0));
      end;
    exception
    when others =>
      Error.Raise_With ("Incorrect value for " & Key & ": <" & Image & ">");
    end;
  end Direction_Of;


  function Value_Of (Key     : String;
                     Section : String := "") return Integer is
    Item : constant String := String_Of (Key, Section);
  begin
    return Integer'value(Image_Of(Item));
  exception
  when others =>
    Error.Raise_With ("Incorrect " & (if Section = "" then "" else Section & " ") & Key & ": <" & Item & ">");
  end Value_Of;


  function Value_Of (Key     : String;
                     Section : String := "") return Device.Microns is
    Item : constant String := String_Of (Key, Section);
  begin
    return Device.Microns(Positive'value(Item));
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


  function PWI_Address_And_Port return String is
  begin
    return Network.Image_Of (The_PWI_Address) & ":" & Network.Image_Of (The_PWI_Port);
  end PWI_Address_And_Port;


  procedure Read is

    Company_Name      : constant String := "PlaneWave Instruments";
    PWI_Program_Files : constant String := Os.System.Program_Files_X86_Folder & Company_Name;

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
      Put ("[" & Controller_Id & "]");
      Put (Ip_Address_Key & "       = 192.168.10.160");
      Put (Restart_Duration_Key & " = 10s");
      Put ("");
      Put ("[" & PWI_Id & "]");
      Put (Program_Key & "           = " & PWI_Program_Files & "\PlaneWave interface 4\PWI4.exe");
      Put (Shutdown_Key & "          = True");
      Put (M3_Ocular_Port_Key & "    = 1");
      Put (Fans_Key & "              = Off");
      Put (Ip_Address_Key & "        = Localhost");
      Put (Port_Key & "              = 8220");
      Put (Moving_Speed_List_Key & " = 30""/s, 3'/s, 20'/s, 2°/s");
      Put (Cwe_Distance_Key & "      = 30'");
      Put (Park_Position_Az_Key & "  = 75" & Angle.Degree);
      Put (Focuser_Maximum_Key & "   = 10000");
      Put ("");
      Put ("[" & Server_Id & "]");
      Put (Port_Key & "       = 9000");
      Put (Gui_Client_Key & " = " & Os.System.Program_Files_Folder & "White Elephant\Handbox.exe");
      Put ("");
      Put ("[" & Sun_Id & "]");
      Put (Safety_Angle_Key & " = 30" & Angle.Degree);
      Put ("");
      Put ("[" & Remote_Id & "]");
      Put (Telescope_Key & "  = cdk_ost");
      Put (Ip_Address_Key & " = 217.160.64.198");
      Put (Port_Key & "       = 5000");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "             = 10001");
      Put (Program_Key & "          = " & Os.System.Program_Files_Folder & "Stellarium\Stellarium.exe");
      Put (Search_Tolerance_Key & " = 3'");
      Put (Magnitude_Key & "        = 8.0");
      Put (Satellite_Group_Key & "  = CDK");
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
      PWI_Handle          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, PWI_Id);
      Controller_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Controller_Id);
      Server_Handle       : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Server_Id);
      Sun_Handle          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Sun_Id);
      Remote_Handle       : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Remote_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);


      procedure Startup_PWI is

        function PWI_Socket return Network.Tcp.Socket is
          Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
        begin
          return Network.Tcp.Socket_For (The_Address  => The_PWI_Address,
                                         The_Port     => The_PWI_Port,
                                         The_Protocol => Socket_Protocol);
        end PWI_Socket;

        procedure Prepare_Tcp is
        begin
          Network.Tcp.Close (PWI_Socket);
        end Prepare_Tcp;

        PWI_Program_Filename : constant String := String_Value_Of (Program_Key);

      begin -- Startup_PWI
        if PWI_Program_Filename = "" then
          Error.Raise_With ("No PWI program file specified");
        end if;
        Log.Write ("PWI program file: """ & PWI_Program_Filename & """");
        if not File.Exists (PWI_Program_Filename) then
          Error.Raise_With ("PWI program file """ & PWI_Program_Filename & """ not found");
        end if;
        The_PWI_Address := Ip_Address_For (PWI_Id);
        The_PWI_Port := Port_For (PWI_Id);
        begin
          Prepare_Tcp;
        exception
        when others =>
          declare
            The_Number_Of_Retries : Natural := 5;
          begin
            if PWI4.Startup (PWI_Program_Filename, Ip_Address => PWI_Address_And_Port) then
              loop
                begin
                  Prepare_Tcp;
                  exit;
                exception
                when others =>
                  if The_Number_Of_Retries = 0 then
                    Error.Raise_With ("PlaneWave interface server not enabled");
                  end if;
                  delay 1.0;
                  The_Number_Of_Retries := The_Number_Of_Retries - 1;
                  Log.Write ("retry to connect to PWI server");
                end;
              end loop;
            else
              Error.Raise_With ("PlaneWave interface not started");
            end if;
          end;
        end;
      end Startup_PWI;


      procedure Startup_Gui_Client is
        Client_Filename : constant String := String_Value_Of (Gui_Client_Key);
      begin
        if Client_Filename /= "" then
          Log.Write ("GUI client program file: """ & Client_Filename & """");
          if not File.Exists (Client_Filename) then
            Error.Raise_With ("GUI client program file """ & Client_Filename & """ not found");
          end if;
          begin
            Os.Process.Create (Client_Filename);
          exception
          when others =>
            Error.Raise_With ("GUI client not started");
          end;
        end if;
      end Startup_Gui_Client;


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


      procedure Define_M3_Ocular_Port is
        Port : constant String := String_Value_Of (M3_Ocular_Port_Key);
      begin
        Log.Write (M3_Ocular_Port_Key & ": " & Port);
        if Strings.Is_Equal (Port, "1") then
          The_M3_Ocular_Port := PWI4.Port_1;
        elsif Strings.Is_Equal (Port, "2") then
          The_M3_Ocular_Port := PWI4.Port_2;
        else
          Error.Raise_With (M3_Ocular_Port_Key & " must be either 1 or 2");
        end if;
      end Define_M3_Ocular_Port;


      procedure Define_Fans_State is
        Fans_State : constant String := String_Value_Of (Fans_Key);
      begin
        Log.Write ("Fans: " & Fans_State);
        if Strings.Is_Equal (Fans_State, "On") then
          Fans_On := True;
        elsif Strings.Is_Equal (Fans_State, "Off") then
          Fans_On := False;
        else
          Error.Raise_With ("Fans must be either On or Off");
        end if;
      end Define_Fans_State;

      Maximum_Speed : constant := 5; -- degrees per second

    begin -- Read_Values
      Set (Localization_Handle);
      Standard.Language.Define (Language);

      Set (Controller_Handle);
      Cdk_700.Startup (Ip_Address_For (Controller_Id),
                       Restart_Duration => Duration_Of (Restart_Duration_Key, Upper_Limit => 60.0));
      if Cdk_700.Had_Powerup then
        PWI4.Mount.Set_Powerup;
      end if;
      Set (PWI_Handle);
      Is_In_Shutdown_Mode := Strings.Is_Equal (String_Value_Of (Shutdown_Key), "True");
      Define_M3_Ocular_Port;
      Define_Fans_State;
      Startup_PWI;
      The_Moving_Speeds := Angles_Of (Moving_Speed_List_Key, Maximum_Speed, Speed_Unit);
      if Natural(The_Moving_Speeds.Length) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      The_Cwe_Distance := Degrees_Of (Cwe_Distance_Key, Cwe.Maximum_Distance);
      Telescope.Define_Park_Position (Direction_Of (Park_Position_Az_Key));
      Telescope.Define_Max_Focuser_Position (Value_Of (Focuser_Maximum_Key));

      Set (Server_Handle);
      The_Server_Port :=  Port_For (Server_Id);
      Startup_Gui_Client;

      Set (Sun_Handle);
      Sun.Define (Degrees_Of (Safety_Angle_Key, 180.0));

      Set (Remote_Handle);
      The_Telescope_Name := [String_Value_Of (Telescope_Key)];
      if Remote_Configured then
        Log.Write ("Telescope Name: " & Telescope_Name);
        The_Remote_Address := Ip_Address_For (Remote_Id);
        The_Remote_Port := Port_For (Remote_Id);
      end if;

      Set (Stellarium_Handle);
      The_Stellarium_Port :=  Port_For (Stellarium_Id);
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
    exception
    when Cdk_700.Startup_Failed =>
      Error.Raise_With ("CDK 700 not started");
    when Cdk_700.ENC_Not_Available =>
      Error.Raise_With ("ENC-2302 not available");
    when others =>
      if Is_In_Shutdown_Mode then
        PWI4.Shutdown;
      end if;
      raise;
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  procedure Shutdown is
  begin
    if Is_In_Shutdown_Mode then
      PWI4.Shutdown;
    end if;
    Stellarium.Shutdown;
  end Shutdown;


  ---------
  -- PWI --
  ---------

  function M3_Ocular_Port return PWI4.Port is
  begin
    return The_M3_Ocular_Port;
  end M3_Ocular_Port;


  function M3_Camera_Port return PWI4.Port is
  begin
    case The_M3_Ocular_Port is
    when PWI4.Port_1 =>
      return PWI4.Port_2;
    when PWI4.Port_2 =>
      return PWI4.Port_1;
    end case;
  end M3_Camera_Port;


  function Turn_Fans_On return Boolean is
  begin
    return Fans_On;
  end Turn_Fans_On;


  function Pole_Height return Angle.Value is
  begin
    return Angle.Quadrant;
  end Pole_Height;


  function Moving_Speeds return Angle.Values is
    package Extension is new Doubly_Linked_Lists_Extension (Angle.Value, Angle.Values, Angles);
  begin
    return Extension.Elements_Of (The_Moving_Speeds);
  end Moving_Speeds;


  function Cwe_Distance return Angle.Degrees is
  begin
    return The_Cwe_Distance;
  end Cwe_Distance;


  ------------
  -- Server --
  ------------

  function Server_Port return Network.Port_Number is
  begin
    return The_Server_Port;
  end Server_Port;


  ------------
  -- Remote --
  ------------

  function Remote_Configured return Boolean is
  begin
    return not (Strings.Is_Equal (Telescope_Name, "none") or Telescope_Name = "");
  end Remote_Configured;


  function Telescope_Name return String is
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
