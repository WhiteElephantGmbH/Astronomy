-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Cdk_700.Parameter;
with Configuration;
with Cwe;
with Device;
with Doubly_Linked_Lists_Extension;
with Error;
with File;
with Http_Server.Parameter;
with Language.Parameter;
with Network.Tcp;
with Os.System;
with Picture.Parameter;
with PWI4.Mount;
with Remote.Parameter;
with Section;
with Stellarium.Parameter;
with Strings;
with Sun.Parameter;
with Telescope;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

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
  Ip_Address_Key        : constant String := Section.Ip_Address_Key;

  Is_In_Shutdown_Mode : Boolean := False;
  The_M3_Ocular_Port  : PWI4.Port;
  Fans_On             : Boolean;

  The_Moving_Speeds : Section.Angles.List;
  The_Cwe_Distance  : Angle.Degrees;
  The_PWI_Address   : Network.Ip_Address;
  The_PWI_Port      : Network.Port_Number;


  function Value_Of (Key  : String;
                     Name : String := "") return Device.Microns is
    Item : constant String := Section.String_Of (Key, Name);
  begin
    return Device.Microns(Positive'value(Item));
  exception
  when others =>
    Error.Raise_With ("Incorrect " & (if Name = "" then "" else Name & " ") & Key & ": <" & Item & ">");
  end Value_Of;


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
      Language.Parameter.Defaults (Put'access);
      Put ("");
      Cdk_700.Parameter.Defaults (Put'access);
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
      Http_Server.Parameter.Defaults (Put'access, "Handbox");
      Put ("");
      Sun.Parameter.Defaults (Put'access);
      Put ("");
      Remote.Parameter.Defaults (Put'access, "cdk_Ost");
      Put ("");
      Picture.Parameter.Defaults (Put'access);
      Put ("");
      Stellarium.Parameter.Defaults (Put'access, "CDK");
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

      Handle     : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      PWI_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, PWI_Id);


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

        PWI_Program_Filename : constant String := Section.String_Value_Of (Program_Key);

      begin -- Startup_PWI
        if PWI_Program_Filename = "" then
          Error.Raise_With ("No PWI program file specified");
        end if;
        Log.Write ("PWI program file: """ & PWI_Program_Filename & """");
        if not File.Exists (PWI_Program_Filename) then
          Error.Raise_With ("PWI program file """ & PWI_Program_Filename & """ not found");
        end if;
        The_PWI_Address := Section.Ip_Address_For (PWI_Id);
        The_PWI_Port := Section.Port_For (PWI_Id);
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


      procedure Define_M3_Ocular_Port is
        Port : constant String := Section.String_Value_Of (M3_Ocular_Port_Key);
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
        Fans_State : constant String := Section.String_Value_Of (Fans_Key);
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
      Language.Parameter.Define (Handle);

      Cdk_700.Parameter.Define (Handle);
      Cdk_700.Startup;
      if Cdk_700.Had_Powerup then
        PWI4.Mount.Set_Powerup;
      end if;

      Section.Set (PWI_Handle);
      Is_In_Shutdown_Mode := Strings.Is_Equal (Section.String_Value_Of (Shutdown_Key), "True");
      Define_M3_Ocular_Port;
      Define_Fans_State;
      Startup_PWI;
      The_Moving_Speeds := Section.Angles_Of (Moving_Speed_List_Key, Maximum_Speed, Speed_Unit);
      if Natural(The_Moving_Speeds.Length) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      The_Cwe_Distance := Section.Degrees_Of (Cwe_Distance_Key, Cwe.Maximum_Distance);
      Telescope.Define_Park_Position (Section.Direction_Of (Park_Position_Az_Key));
      Telescope.Define_Max_Focuser_Position (Value_Of (Focuser_Maximum_Key));

      Http_Server.Parameter.Define (Handle);
      Sun.Parameter.Define (Handle);
      Remote.Parameter.Define (Handle);
      Picture.Parameter.Define (Handle);
      Stellarium.Parameter.Define (Handle);
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


  function Moving_Speeds return Angle.Values is
    package Extension is new Doubly_Linked_Lists_Extension (Angle.Value, Angle.Values, Section.Angles);
  begin
    return Extension.Elements_Of (The_Moving_Speeds);
  end Moving_Speeds;


  function Cwe_Distance return Angle.Degrees is
  begin
    return The_Cwe_Distance;
  end Cwe_Distance;

end Parameter;
