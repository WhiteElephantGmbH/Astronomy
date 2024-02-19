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

with Ada.Text_IO;
with Application;
with Cdk_700;
with Configuration;
with Cwe;
with Doubly_Linked_Lists_Extension;
with Error;
with File;
with Language.Parameter;
with Network.Tcp;
with Os.System;
with Os.User;
with PWI2.Settings;
with Remote.Parameter;
with Section;
with Stellarium.Parameter;
with Strings;
with Sun.Parameter;
with Telescope;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  use type Strings.Element;

  Filename : constant String := Application.Composure (Application.Name, "ini");

  PWI_Id                : constant String := "PWI";
  Program_Key           : constant String := "Program";
  Settings_Key          : constant String := "Settings";
  Shutdown_Key          : constant String := "Shutdown";
  Simulation_Mode_Key   : constant String := "Simulation Mode";
  Expert_Mode_Key       : constant String := "Expert Mode";
  Fans_Key              : constant String := "Fans";
  M3_Default_Place_Key  : constant String := "M3 Default Place";
  M3_Ocular_Port_Key    : constant String := "M3 Ocular Port";
  Pointing_Model_Key    : constant String := "Pointing Model";
  Port_Key              : constant String := "Port";
  Moving_Speed_List_Key : constant String := "Moving Speed List";
  Cwe_Distance_Key      : constant String := "CWE Distance";
  Time_Adjustment_Key   : constant String := "Time Adjustment";
  Park_Position_Az_Key  : constant String := "Park Position Az";

  Controller_Id  : constant String := "Controller";
  Ip_Address_Key : constant String := "IP Address";

  Lx200_Id : constant String := "Lx200";

  Is_In_Shutdown_Mode   : Boolean := False;
  Is_In_Expert_Mode     : Boolean;
  Is_In_Simulation_Mode : Boolean;
  The_M3_Default_Place  : Device.M3.Place;
  The_M3_Ocular_Port    : PWI2.Port;
  Fans_On               : Boolean;
  The_Pointing_Model    : Strings.Element;

  The_Moving_Speeds   : Section.Angles.List;
  The_Cwe_Distance    : Angle.Degrees;
  The_PWI_Address     : Network.Ip_Address;
  The_PWI_Port        : Network.Port_Number;
  The_Time_Adjustment : Duration;

  --Lx200
  The_Lx200_Port : Network.Port_Number;


  function PWI_Socket return Network.Tcp.Socket is
    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
  begin
    return Network.Tcp.Socket_For (The_Address  => The_PWI_Address,
                                   The_Port     => The_PWI_Port,
                                   The_Protocol => Socket_Protocol);
  end PWI_Socket;


  procedure Read is

    Company_Name      : constant String := "PlaneWave Instruments";
    PWI_Program_Files : constant String := Os.System.Program_Files_X86_Folder & Company_Name;
    PWI_Documents     : constant String := Os.User.Documents_Folder & Company_Name;
    PWI_Mount_Folder  : constant String := PWI_Documents & "\PWI2\Mount\";

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
      Put ("[" & PWI_Id & "]");
      Put (Program_Key & "           = " & PWI_Program_Files & "\PlaneWave interface\PWI.exe");
      Put (Settings_Key & "          = " & PWI_Mount_Folder & "settingsMount.xml");
      Put (Shutdown_Key & "          = True");
      Put (Expert_Mode_Key & "       = False");
      Put (Simulation_Mode_Key & "   = False");
      Put (M3_Default_Place_Key & "  = Ocular");
      Put (M3_Ocular_Port_Key & "    = 1");
      Put (Fans_Key & "              = On");
      Put (Pointing_Model_Key & "    = Default_Mount_Model.PXP");
      Put (Ip_Address_Key & "        = Localhost");
      Put (Port_Key & "              = 8080");
      Put (Moving_Speed_List_Key & " = 30""/s, 3'/s, 20'/s, 2°/s");
      Put (Cwe_Distance_Key & "      = 30'");
      Put (Time_Adjustment_Key & "   = 0.5s");
      Put (Park_Position_Az_Key & "  = 225°");
      Put ("");
      Put ("[" & Controller_Id & "]");
      Put (Ip_Address_Key & " = 192.168.10.160");
      Put ("");
      Sun.Parameter.Defaults (Put'access);
      Put ("");
      Put ("[" & Lx200_Id & "]");
      Put (Port_Key & " = 4030");
      Put ("");
      Remote.Parameter.Defaults (Put'access, "cdk_west");
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

      Handle            : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      PWI_Handle        : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, PWI_Id);
      Lx200_Handle      : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Lx200_Id);
      Controller_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Controller_Id);

      procedure Define_Site_Parameters is
        Settings_Filename : constant String := Section.String_Value_Of (Settings_Key);
      begin
        if Settings_Filename = "" then
          Error.Raise_With ("PWI settings filename not defined");
        end if;
        Log.Write ("PWI settings file: """ & Settings_Filename & """");
        PWI2.Settings.Read (Settings_Filename);
      exception
      when PWI2.Settings.File_Not_Found =>
        Error.Raise_With ("PWI settings file " & Settings_Filename & " not found");
      when PWI2.Settings.Missing_Longitude =>
        Error.Raise_With ("PWI settings missing longitude");
      when PWI2.Settings.Missing_Latitude =>
        Error.Raise_With ("PWI settings missing latitude");
      when PWI2.Settings.Missing_Elevation =>
        Error.Raise_With ("PWI settings missing elevation");
      when PWI2.Settings.Missing_Lower_Azm_Goto_Limit =>
        Error.Raise_With ("PWI settings missing lower azm goto limit");
      when PWI2.Settings.Missing_Upper_Azm_Goto_Limit =>
        Error.Raise_With ("PWI settings missing upper azm goto limit");
      when PWI2.Settings.Missing_Lower_Alt_Goto_Limit =>
        Error.Raise_With ("PWI settings missing lower alt goto limit");
      when PWI2.Settings.Missing_Upper_Alt_Goto_Limit =>
        Error.Raise_With ("PWI settings missing upper alt goto limit");
      end Define_Site_Parameters;

      procedure Startup_PWI is

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
            if PWI2.Startup (PWI_Program_Filename) then
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


      procedure Define_M3_Default_Place is
        Place : constant String := Section.String_Value_Of (M3_Default_Place_Key);
      begin
        Log.Write (M3_Default_Place_Key & ": " & Place);
        if Strings.Is_Equal (Place, "Camera") then
          The_M3_Default_Place := Device.M3.Camera;
        elsif Strings.Is_Equal (Place, "Ocular") then
          The_M3_Default_Place := Device.M3.Ocular;
        else
          Error.Raise_With (M3_Default_Place_Key & " must be either Camera or Ocular");
        end if;
      end Define_M3_Default_Place;


      procedure Define_M3_Ocular_Port is
        Port : constant String := Section.String_Value_Of (M3_Ocular_Port_Key);
      begin
        Log.Write (M3_Ocular_Port_Key & ": " & Port);
        if Strings.Is_Equal (Port, "1") then
          The_M3_Ocular_Port := PWI2.Port_1;
        elsif Strings.Is_Equal (Port, "2") then
          The_M3_Ocular_Port := PWI2.Port_2;
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


      procedure Define_Pointing_Model is
        Model_Name : constant String := Section.String_Value_Of (Pointing_Model_Key);
        Model_File : constant String := PWI_Mount_Folder & Model_Name;
      begin
        if not File.Exists (Model_File) then
          Error.Raise_With ("Pointing Model """ & Model_File & """ not found");
        end if;
        The_Pointing_Model := [Model_Name];
        Log.Write ("Pointing_Model: " & Model_Name);
      end Define_Pointing_Model;

      Maximum_Speed : constant := 5; -- degrees per second

    begin -- Read_Values
      Language.Parameter.Define (Handle);

      Section.Set (PWI_Handle);
      Define_Site_Parameters;

      Is_In_Shutdown_Mode := Strings.Is_Equal (Section.String_Value_Of (Shutdown_Key), "True");
      Is_In_Expert_Mode := Strings.Is_Equal (Section.String_Value_Of (Expert_Mode_Key), "True");
      Is_In_Simulation_Mode := Strings.Is_Equal (Section.String_Value_Of (Simulation_Mode_Key), "True");
      Define_M3_Default_Place;
      Define_M3_Ocular_Port;
      Define_Fans_State;
      Define_Pointing_Model;
      Startup_PWI;
      PWI2.Install (PWI_Socket'access);
      The_Moving_Speeds := Section.Angles_Of (Moving_Speed_List_Key, Maximum_Speed, Speed_Unit);
      if Natural(The_Moving_Speeds.Length) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      The_Cwe_Distance := Section.Degrees_Of (Cwe_Distance_Key, Cwe.Maximum_Distance);
      The_Time_Adjustment := Section.Duration_Of (Time_Adjustment_Key, Lower_Limit => -2.0, Upper_Limit => 2.0);
      Telescope.Define_Park_Position (Section.Direction_Of (Park_Position_Az_Key));

      Section.Set (Controller_Handle);
      if not Is_In_Simulation_Mode or else Is_In_Expert_Mode then
        Cdk_700.Startup (Section.Ip_Address_For (Controller_Id),
                         Restart_Duration => 60.0); -- 1 Minute
      end if;

      Sun.Parameter.Define (Handle);

      Section.Set (Lx200_Handle);
      The_Lx200_Port := Section.Port_For (Lx200_Id);

      Remote.Parameter.Define (Handle);
      Stellarium.Parameter.Define (Handle);
    exception
    when Cdk_700.Startup_Failed =>
      Error.Raise_With ("CDK 700 not started");
    when Cdk_700.ENC_Not_Available =>
      Error.Raise_With ("ENC-2302 not available");
    when others =>
      if Is_In_Shutdown_Mode then
        PWI2.Shutdown;
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
      PWI2.Shutdown;
    end if;
    Stellarium.Shutdown;
  end Shutdown;


  ---------------
  -- Telescope --
  ---------------

  function Is_Expert_Mode return Boolean is
  begin
    return Is_In_Expert_Mode;
  end Is_Expert_Mode;


  function Is_Simulation_Mode return Boolean is
  begin
    return Is_In_Simulation_Mode;
  end Is_Simulation_Mode;


  function M3_Ocular_Port return PWI2.Port is
  begin
    return The_M3_Ocular_Port;
  end M3_Ocular_Port;


  function M3_Camera_Port return PWI2.Port is
  begin
    case The_M3_Ocular_Port is
    when PWI2.Port_1 =>
      return PWI2.Port_2;
    when PWI2.Port_2 =>
      return PWI2.Port_1;
    end case;
  end M3_Camera_Port;


  function M3_Default_Place return Device.M3.Place is
  begin
    return The_M3_Default_Place;
  end M3_Default_Place;


  function Turn_Fans_On return Boolean is
  begin
    return Fans_On;
  end Turn_Fans_On;


  function Pointing_Model return String is
  begin
    return +The_Pointing_Model;
  end Pointing_Model;


  function Pole_Height return Angle.Value is
  begin
    return Angle.Quadrant;
  end Pole_Height;


  function Is_Azimuthal_Mount return Boolean is
  begin
    return True;
  end Is_Azimuthal_Mount;


  function Moving_Speeds return Angle.Values is
    package Extension is new Doubly_Linked_Lists_Extension (Angle.Value, Angle.Values, Section.Angles);
  begin
    return Extension.Elements_Of (The_Moving_Speeds);
  end Moving_Speeds;


  function Cwe_Distance return Angle.Degrees is
  begin
    return The_Cwe_Distance;
  end Cwe_Distance;


  function Time_Adjustment return Duration is
  begin
    return The_Time_Adjustment;
  end Time_Adjustment;


  -----------
  -- Lx200 --
  -----------

  function Lx200_Port return Network.Port_Number is
  begin
    return The_Lx200_Port;
  end Lx200_Port;

end Parameter;
