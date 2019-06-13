-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Environment_Variables;
with Ada.Text_IO;
with Application;
with Configuration;
with Definite_Doubly_Linked_Lists;
with Error;
with File;
with Language;
with Network.Tcp;
with PWI.Settings;
with Strings;
with Text;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  use type Angle.Value;

  package Angle_List is new Definite_Doubly_Linked_Lists (Angle.Value);

  Filename : constant String := Application.Composure (Application.Name, "ini");

  Localization_Id : constant String := "Localization";
  Language_Key    : constant String := "Language";

  PWI_Id                : constant String := "PWI";
  Name_Key              : constant String := "Name";
  Program_Key           : constant String := "Program";
  Settings_Key          : constant String := "Settings";
  Shutdown_Key          : constant String := "Shutdown";
  Simulation_Mode_Key   : constant String := "Simulation Mode";
  Expert_Mode_Key       : constant String := "Expert Mode";
  Fans_Key              : constant String := "Fans";
  Pointing_Model_Key    : constant String := "Pointing Model";
  Ip_Address_Key        : constant String := "IP Address";
  Moving_Speed_List_Key : constant String := "Moving Speed List";

  Handbox_Id     : constant String := "Handbox";
  Lx200_Id       : constant String := "Lx200";
  Stellarium_Id  : constant String := "Stellarium";
  Port_Key       : constant String := "Port";
  Satellites_Key : constant String := "Satellites";
  Magnitude_Key  : constant String := "Magnitude";

  The_Section : Configuration.Section_Handle;

  The_Telescope_Name    : Text.String;
  Is_In_Shutdown_Mode   : Boolean := False;
  Is_In_Expert_Mode     : Boolean;
  Is_In_Simulation_Mode : Boolean;
  Fans_On               : Boolean;
  The_Pointing_Model    : Text.String;

  The_Moving_Speeds  : Angle_List.Item;
  The_PWI_Address    : Network.Ip_Address;
  The_PWI_Port       : Network.Port_Number;

  --Handbox
  The_Handbox_Is_Available : Boolean;
  The_Handbox_Port         : Serial_Io.Port;

  --Lx200
  The_Lx200_Port : Network.Port_Number;

  --Stellarium
  The_Stellarium_Port     : Network.Port_Number;
  The_Satellites_Filename : Text.String;
  The_Magnitude_Maximum   : Stellarium.Magnitude;


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
    begin
      return Standard.Language.Kind'value(Image);
    exception
    when others =>
      Error.Raise_With ("Incorrect " & Language_Key & ": <" & Image & ">");
    end;
  end Language;


  function Angles_Of (Key  : String;
                      Unit : String := "") return Angle_List.Item is
    Item     : constant String := String_Of (Key);
    Images   : constant Strings.Item := Strings.Item_Of (Item, ',');
    The_List : Angle_List.Item;
  begin
    Log.Write (Key & ": " & Item);
    for Index in Strings.First_Index .. Images.Count loop
      begin
        declare
          Image : constant String := Image_Of (Images(Index), Unit);
          Value : constant Angle.Value := Angle.Value_Of (Image);
          use type Angle_List.Item;
        begin
          if Value < Angle.Semi_Circle then
            The_List := The_List + Value;
          else
            Error.Raise_With ("value too large");
          end if;
        end;
      exception
      when others =>
        Error.Raise_With ("Incorrect" & Index'img & ". value of " & Key & ": <" & Item & ">");
      end;
    end loop;
    return The_List;
  end Angles_Of;


  function Value_Of (Key : String) return Integer is
    Item : constant String := String_Of (Key);
  begin
    return Integer'value(Image_Of(Item));
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  function PWI_Socket return Network.Tcp.Socket is
    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
  begin
    return Network.Tcp.Socket_For (The_Address  => The_PWI_Address,
                                   The_Port     => The_PWI_Port,
                                   The_Protocol => Socket_Protocol);
  end PWI_Socket;


  procedure Read is

    procedure Create_Default_Parameters is

      function Value (Item : String) return String is
      begin
        return Ada.Environment_Variables.Value (Item);
      exception
      when others =>
        Error.Raise_With ("Environment Variable " & Item & " not found");
      end Value;

      Company_Name      : constant String := "PlaneWave Instruments";
      PWI_Program_Files : constant String := Value ("ProgramFiles(x86)") & "\" & Company_Name;
      PWI_Documents     : constant String := Value ("HomeDrive") & Value ("HomePath") & "\Documents\" & Company_Name;

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
      Put (Language_Key & " = " & Strings.Legible_Of (Standard.Language.German'img));
      Put ("");
      Put ("[" & PWI_Id & "]");
      Put (Name_Key & "              = CDK Ost");
      Put (Program_Key & "           = " & PWI_Program_Files & "\PlaneWave interface\PWI.exe");
      Put (Settings_Key & "          = " & PWI_Documents & "\PWI2\Mount\settingsMount.xml");
      Put (Shutdown_Key & "          = True");
      Put (Expert_Mode_Key & "       = False");
      Put (Simulation_Mode_Key & "   = False");
      Put (Fans_Key & "              = On");
      Put (Pointing_Model_Key & "    = First.PXP");
      Put (Ip_Address_Key & "        = 127.0.0.1");
      Put (Port_Key & "              = 8080");
      Put (Moving_Speed_List_Key & " = 10""/s, 1'/s, 6'/s, 1°/s");
      Put ("");
      Put ("[" & Handbox_Id & "]");
      Put (Port_Key & " = Com3");
      Put ("");
      Put ("[" & Lx200_Id & "]");
      Put (Port_Key & " = 4030");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "       = 10001");
      Put (Program_Key & "    = C:\Program Files\Stellarium\Stellarium.exe");
      Put (Satellites_Key & " = " & Stellarium.Satellites_Filename);
      Put (Magnitude_Key & "  = 8.0");
      Ada.Text_IO.Close (The_File);
    exception
    when Item: others =>
      Log.Termination (Item);
      Ada.Text_IO.Delete (The_File);
      Error.Raise_With ("Internal Error - creating default parameters");
    end Create_Default_Parameters;


    procedure Read_Values is

      Handle              : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      PWI_Handle          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, PWI_Id);
      Handbox_Handle      : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Handbox_Id);
      Lx200_Handle        : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Lx200_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);

      procedure Define_Site_Parameters is
        Settings_Filename : constant String := String_Value_Of (Settings_Key);
      begin
        if Settings_Filename = "" then
          Error.Raise_With ("PWI settings filename not defined");
        end if;
        Log.Write ("PWI settings file: """ & Settings_Filename & """");
        PWI.Settings.Read (Settings_Filename);
      exception
      when PWI.Settings.File_Not_Found =>
        Error.Raise_With ("PWI settings file " & Settings_Filename & " not found");
      when PWI.Settings.Missing_Longitude =>
        Error.Raise_With ("PWI settings missing longitude");
      when PWI.Settings.Missing_Latitude =>
        Error.Raise_With ("PWI settings missing latitude");
      when PWI.Settings.Missing_Elevation =>
        Error.Raise_With ("PWI settings missing elevation");
      end Define_Site_Parameters;


      procedure Startup_PWI is

        procedure Prepare_Tcp is
          Server : constant String := String_Of (Ip_Address_Key);
        begin
          begin
            begin
              The_PWI_Address := Network.Ip_Address_Of (Server);
            exception
            when others =>
              The_PWI_Address := Network.Ip_Address_Of_Host (Server);
              Log.Write ("IP address of: " & Server & " = " & Network.Image_Of (The_PWI_Address));
            end;
            Network.Tcp.Close (PWI_Socket);
          end;
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
        begin
          The_PWI_Port := Network.Port_Number (Value_Of (Port_Key));
          Log.Write ("PWI port:" & The_PWI_Port'img);
        exception
        when others =>
          Error.Raise_With ("PWI port number out of range");
        end;
        begin
          Prepare_Tcp;
        exception
        when others =>
          declare
            The_Number_Of_Retries : Natural := 5;
          begin
            if PWI.Startup (PWI_Program_Filename) then
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


      procedure Define_Satellites_Filename is
        Json_Filename : constant String := String_Value_Of (Satellites_Key);
      begin
        if Json_Filename = "" or Strings.Is_Equal (Json_Filename, "None") then
          Log.Write ("No Satellites");
          Text.Clear (The_Satellites_Filename);
        else
          Log.Write ("Stellarium satellites file: """ & Json_Filename & """");
          if not File.Exists (Json_Filename) then
            Error.Raise_With ("Stellarium satellites file """ & Json_Filename & """ not found");
          end if;
          The_Satellites_Filename := Text.String_Of (Json_Filename);
        end if;
      end Define_Satellites_Filename;


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

    begin -- Read_Values
      Set (Localization_Handle);
      Standard.Language.Define (Language);

      Set (PWI_Handle);
      The_Telescope_Name := Text.String_Of (String_Value_Of (Name_Key));
      Log.Write ("Name: " & Telescope_Name);

      Define_Site_Parameters;

      Is_In_Shutdown_Mode := Strings.Is_Equal (String_Value_Of (Shutdown_Key), "True");
      Is_In_Expert_Mode := Strings.Is_Equal (String_Value_Of (Expert_Mode_Key), "True");
      Is_In_Simulation_Mode := Strings.Is_Equal (String_Value_Of (Simulation_Mode_Key), "True");
      Define_Fans_State;
      The_Pointing_Model := Text.String_Of (String_Value_Of (Pointing_Model_Key));
      Log.Write ("Pointing_Model: " & Pointing_Model);
      Startup_PWI;
      PWI.Install (PWI_Socket'access);
      The_Moving_Speeds := Angles_Of (Moving_Speed_List_Key, Speed_Unit);
      if Natural(Angle_List.Length (The_Moving_Speeds)) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;

      Set (Handbox_Handle);
      declare
        Port_Name : constant String := Strings.Legible_Of (String_Of (Port_Key, "Handbox"));
      begin
        if Port_Name /= "None" then
          begin
            The_Handbox_Port := Serial_Io.Port'value(Port_Name);
          exception
          when others =>
            Error.Raise_With ("Handbox port " & Port_Name & " is not in range Com1 .. Com8");
          end;
          if not Serial_Io.Is_Available (The_Handbox_Port) then
            Error.Raise_With ("Handbox port " & Port_Name & " is not available");
          end if;
          The_Handbox_Is_Available := True;
          Log.Write ("Handbox on Port: " & The_Handbox_Port'img);
        else
          The_Handbox_Is_Available := False;
          Log.Write ("No Handbox");
        end if;
      end;

      Set (Lx200_Handle);
      begin
        The_Lx200_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Lx200 Port:" & The_Lx200_Port'img);
      exception
      when others =>
        Error.Raise_With ("Lx200 port number out of range");
      end;
      Set (Stellarium_Handle);
      Define_Satellites_Filename;
      begin
        The_Stellarium_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Stellarium Port:" & The_Stellarium_Port'img);
      exception
      when others =>
        Error.Raise_With ("Stellarium port number out of range");
      end;
      declare
        Image : constant String := String_Value_Of (Magnitude_Key);
      begin
        if Image = "" then
          The_Magnitude_Maximum := Stellarium.Magnitude'last;
        else
          The_Magnitude_Maximum := Stellarium.Magnitude'value(Image);
        end if;
        Log.Write ("Magnitude Maximum:" & The_Magnitude_Maximum'img);
      exception
      when others =>
        Error.Raise_With ("Magnitude out of range");
      end;
      Startup_Stellarium;
    exception
    when others =>
      if Is_In_Shutdown_Mode then
        PWI.Shutdown;
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
      PWI.Shutdown;
    end if;
    Stellarium.Shutdown;
  end Shutdown;


  ----------
  -- Site --
  ----------

  function Latitude return Angle.Value is
  begin
    return PWI.Settings.Latitude;
  end Latitude;


  function Longitude return Angle.Value is
  begin
    return PWI.Settings.Longitude;
  end Longitude;


  function Elevation return Integer is
  begin
    return PWI.Settings.Elevation;
  end Elevation;


  ---------------
  -- Telescope --
  ---------------

  function Telescope_Name return String is
  begin
    return Text.String_Of (The_Telescope_Name);
  end Telescope_Name;


  function Is_Expert_Mode return Boolean is
  begin
    return Is_In_Expert_Mode;
  end Is_Expert_Mode;


  function Is_Simulation_Mode return Boolean is
  begin
    return Is_In_Simulation_Mode;
  end Is_Simulation_Mode;


  function Turn_Fans_On return Boolean is
  begin
    return Fans_On;
  end Turn_Fans_On;


  function Pointing_Model return String is
  begin
    return Text.String_Of (The_Pointing_Model);
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
  begin
    return Angle.Values(Angle_List.Elements (The_Moving_Speeds));
  end Moving_Speeds;


  -------------
  -- Handbox --
  -------------

  function Handbox_Is_Available return Boolean is
  begin
    return The_Handbox_Is_Available;
  end Handbox_Is_Available;


  function Handbox_Port return Serial_Io.Port is
  begin
    return The_Handbox_Port;
  end Handbox_Port;


  -----------
  -- Lx200 --
  -----------

  function Lx200_Port return Network.Port_Number is
  begin
    return The_Lx200_Port;
  end Lx200_Port;


  ----------------
  -- Stellarium --
  ----------------

  function Stellarium_Port return Network.Port_Number is
  begin
    return The_Stellarium_Port;
  end Stellarium_Port;


  function Satellites_Filename return String is
  begin
    return Text.String_Of (The_Satellites_Filename);
  end Satellites_Filename;


  function Magnitude_Maximum return Stellarium.Magnitude is
  begin
    return The_Magnitude_Maximum;
  end Magnitude_Maximum;

end Parameter;
