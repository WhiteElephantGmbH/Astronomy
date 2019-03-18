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

with Ada.Text_IO;
with Application;
with Configuration;
with Definite_Doubly_Linked_Lists;
with Error;
with File;
with Language;
with Os.Process;
with Network.Tcp;
with PWI;
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
  Simulation_Mode_Key   : constant String := "Simulation Mode";
  Expert_Mode_Key       : constant String := "Expert Mode";
  Fans_Key              : constant String := "Fans";
  Pointing_Model_Key    : constant String := "Pointing Model";
  Ip_Address_Key        : constant String := "IP Address";
  Moving_Speed_List_Key : constant String := "Moving Speed List";

  Stellarium_Id : constant String := "Stellarium";
  Lx200_Id      : constant String := "Lx200";
  Port_Key      : constant String := "Port";

  Site_Id       : constant String := "Site";
  Longitude_Key : constant String := "Longitude";
  Latitude_Key  : constant String := "Latitude";
  Altitude_Key  : constant String := "Altitude";

  The_Section : Configuration.Section_Handle;

  The_Telescope_Name    : Text.String;
  Is_In_Expert_Mode     : Boolean;
  Is_In_Simulation_Mode : Boolean;
  Fans_On               : Boolean;
  The_Pointing_Model    : Text.String;

  The_Moving_Speeds  : Angle_List.Item;
  The_PWI_Address    : Network.Ip_Address;
  The_PWI_Port       : Network.Port_Number;

  --Servers
  The_Lx200_Port      : Network.Port_Number;
  The_Stellarium_Port : Network.Port_Number;

  -- Site
  The_Latitude  : Angle.Value;
  The_Longitude : Angle.Value;
  The_Altitude  : Integer; -- in meters above see level

  CDK700_Latitude  : constant Angle.Degrees := 47.705500;
  CDK700_Longitude : constant Angle.Degrees :=  8.609865;
  CDK700_Altitude  : constant Integer       :=  540;



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
    begin
      return Standard.Language.Kind'value(Image);
    exception
    when others =>
      Error.Raise_With ("Incorrect " & Language_Key & ": <" & Image & ">");
    end;
  end Language;


  function Angle_Of (Key  : String;
                     Unit : String := "") return Angle.Value is
    Item : constant String := String_Of (Key);
  begin
    declare
      Image : constant String := Image_Of (Item, Unit);
    begin
      Log.Write (Key & ": " & Item);
      return Angle.Value_Of (Image);
    end;
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Angle_Of;


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


  function Value_Of (Key  : String;
                     Unit : String := "") return Integer is
    Item : constant String := String_Of (Key);
  begin
    return Integer'value(Image_Of(Item, Unit));
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
      Put (Program_Key & "           = C:\Program Files (x86)\PlaneWave Instruments\PlaneWave interface\PWI.exe");
      Put (Expert_Mode_Key & "       = False");
      Put (Simulation_Mode_Key & "   = False");
      Put (Fans_Key & "              = On");
      Put (Pointing_Model_Key & "    = First.PXP");
      Put (Ip_Address_Key & "        = 127.0.0.1");
      Put (Port_Key & "              = 8080");
      Put (Moving_Speed_List_Key & " = 6""/s, 1'/s, 10'/s, 3°00'/s");
      Put ("");
      Put ("[" & Lx200_Id & "]");
      Put (Port_Key & " = 4030");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Program_Key & " = C:\Program Files\Stellarium\Stellarium.exe");
      Put (Port_Key & "    = 10001");
      Put ("");
      Put ("[" & Site_Id & "]");
      Put (Longitude_Key & " = " & Angle.Image_Of (+CDK700_Longitude, Decimals => 2, Show_Signed => True));
      Put (Latitude_Key & "  = " & Angle.Image_Of (+CDK700_Latitude, Decimals => 2, Show_Signed => True));
      Put (Altitude_Key & "  ="  & CDK700_Altitude'img & "m");
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
      Lx200_Handle        : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Lx200_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Site_Handle         : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Site_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);

      procedure Connect_PWI is

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

      begin -- Connect_PWI
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
            Os.Process.Create (PWI_Program_Filename);
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
          exception
          when Error.Occurred =>
            raise;
          when others =>
            Error.Raise_With ("PlaneWave interface server not available");
          end;
        end;
      end Connect_PWI;

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
        begin
          Os.Process.Create (Stellarium_Filename);
        exception
        when others =>
          Error.Raise_With ("Stellarium not started");
        end;
      end Startup_Stellarium;

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

      Set (Site_Handle);
      The_Latitude := Angle_Of (Latitude_Key);
      The_Longitude := Angle_Of (Longitude_Key);
      The_Altitude := Value_Of (Altitude_Key, "m");

      Set (PWI_Handle);
      The_Telescope_Name := Text.String_Of (String_Value_Of (Name_Key));
      Log.Write ("Name: " & Telescope_Name);
      Is_In_Expert_Mode := Strings.Is_Equal (String_Value_Of (Expert_Mode_Key), "True");
      Is_In_Simulation_Mode := Strings.Is_Equal (String_Value_Of (Simulation_Mode_Key), "True");
      Define_Fans_State;
      The_Pointing_Model := Text.String_Of (String_Value_Of (Pointing_Model_Key));
      Log.Write ("Pointing_Model: " & Pointing_Model);
      Connect_PWI;
      PWI.Install (PWI_Socket'access);
      The_Moving_Speeds := Angles_Of (Moving_Speed_List_Key, Speed_Unit);
      if Natural(Angle_List.Length (The_Moving_Speeds)) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      Set (Lx200_Handle);
      begin
        The_Lx200_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Lx200 Port:" & The_Lx200_Port'img);
      exception
      when others =>
        Error.Raise_With ("Lx200 port number out of range");
      end;
      Set (Stellarium_Handle);
      Startup_Stellarium;
      begin
        The_Stellarium_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Stellarium Port:" & The_Stellarium_Port'img);
      exception
      when others =>
        Error.Raise_With ("Stellarium port number out of range");
      end;
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  ----------
  -- Site --
  ----------

  function Latitude return Angle.Value is
  begin
    return The_Latitude;
  end Latitude;


  function Longitude return Angle.Value is
  begin
    return The_Longitude;
  end Longitude;


  function Altitude return Integer is
  begin
    return The_Altitude;
  end Altitude;


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
  -- Servers --
  -------------

  function Lx200_Port return Network.Port_Number is
  begin
    return The_Lx200_Port;
  end Lx200_Port;


  function Stellarium_Port return Network.Port_Number is
  begin
    return The_Stellarium_Port;
  end Stellarium_Port;

end Parameter;
