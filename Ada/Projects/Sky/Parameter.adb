-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Numerics;
with Ada.Text_IO;
with Application;
with Astro;
with Configuration;
with Definite_Doubly_Linked_Lists;
with Error;
with File;
with Language;
with Motor;
with Os;
with Os.Process;
with Stellarium;
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

  Telescope_Id                    : constant String := "Telescope";
  Name_Key                        : constant String := "Name";
  Ip_Address_Key                  : constant String := "IP Address";
  Serial_Port_Key                 : constant String := "Serial Port";
  Steps_Per_Revolution_Key        : constant String := "Steps Per Revolution";
  First_Steps_Per_Revolution_Key  : constant String := "First Steps Per Revolution";
  Second_Steps_Per_Revolution_Key : constant String := "Second Steps Per Revolution";
  Clocks_Per_Second_Key           : constant String := "Clocks Per Second";
  Park_Azimuth_Key                : constant String := "Park Azimuth";
  Park_Altitude_Key               : constant String := "Park Altitude";
  Pole_Height_Key                 : constant String := "Pole Height";
  Moving_Speed_List_Key           : constant String := "Moving Speed List";
  First_Acceleration_Key          : constant String := "First Acceleration";
  Second_Acceleration_Key         : constant String := "Second Acceleration";
  First_Lower_Limit_Key           : constant String := "First Lower Limit";
  First_Upper_Limit_Key           : constant String := "First Upper Limit";
  Second_Lower_Limit_Key          : constant String := "Second Lower Limit";
  Second_Upper_Limit_Key          : constant String := "Second Upper Limit";

  Stellarium_Id  : constant String := "Stellarium";
  Lx200_Id       : constant String := "Lx200";
  Port_Key       : constant String := "Port";
  Program_Key    : constant String := "Program";
  Satellites_Key : constant String := "Satellites";

  Site_Id       : constant String := "Site";
  Longitude_Key : constant String := "Longitude";
  Latitude_Key  : constant String := "Latitude";
  Altitude_Key  : constant String := "Altitude";
  Sky_Line_Key  : constant String := "Sky Line";

  The_Section : Configuration.Section_Handle;

  --Telescope
  Datagram_Port : constant := 44422;

  The_Telescope_Name       : Text.String;
  Is_In_Setup_Mode         : Boolean;
  The_Steps_Per_Revolution : Device.Steps_Per_Revolution;
  The_Clocks_Per_Second    : Natural;
  The_Park_Azimuth         : Angle.Value;
  The_Park_Altitude        : Angle.Value;
  The_Pole_Height          : Angle.Value;
  The_Moving_Speeds        : Angle_List.Item;
  The_First_Acceleration   : Angle.Value;
  The_Second_Acceleration  : Angle.Value;
  The_First_Lower_Limit    : Angle.Degrees;
  The_First_Upper_Limit    : Angle.Degrees;
  The_Second_Lower_Limit   : Angle.Degrees;
  The_Second_Upper_Limit   : Angle.Degrees;
  The_Telescope_Connection : Connection;

  --Servers
  The_Lx200_Port          : Network.Port_Number;
  The_Stellarium_Port     : Network.Port_Number;
  The_Satellites_Filename : Text.String;

  -- Site
  The_Latitude  : Angle.Value;
  The_Longitude : Angle.Value;
  The_Altitude  : Integer; -- in meters above see level
  The_Sky_Line  : Text.String;


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


  function Pole_Heigth return Angle.Value is
    Key  : constant String := Pole_Height_Key;
    Item : constant String := String_Value_Of (Key);
  begin
    if Item = "Latitude" then
      return The_Latitude;
    else
      return Angle_Of (Key);
    end if;
  end Pole_Heigth;


  function Degrees_Of (Key : String) return Angle.Degrees is
    Item  : constant String := String_Of (Key);
  begin
    declare
      Image : constant String := Image_Of (Item, Unit => "°");
    begin
      Log.Write (Key & ": " & Item);
      return Angle.Degrees'value(Image);
    end;
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Degrees_Of;


  function Value_Of (Key  : String;
                     Unit : String := "") return Integer is
    Item : constant String := String_Of (Key);
  begin
    return Integer'value(Image_Of(Item, Unit));
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  function Number_Of (Key  : String) return Positive is
    Item : constant String := String_Of (Key);
  begin
    return Positive'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Number_Of;


  procedure Read (Is_Stepper_Driver : Boolean) is

    procedure Create_Default_Parameters is

      The_File : Ada.Text_IO.File_Type;

      procedure Put (Line : String) is
      begin
        Ada.Text_IO.Put_Line (The_File, Line);
      end Put;

      procedure Put_Steps_Per_Revolution (Steps : Positive) is
        Steps_Image : constant String := Positive'image(Steps);
      begin
        Put (First_Steps_Per_Revolution_Key & "  =" & Steps_Image);
        Put (Second_Steps_Per_Revolution_Key & " =" & Steps_Image);
      end Put_Steps_Per_Revolution;

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
      case Default_Location is
      when Home =>
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                 = Setup");
        if Is_Stepper_Driver then
          Put (Ip_Address_Key & "           = SkyTracker" & (if Os.Is_Windows then "" else ".local"));
          Put_Steps_Per_Revolution (141 * 5 * 200 * 16); -- EQU-6
          Put (Clocks_Per_Second_Key & "    = 5000000");
        else
          Put (Ip_Address_Key & "           = 127.0.0.1");
        end if;
        Put (Park_Azimuth_Key & "         = +137°16'00.0""");
        Put (Park_Altitude_Key & "        = +5°35'00.0""");
        Put (Pole_Height_Key & "          = Latitude");
        Put (Moving_Speed_List_Key & "    = 6""/s, 1'/s, 10'/s, 6°/s");
        Put (First_Acceleration_Key & "   = 6°/s²");
        Put (Second_Acceleration_Key & "  = 6°/s²");
        Put (First_Lower_Limit_Key & "    = -5°");
        Put (First_Upper_Limit_Key & "    = 185°");
        Put (Second_Lower_Limit_Key & "   = 0°");
        Put (Second_Upper_Limit_Key & "   = 0°");
      when Sternwarte_Schaffhausen =>
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                        = Newton");
        if Is_Stepper_Driver then
          Put (Serial_Port_Key & "                 = None");
          Put_Steps_Per_Revolution (240 * 8 * 200 * 16);
          Put (Clocks_Per_Second_Key & "           = 5000228");
        else
          Put (Ip_Address_Key & "                  = 127.0.0.1");
        end if;
        Put (Park_Azimuth_Key & "                = +74°");
        Put (Park_Altitude_Key & "               = -5°");
        Put (Pole_Height_Key & "                 = 90°");
        Put (Moving_Speed_List_Key & "           = 6""/s, 1'/s, 10'/s, 3°00'/s");
        Put (First_Acceleration_Key & "          = 30'/s²");
        Put (Second_Acceleration_Key & "         = 30'/s²");
        Put (First_Lower_Limit_Key & "           = -1726°");
        Put (First_Upper_Limit_Key & "           = +1874°");
        Put (Second_Lower_Limit_Key & "          = -10°");
        Put (Second_Upper_Limit_Key & "          = +90°");
      when Unknown =>
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                        = ");
        if Is_Stepper_Driver then
          Put (Ip_Address_Key & "                  = None");
          Put_Steps_Per_Revolution (1232086); -- M-Zero
          Put (Clocks_Per_Second_Key & "           = 5000000");
        else
          Put (Ip_Address_Key & "                  = 127.0.0.1");
        end if;
        Put (Park_Azimuth_Key & "                = +180°00'");
        Put (Park_Altitude_Key & "               = +0°00'");
        Put (Pole_Height_Key & "                 = Latitude");
        Put (Moving_Speed_List_Key & "           = 6""/s, 1'/s, 10'/s, 6°/s");
        Put (First_Acceleration_Key & "          = 3°/s²");
        Put (Second_Acceleration_Key & "         = 3°/s²");
        Put (First_Lower_Limit_Key & "           = -360°");
        Put (First_Upper_Limit_Key & "           = +360°");
        Put (Second_Lower_Limit_Key & "          = -30°");
        Put (Second_Upper_Limit_Key & "          = +210°");
      end case;
      Put ("");
      Put ("[" & Lx200_Id & "]");
      Put (Port_Key & " = 4030");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Program_Key & "    = C:\Program Files\Stellarium\Stellarium.exe");
      Put (Satellites_Key & " = " & Stellarium.Satellites_Filename);
      Put (Port_Key & "       = 10001");
      Put ("");
      Put ("[" & Site_Id & "]");
      Put (Longitude_Key & " = " & Angle.Image_Of (+Stellarium.Longitude, Decimals => 2, Show_Signed => True));
      Put (Latitude_Key & "  = " & Angle.Image_Of (+Stellarium.Latitude, Decimals => 2, Show_Signed => True));
      Put (Altitude_Key & "  ="  & Stellarium.Altitude'img & "m");
      Put (Sky_Line_Key & "  = " & Stellarium.Landscape);
      Ada.Text_IO.Close (The_File);
    exception
    when Item: others =>
      Log.Termination (Item);
      Ada.Text_IO.Delete (The_File);
      Error.Raise_With ("Internal Error - creating default parameters");
    end Create_Default_Parameters;


    procedure Read_Values is

      Handle              : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      Telescope_Handle    : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Telescope_Id);
      Lx200_Handle        : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Lx200_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Site_Handle         : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Site_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);

      use type Angle.Degrees;

      procedure Connect_Telescope is

        procedure Prepare_Udp (Telescope : String) is
          The_Ip_Address : Network.Ip_Address;
        begin
          begin
            begin
              The_Ip_Address := Network.Ip_Address_Of (Telescope);
            exception
            when others =>
              The_Ip_Address := Network.Ip_Address_Of_Host (Telescope);
              Log.Write ("IP address of: " & Telescope & " = " & Network.Image_Of (The_Ip_Address));
            end;
            The_Telescope_Connection := (Kind   => Is_Udp,
                                         Socket => Network.Udp.Socket_For (Port            => Datagram_Port,
                                                                           Receive_Timeout => Datagram_Timeout),
                                         Address => Network.Address_Of (The_Ip_Address, Datagram_Port));

          exception
          when others =>
            Error.Raise_With ("Can't find the telescope " & Telescope);
          end;
        end Prepare_Udp;

        procedure Prepare_Serial (Port_Name : String) is
        begin
          begin
            The_Telescope_Connection := (Kind => Is_Serial,
                                         Port => Serial_Io.Port'value(Port_Name));
          exception
          when others =>
            Error.Raise_With ("Incorrect Serial Port: <" & Port_Name & ">");
          end;
        end Prepare_Serial;


        function Step_Number_Of (Key : String) return Device.Step_Number is
          Number : constant Positive := Number_Of (Key);
        begin
          if Number > Device.Step_Number'last  then
            Error.Raise_With ("Too many steps per revolution");
          end if;
          return Number;
        end Step_Number_Of;


        Port_Name : constant String := String_Value_Of (Serial_Port_Key);

      begin -- Connect_Telescope
        if Port_Name /= "" then
          if Port_Name /= "None" then
            Prepare_Serial (Port_Name);
          end if;
        else
          declare
            Telescope : constant String := String_Of (Ip_Address_Key);
          begin
            if not Is_Stepper_Driver or else (Telescope /= "None") then
              Prepare_Udp (Telescope);
            end if;
          end;
        end if;
        if Is_Stepper_Driver then
          The_Clocks_Per_Second := Number_Of (Clocks_Per_Second_Key);
          if String_Value_Of (Steps_Per_Revolution_Key) = "" then
            The_Steps_Per_Revolution(Device.D1) := Step_Number_Of (First_Steps_Per_Revolution_Key);
            The_Steps_Per_Revolution(Device.D2) := Step_Number_Of (Second_Steps_Per_Revolution_Key);
          elsif (String_Value_Of (First_Steps_Per_Revolution_Key) = "") and
                (String_Value_Of (Second_Steps_Per_Revolution_Key) = "")
          then
            The_Steps_Per_Revolution(Device.D1) := Step_Number_Of (Steps_Per_Revolution_Key);
            The_Steps_Per_Revolution(Device.D2) := The_Steps_Per_Revolution(Device.D1);
          else
            Error.Raise_With ("The steps per revolution are defined more then once");
          end if;
        end if;
        if The_Telescope_Connection.Kind = Is_Simulated then
          Text.Append_To (The_Telescope_Name, " Simulation");
          Log.Write ("Telescope Simulation");
        end if;
        Motor.Connect_Communication;
      end Connect_Telescope;

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

    begin -- Read_Values
      Set (Localization_Handle);
      Standard.Language.Define (Language);

      Set (Site_Handle);
      The_Latitude := Angle_Of (Latitude_Key);
      The_Longitude := Angle_Of (Longitude_Key);
      The_Altitude := Value_Of (Altitude_Key, "m");
      The_Sky_Line := Text.String_Of (String_Value_Of (Sky_Line_Key));
      Log.Write (Sky_Line_Key & ": " & Sky_Line);

      Set (Telescope_Handle);
      The_Telescope_Name := Text.String_Of (String_Value_Of ("Name"));
      Log.Write ("Name: " & Telescope_Name);
      Is_In_Setup_Mode := Telescope_Name = "Setup";
      Connect_Telescope;
      The_Park_Azimuth := Angle_Of (Park_Azimuth_Key);
      The_Park_Altitude := Angle_Of (Park_Altitude_Key);
      The_Pole_Height := Pole_Heigth;
      The_Moving_Speeds := Angles_Of (Moving_Speed_List_Key, Speed_Unit);
      if Natural(Angle_List.Length (The_Moving_Speeds)) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      The_First_Acceleration := Angle_Of (First_Acceleration_Key, Acceleration_Unit);
      The_Second_Acceleration := Angle_Of (Second_Acceleration_Key, Acceleration_Unit);
      The_First_Lower_Limit := Degrees_Of (First_Lower_Limit_Key);
      The_First_Upper_Limit := Degrees_Of (First_Upper_Limit_Key);
      The_Second_Lower_Limit := Degrees_Of (Second_Lower_Limit_Key);
      The_Second_Upper_Limit := Degrees_Of (Second_Upper_Limit_Key);
      if The_First_Lower_Limit /= 0.0 or The_First_Upper_Limit /= 0.0 then
        declare
          The_Minimum_Range : Angle.Degrees;
        begin
          if The_Second_Upper_Limit = 0.0 or The_Second_Upper_Limit >= 180.0 then
            The_Minimum_Range := 180.0;
          else
            The_Minimum_Range := 360.0;
          end if;
          if (The_First_Upper_Limit - The_First_Lower_Limit) < The_Minimum_Range then
            Error.Raise_With ("Incorrect first limit range");
          end if;
        end;
      end if;
      if The_Second_Lower_Limit /= 0.0 or The_Second_Upper_Limit /= 0.0 then
        if The_Second_Lower_Limit > 0.0 or The_Second_Upper_Limit < 90.0 then
          Error.Raise_With ("Incorrect second limit range");
        end if;
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
      Define_Satellites_Filename;
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


  ----------
  -- Site --
  ----------

  Home_Latitude    : constant Angle.Degrees := 47.695009;
  Home_Longitude   : constant Angle.Degrees :=  8.627870;

  Newton_Latitude  : constant Angle.Degrees := 47.705500;
  Newton_Longitude : constant Angle.Degrees :=  8.609865;

  function Default_Location return Location is

    function "=" (Left, Right : Angle.Degrees) return Boolean is
      use type Angle.Degrees;
      Two_Meters : constant Angle.Degrees := 0.36 / (Astro.Earth_Equatorial_Radius * Ada.Numerics.Pi);
    begin
      return abs (Left - Right) < Two_Meters;
    end "=";

  begin
    if Stellarium.Latitude = Home_Latitude and then Stellarium.Longitude = Home_Longitude then
      return Home;
    elsif Stellarium.Latitude = Newton_Latitude and then Stellarium.Longitude = Newton_Longitude then
      return Sternwarte_Schaffhausen;
    end if;
    return Unknown;
  end Default_Location;


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


  function Sky_Line return String is
  begin
    return Text.String_Of (The_Sky_Line);
  end Sky_Line;


  ---------------
  -- Telescope --
  ---------------

  function Telescope_Name return String is
  begin
    return Text.String_Of (The_Telescope_Name);
  end Telescope_Name;


  function Is_Setup_Mode return Boolean is
  begin
    return Is_In_Setup_Mode;
  end Is_Setup_Mode;


  function Telescope_Connection return Connection is
  begin
    return The_Telescope_Connection;
  end Telescope_Connection;


  function Steps_Per_Revolution return Device.Steps_Per_Revolution is
  begin
    return The_Steps_Per_Revolution;
  end Steps_Per_Revolution;


  function Clocks_Per_Second return Positive is
  begin
    return The_Clocks_Per_Second;
  end Clocks_Per_Second;


  function Park_Azimuth return Angle.Value is
  begin
    return The_Park_Azimuth;
  end Park_Azimuth;


  function Park_Altitude return Angle.Value is
  begin
    return The_Park_Altitude;
  end Park_Altitude;


  function Pole_Height return Angle.Value is
  begin
    return The_Pole_Height;
  end Pole_Height;


  function Is_Azimuthal_Mount return Boolean is
  begin
    return The_Pole_Height = Angle.Quadrant;
  end Is_Azimuthal_Mount;


  function Maximum_Speed return Angle.Value is
  begin
    return Angle_List.Last_Element (The_Moving_Speeds);
  end Maximum_Speed;


  function Moving_Speeds return Angle.Values is
  begin
    return Angle.Values(Angle_List.Elements (The_Moving_Speeds));
  end Moving_Speeds;


  function First_Acceleration return Angle.Value is
  begin
    return The_First_Acceleration;
  end First_Acceleration;


  function Second_Acceleration return Angle.Value is
  begin
    return The_Second_Acceleration;
  end Second_Acceleration;


  function First_Lower_Limit return Angle.Degrees is
  begin
    return The_First_Lower_Limit;
  end First_Lower_Limit;


  function First_Upper_Limit return Angle.Degrees is
  begin
    return The_First_Upper_Limit;
  end First_Upper_Limit;


  function Second_Lower_Limit return Angle.Degrees is
  begin
    return The_Second_Lower_Limit;
  end Second_Lower_Limit;


  function Second_Upper_Limit return Angle.Degrees is
  begin
    return The_Second_Upper_Limit;
  end Second_Upper_Limit;


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


  function Satellites_Filename return String is
  begin
    return Text.String_Of (The_Satellites_Filename);
  end Satellites_Filename;

end Parameter;
