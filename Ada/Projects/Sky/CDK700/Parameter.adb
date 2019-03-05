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

with Ada.Numerics;
with Ada.Text_IO;
with Application;
with Astro;
with Configuration;
with Definite_Doubly_Linked_Lists;
with Error;
with File;
with Language;
with Mount;
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

  Telescope_Id            : constant String := "Telescope";
  Name_Key                : constant String := "Name";
  Ip_Address_Key          : constant String := "IP Address";
  Pole_Height_Key         : constant String := "Pole Height";
  Moving_Speed_List_Key   : constant String := "Moving Speed List";
  First_Acceleration_Key  : constant String := "First Acceleration";
  Second_Acceleration_Key : constant String := "Second Acceleration";

  Stellarium_Id : constant String := "Stellarium";
  Lx200_Id      : constant String := "Lx200";
  Port_Key      : constant String := "Port";

  Site_Id       : constant String := "Site";
  Longitude_Key : constant String := "Longitude";
  Latitude_Key  : constant String := "Latitude";
  Altitude_Key  : constant String := "Altitude";
  Sky_Line_Key  : constant String := "Sky Line";

  The_Section : Configuration.Section_Handle;

  The_Telescope_Name       : Text.String;
  Is_In_Setup_Mode         : Boolean;
  The_Pole_Height          : Angle.Value;
  The_Moving_Speeds        : Angle_List.Item;
  The_First_Acceleration   : Angle.Value;
  The_Second_Acceleration  : Angle.Value;
  The_Telescope_Connection : Connection;

  --Servers
  The_Lx200_Port      : Network.Port_Number;
  The_Stellarium_Port : Network.Port_Number;

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
      case Default_Location is
      when Home =>
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                        = Setup");
        Put (Ip_Address_Key & "                  = 127.0.0.1");
        Put (Pole_Height_Key & "                 = Latitude");
        Put (Moving_Speed_List_Key & "           = 6""/s, 1'/s, 10'/s, 6°/s");
        Put (First_Acceleration_Key & "          = 6°/s²");
        Put (Second_Acceleration_Key & "         = 6°/s²");
      when Sternwarte_Schaffhausen =>
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                               = CDK700");
        Put (Ip_Address_Key & "                         = 127.0.0.1");
        Put (Pole_Height_Key & "                        = 90°");
        Put (Moving_Speed_List_Key & "                  = 6""/s, 1'/s, 10'/s, 3°00'/s");
        Put (First_Acceleration_Key & "                 = 30'/s²");
        Put (Second_Acceleration_Key & "                = 30'/s²");
      when Unknown =>
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                        = ");
        Put (Ip_Address_Key & "                  = 127.0.0.1");
        Put (Pole_Height_Key & "                 = Latitude");
        Put (Moving_Speed_List_Key & "           = 6""/s, 1'/s, 10'/s, 6°/s");
        Put (First_Acceleration_Key & "          = 3°/s²");
        Put (Second_Acceleration_Key & "         = 3°/s²");
      end case;
      Put ("");
      Put ("[" & Lx200_Id & "]");
      Put (Port_Key & " = 4030");
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & " = 10001");
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

        procedure Prepare_Tcp (Telescope : String) is
          Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
          Server_Port     : constant Network.Port_Number := 8080;
          The_Ip_Address  : Network.Ip_Address;
        begin
          begin
            begin
              The_Ip_Address := Network.Ip_Address_Of (Telescope);
            exception
            when others =>
              The_Ip_Address := Network.Ip_Address_Of_Host (Telescope);
              Log.Write ("IP address of: " & Telescope & " = " & Network.Image_Of (The_Ip_Address));
            end;
            The_Telescope_Connection := (Socket => Network.Tcp.Socket_For (The_Address  => The_Ip_Address,
                                                                           The_Port     => Server_Port,
                                                                           The_Protocol => Socket_Protocol),
                                         Address => Network.Address_Of (The_Ip_Address, Server_Port));

          exception
          when others =>
            Error.Raise_With ("Can't find the telescope " & Telescope);
          end;
        end Prepare_Tcp;
      begin -- Connect_Telescope
        Prepare_Tcp (String_Of (Ip_Address_Key));
        Mount.Connect_Communication;
      end Connect_Telescope;

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
      The_Pole_Height := Pole_Heigth;
      The_Moving_Speeds := Angles_Of (Moving_Speed_List_Key, Speed_Unit);
      if Natural(Angle_List.Length (The_Moving_Speeds)) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      The_First_Acceleration := Angle_Of (First_Acceleration_Key, Acceleration_Unit);
      The_Second_Acceleration := Angle_Of (Second_Acceleration_Key, Acceleration_Unit);
      Set (Lx200_Handle);
      begin
        The_Lx200_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Lx200 Port:" & The_Lx200_Port'img);
      exception
      when others =>
        Error.Raise_With ("Lx200 port number out of range");
      end;
      Set (Stellarium_Handle);
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

  Home_Latitude  : constant Angle.Degrees := 47.695009;
  Home_Longitude : constant Angle.Degrees :=  8.627870;

  CDK700_Latitude  : constant Angle.Degrees := 47.705500;
  CDK700_Longitude : constant Angle.Degrees :=  8.609865;

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
    elsif Stellarium.Latitude = CDK700_Latitude and then Stellarium.Longitude = CDK700_Longitude then
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
