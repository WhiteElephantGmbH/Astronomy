-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Applications;
with Configuration;
with Directory;
with File;
with Network.Tcp.Servers;
with Os.Process;
with Site;
with Text;
with Unsigned;

package body Stellarium is

  package IO renames Ada.Text_IO;

  The_Magnitude_Maximum : Magnitude := 8.0; -- Default

  procedure Set_Maximum (Item : Magnitude) is
  begin
    The_Magnitude_Maximum := Item;
  end Set_Maximum;


  function Magnitude_Maximum return Magnitude is
  begin
    return The_Magnitude_Maximum;
  end Magnitude_Maximum;


  The_Set_Satellite_Group : Text.String;


  procedure Set_Satellite_Group (Name : String) is
  begin
    Log.Write ("Satellite Group: " & Name);
    The_Set_Satellite_Group := [Name];
  end Set_Satellite_Group;


  function Satellite_Group return String is
  begin
    return The_Set_Satellite_Group.S;
  end Satellite_Group;


  function Actual_Data_Directory return String is

    package Application is new Applications (Product => "stellarium"); -- lower case for Linux

    Path_Variable : constant String := "STELLARIUM_DATA_PATH";

  begin
    if Ada.Environment_Variables.Exists (Path_Variable) then
      declare
        Data_Directory : constant String := Ada.Environment_Variables.Value (Path_Variable);
      begin
        if not Directory.Exists (Data_Directory) then
          Log.Error ("Data Directory " & Data_Directory & " not found");
          return "";
        end if;
        Log.Write ("Data Directory " & Data_Directory & " used from " & Path_Variable);
        return Data_Directory;
      end;
    else
      return Application.Data_Directory;
    end if;
  end Actual_Data_Directory;


  Application_Data_Directory : constant String := Actual_Data_Directory;

  Configuration_Filename : constant String := File.Composure (Directory => Application_Data_Directory,
                                                              Filename  => "config",
                                                              Extension => "ini");

  Config_Handle : constant Configuration.File_Handle    := Configuration.Handle_For (Configuration_Filename);
  Init_Location : constant Configuration.Section_Handle := Configuration.Handle_For (Config_Handle, "init_location");
  Localization  : constant Configuration.Section_Handle := Configuration.Handle_For (Config_Handle, "localization");

  Landscape_Name : constant String := Configuration.Value_Of (Init_Location, "landscape_name");
  Location       : constant String := Configuration.Value_Of (Init_Location, "location");
  Last_Location  : constant String := Configuration.Value_Of (Init_Location, "last_location");

  Sky_Locale : constant String := Configuration.Value_Of (Localization, "sky_locale");

  use type File.Folder;

  Data_Directory : constant File.Folder := Application_Data_Directory + "data";

  User_Locations_Filename : constant String := File.Composure (Directory => Data_Directory,
                                                               Filename  => "user_locations",
                                                               Extension => "txt");

  Satellites_Directory : constant File.Folder := Application_Data_Directory + "modules" + "Satellites";

  Satellites_Json : constant String := File.Composure (Directory => Satellites_Directory,
                                                       Filename  => "satellites",
                                                       Extension => "json");

  Landscapes_Directory : constant String := Application_Data_Directory & File.Folder_Separator & "landscapes";
  Landscape_Directory  : constant File.Folder := Landscapes_Directory + Landscape_Name;

  function Landscape_Config_Name return String is
  begin
    declare
      Config_Name : constant String := File.Composure (Landscape_Directory, "landscape", "ini");
    begin
      if File.Exists (Config_Name) then
        Log.Write ("Landscape configuration: " & Config_Name);
        return Config_Name;
      else
        Log.Warning ("Landscape configuration " & Config_Name & " not defined by user");
        return "";
      end if;
    end;
  exception
  when others =>
    Log.Warning ("No landsape defined by user in " & Landscapes_Directory);
    return "";
  end Landscape_Config_Name;

  Landscape_Config  : constant Configuration.File_Handle := Configuration.Handle_For (Landscape_Config_Name);
  Landscape_Section : constant Configuration.Section_Handle := Configuration.Handle_For (Landscape_Config, "landscape");
  Location_Section  : constant Configuration.Section_Handle := Configuration.Handle_For (Landscape_Config, "location");


  function Satellites_Filename return String is
  begin
    if Satellites_Json /= "" and then File.Exists (Satellites_Json) then
      Log.Write ("Satellites filename: " & Satellites_Json);
      return Satellites_Json;
    else
      Log.Warning ("No satellites defined");
      return "";
    end if;
  end Satellites_Filename;


  function Search_Tolerance return Angle.Degrees is
  begin
    return The_Search_Tolerance;
  end Search_Tolerance;


  procedure Read_Location is

    function Value_Of (Image        : String;
                       Is_Positive  : Boolean := True) return Angle.Value is

      The_Value : Angle.Degrees := Angle.Degrees'value(Image);

      use type Angle.Degrees;
      use type Angle.Value;

    begin
      if not Is_Positive then
        The_Value := -The_Value;
      end if;
      return +The_Value;
    end Value_Of;


    function Is_Automatic_Defined_Location return Boolean is
    begin
      if Location = "auto" and then Last_Location /= "" then
        declare
          Items           : constant Text.Strings := Text.Strings_Of (Last_Location, Separator => ',');
          Latitude_Image  : constant String       := Items(Text.First_Index);
          Longitude_Image : constant String       := Items(Text.First_Index + 1);
        begin
          Site.Define (Site.Data'(Latitude  => Value_Of (Latitude_Image),
                                  Longitude => Value_Of (Longitude_Image),
                                  Elevation => 0));
        end;
        Log.Write ("automatic location => " & Last_Location);
        return True;
      end if;
      return False;
    exception
    when others =>
      Log.Error ("automatic location handling failed");
      return False;
    end Is_Automatic_Defined_Location;


    function Is_Location_From_Landscape return Boolean is
      Latitude  : constant String := Configuration.Value_Of (Location_Section, "latitude");
      Longitude : constant String := Configuration.Value_Of (Location_Section, "longitude");
      Altitude  : constant String := Configuration.Value_Of (Location_Section, "altitude");
    begin
      if Latitude /= "" and then Longitude /= "" and then Altitude /= "" then
        Site.Define (Site.Data'(Latitude  => Angle.Value_Of (Latitude),
                                Longitude => Angle.Value_Of (Longitude),
                                Elevation => Integer'value(Altitude)));
        return True;
      end if;
      return False;
    exception
    when others =>
      Log.Error ("landscape location handling failed");
      return False;
    end Is_Location_From_Landscape;


    function Is_User_Defined_Location return Boolean is
      The_File : IO.File_Type;
    begin
      IO.Open (File => The_File,
               Mode => IO.In_File,
               Name => User_Locations_Filename);
      while not IO.End_Of_File (The_File) loop
        declare
          Items   : constant Text.Strings := Text.Strings_Of (IO.Get_Line (The_File), Separator => Ascii.Ht);
          Place   : constant String       := Items(Text.First_Index);
          Country : constant String       := Items(Text.First_Index + 2);
        begin
          if Location = Place & ", " & Country then
            declare
              Lat_Image : constant String := Items(Text.First_Index + 5);
              Lon_Image : constant String := Items(Text.First_Index + 6);
              Alt_Image : constant String := Items(Text.First_Index + 7);
            begin
              Site.Define (Site.Data'(Latitude  => Value_Of (Lat_Image(Lat_Image'first .. Lat_Image'last - 1),
                                                             Is_Positive => Lat_Image(Lat_Image'last) = 'N'),
                                      Longitude => Value_Of (Lon_Image(Lon_Image'first .. Lon_Image'last - 1),
                                                             Is_Positive => Lon_Image(Lon_Image'last) = 'E'),
                                      Elevation => Integer'value(Alt_Image)));
            end;
            Log.Write ("user location => " & Location);
            IO.Close (The_File);
            return True;
          end if;
        end;
      end loop;
      IO.Close (The_File);
      return False;
    exception
    when IO.Name_Error =>
      Log.Warning (User_Locations_Filename & " not found");
      return False;
    when others =>
      Log.Error ("user location handling failed");
      return False;
    end Is_User_Defined_Location;

   begin -- Read_Location
    if Is_Automatic_Defined_Location then
      return;
    elsif Is_Location_From_Landscape then
      return;
    elsif Is_User_Defined_Location then
      return;
    end if;
    if Last_Location /= "" then
      Log.Write ("location Paris => " & Last_Location);
    else
      Log.Write ("Location undefined");
    end if;
  end Read_Location;


  The_Process_Id : Os.Process.Id;

  function Startup (Filename : String;
                    The_Port : Network.Port_Number) return Boolean is

    The_Listener : Network.Tcp.Listener_Socket;
    The_Client   : Network.Tcp.Socket;
    The_Address  : Network.Ip_Address;

  begin
    begin
      Network.Tcp.Create_Socket_For (The_Port, Network.Tcp.Raw, The_Listener);
      begin
        Network.Tcp.Accept_Client_From (The_Listener   => The_Listener,
                                        The_Client     => The_Client,
                                        Client_Address => The_Address,
                                        The_Timeout    => 1.0);
      exception
      when others =>
        Network.Tcp.Close (The_Listener);
        raise; -- not started
      end;
      Network.Tcp.Close (The_Client);
      Network.Tcp.Close (The_Listener);
      return True; -- already started
    exception
    when others =>
      null; -- not started
    end;
    The_Process_Id := Os.Process.Created (Filename);
    return True;
  exception
  when others =>
    return False;
  end Startup;


  procedure Shutdown is
  begin
    Os.Process.Terminate_With (The_Process_Id);
  exception
  when others =>
    Log.Write ("already terminated");
  end Shutdown;


  function Landscape_Filename return String is
    Name : constant String := Configuration.Value_Of (Landscape_Section, "maptex");
  begin
    return File.Composure (Landscape_Directory, Name);
  end Landscape_Filename;


  function Landscape_Rotation return Angle.Degrees is
    Rotation : constant String := Configuration.Value_Of (Landscape_Section, "angle_rotatez");
  begin
    return Angle.Degrees'value(Rotation);
  exception
  when others =>
    Log.Error ("Incorrect landscape rotation angle (assumed 0.0)");
    return 0.0;
  end Landscape_Rotation;


  function Language return Standard.Language.Kind is
  begin
    if Sky_Locale'length > 1 then
      declare
        Id : constant String := Sky_Locale(Sky_Locale'first .. Sky_Locale'first + 1);
      begin
        if Id = "de" then
          return Standard.Language.German;
        elsif Id = "fr" then
          return Standard.Language.French;
        elsif Id = "el" then
          return Standard.Language.Greek;
        elsif Id = "it" then
          return Standard.Language.Italian;
        elsif Id = "es" then
          return Standard.Language.Spanish;
        end if;
      end;
    end if;
    return Standard.Language.English;
  end Language;


  use type Angle.Signed;

  Right_Angle : constant Angle.Signed := +Angle.Quadrant;

  subtype Declination     is Angle.Signed range -Right_Angle .. Right_Angle;
  subtype Right_Ascention is Angle.Unsigned;

  type Send_Message is record
    Kind   : Unsigned.Word;
    Time   : Unsigned.Quadword;
    Ra     : Right_Ascention;
    Dec    : Declination;
    Status : Unsigned.Longword;
  end record
  with
    Convention => C,
    Pack       => True;

  type Receive_Message is record
    Unused_Kind : Unsigned.Word;
    Unused_Time : Unsigned.Quadword;
    Ra          : Right_Ascention;
    Dec         : Declination;
  end record
  with
    Convention => C,
    Pack       => True;

  package Server is new Network.Tcp.Servers (Outgoing     => Send_Message,
                                             Incoming     => Receive_Message,
                                             The_Protocol => Network.Tcp.LE16_Included);
  Goto_Location : Goto_Handler;

  procedure Define_Handler (The_Handler : Goto_Handler) is
  begin
    Goto_Location := The_Handler;
  end Define_Handler;


  procedure Message_Handler (The_Message : Receive_Message) is
    use type Angle.Value;
  begin
    if Goto_Location /= null then
      Goto_Location (Space.Direction_Of (Ra  => Angle.Value'(+The_Message.Ra),
                                         Dec => Angle.Value'(+The_Message.Dec)));
    end if;
  end Message_Handler;


  procedure Start is
  begin
    Log.Write ("start - port:" & The_Port_Number'img);
    Server.Start (Message_Handler'access, The_Port_Number);
  end Start;


  function Port_Number return Network.Port_Number is
  begin
    return The_Port_Number;
  end Port_Number;


  procedure Set (Direction : Space.Direction) is
    use type Angle.Value;
  begin
    declare
      Current_Location : constant Send_Message
        := (Kind   => 0,
            Time   => 0,
            Ra     => +Space.Ra_Of (Direction),
            Dec    => +Space.Dec_Of (Direction),
            Status => 0);
    begin
      Server.Send (Current_Location);
    end;
  exception
  when others =>
    null;
  end Set;


  procedure Close is
  begin
    Server.Close;
    Log.Write ("end");
  end Close;

begin
  Read_Location;
end Stellarium;
