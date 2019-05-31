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

with Ada.Text_IO;
with Configuration;
with File;
with Network.Tcp.Servers;
with Os.Process;
with Strings;
with Traces;
with Unsigned;

package body Stellarium is

  package Log is new Traces ("Stellarium");

  package IO renames Ada.Text_IO;

  Configuration_Filename : constant String := Application.Composure ("config", "ini");

  Config_Handle : constant Configuration.File_Handle    := Configuration.Handle_For (Configuration_Filename);
  Init_Location : constant Configuration.Section_Handle := Configuration.Handle_For (Config_Handle, "init_location");
  Localization  : constant Configuration.Section_Handle := Configuration.Handle_For (Config_Handle, "localization");

  Location       : constant String := Configuration.Value_Of (Init_Location, "location");
  Last_Location  : constant String := Configuration.Value_Of (Init_Location, "last_location");

  Sky_Locale : constant String := Configuration.Value_Of (Localization, "sky_locale");

  Data_Directory : constant String := Application.Composure ("data");

  User_Locations_Filename : constant String := File.Composure (Directory => Data_Directory,
                                                               Filename  => "user_locations",
                                                               Extension => "txt");

  use type File.Folder;

  Satellites_Directory : constant File.Folder := Application.Data_Directory + "modules" + "Satellites";

  Satellites_Json : constant String := File.Composure (Directory => Satellites_Directory,
                                                       Filename  => "satellites",
                                                       Extension => "json");

  Paris_Altitude  : constant Natural       := 42;
  Paris_Latitude  : constant Angle.Degrees := 48.8534083333;
  Paris_Longitude : constant Angle.Degrees := 2.4488;

  The_Altitude  : Integer       := Paris_Altitude;
  The_Latitude  : Angle.Degrees := Paris_Latitude;
  The_Longitude : Angle.Degrees := Paris_Longitude;


  function Satellites_Filename return String is
  begin
    return Satellites_Json;
  end Satellites_Filename;


  procedure Read_Location is

    function Value_Of (Image        : String;
                       Is_Positive  : Boolean := True) return Angle.Degrees is

      The_Value : Angle.Degrees := Angle.Degrees'value(Image);

      use type Angle.Degrees;

    begin
      if not Is_Positive then
        The_Value := -The_Value;
      end if;
      return The_Value;
    end Value_Of;


    function Is_Automatic_Defined_Location return Boolean is
    begin
      if Location = "auto" and then Last_Location /= "" then
        declare
          Items           : constant Strings.Item := Strings.Item_Of (Last_Location, Separator => ',');
          Latitude_Image  : constant String       := Items(Strings.First_Index);
          Longitude_Image : constant String       := Items(Strings.First_Index + 1);
        begin
          The_Latitude := Value_Of (Latitude_Image);
          The_Longitude := Value_Of (Longitude_Image);
          The_Altitude := 0;
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


    function Is_User_Defined_Location return Boolean is
      The_File : IO.File_Type;
    begin
      IO.Open (File => The_File,
               Mode => IO.In_File,
               Name => User_Locations_Filename);
      while not IO.End_Of_File (The_File) loop
        declare
          Items   : constant Strings.Item := Strings.Item_Of (IO.Get_Line (The_File), Separator => Ascii.Ht);
          Place   : constant String       := Items(Strings.First_Index);
          Country : constant String       := Items(Strings.First_Index + 2);
        begin
          if Location = Place & ", " & Country then
            declare
              Latitude_Image  : constant String := Items(Strings.First_Index + 5);
              Longitude_Image : constant String := Items(Strings.First_Index + 6);
              Altitude_Image  : constant String := Items(Strings.First_Index + 7);
            begin
              The_Latitude := Value_Of (Latitude_Image(Latitude_Image'first .. Latitude_Image'last - 1),
                                        Is_Positive => Latitude_Image(Latitude_Image'last) = 'N');
              The_Longitude := Value_Of (Longitude_Image(Longitude_Image'first .. Longitude_Image'last - 1),
                                         Is_Positive => Longitude_Image(Longitude_Image'last) = 'E');
              The_Altitude := Integer'value(Altitude_Image);
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
    elsif Is_User_Defined_Location then
      return;
    end if;
    Log.Write ("location Paris => " & Last_Location);
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
    Os.Process.Create (Filename, The_Process_Id);
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


  function Altitude return Integer is
  begin
    return The_Altitude;
  end Altitude;


  function Latitude return Angle.Degrees is
  begin
    return The_Latitude;
  end Latitude;


  function Longitude return Angle.Degrees is
  begin
    return The_Longitude;
  end Longitude;


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


  procedure Start (Used_Port : Port_Number) is
  begin
    Log.Write ("start - port:" & Used_Port'img);
    Server.Start (Message_Handler'access, Used_Port);
  end Start;


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
