-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Motor;
with Os;
with Picture;
with Site;
with Strings;
with Text;
with Traces;
with User;

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
  Meridian_Flip_Offset_Key        : constant String := "Meridian Flip Offset";
  Moving_Speed_List_Key           : constant String := "Moving Speed List";
  First_Acceleration_Key          : constant String := "First Acceleration";
  Second_Acceleration_Key         : constant String := "Second Acceleration";
  Synch_On_Targets_Key            : constant String := "Synch On Targets";
  Expert_Mode_Key                 : constant String := "Expert Mode";

  Stellarium_Id  : constant String := "Stellarium";
  Lx200_Id       : constant String := "Lx200";
  Picture_Id     : constant String := "Picture";
  Filename_Key   : constant String := "Filename";
  Height_Key     : constant String := "Height";
  Width_Key      : constant String := "Width";
  Port_Key       : constant String := "Port";
  Program_Key    : constant String := "Program";
  Magnitude_Key  : constant String := "Magnitude";

  Degree_Unit : constant String := "°";

  The_Section : Configuration.Section_Handle;

  --Telescope
  Datagram_Port : constant := 44422;

  The_Telescope_Name       : Text.String;
  Is_Synch_On_Targets      : Boolean;
  Is_Expert_Mode           : Boolean;
  The_Steps_Per_Revolution : Device.Steps_Per_Revolution;
  The_Clocks_Per_Second    : Natural;
  The_Park_Azimuth         : Angle.Value;
  The_Park_Altitude        : Angle.Value;
  The_Meridian_Flip_Offset : Angle.Value;
  The_Moving_Speeds        : Angle_List.Item;
  The_First_Acceleration   : Angle.Value;
  The_Second_Acceleration  : Angle.Value;
  The_Telescope_Connection : Connection;

  --Lx200
  The_Lx200_Port : Network.Port_Number;

  --Stellarium
  The_Stellarium_Port   : Network.Port_Number;
  The_Magnitude_Maximum : Stellarium.Magnitude;


  function Default_Picture_Filename return String is
  begin
    case Os.Family is
    when Os.Osx =>
      return "/Users/" & Os.User_Name & "/Downloads/getframe.php.jpeg";
    when Os.Windows =>
      return "D:\temp\Picture.jpeg";
    when Os.Linux =>
      return "Picture.jpeg";
    end case;
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


  function Angle_Of (Key : String) return Angle.Degrees is
    Item : constant String := String_Of (Key);
  begin
    declare
      Image : constant String := Image_Of (Item, Degree_Unit);
    begin
      Log.Write (Key & ": " & Item);
      return Angle.Degrees'value(Image);
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


  function Number_Of (Key  : String) return Positive is
    Item : constant String := String_Of (Key);
  begin
    return Positive'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Number_Of;


  procedure Read is

    procedure Create_Default_Parameters is

      The_File : Ada.Text_IO.File_Type;

      procedure Put (Line : String) is
      begin
        Ada.Text_IO.Put_Line (The_File, Line);
      end Put;

      procedure Put_Steps_Per_Revolution (Steps            : Positive;
                                          Single_Parameter : Boolean := False) is
        Steps_Image : constant String := Positive'image(Steps);
      begin
        if Single_Parameter then
          Put (Steps_Per_Revolution_Key & " =" & Steps_Image);
        else
          Put (First_Steps_Per_Revolution_Key & "  =" & Steps_Image);
          Put (Second_Steps_Per_Revolution_Key & " =" & Steps_Image);
        end if;
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
      if Site.Is_Defined then
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                 = EQ6");
        Put (Ip_Address_Key & "           = SkyTracker" & (if Os.Is_Windows then "" else ".local"));
        Put_Steps_Per_Revolution (141 * 5 * 200 * 16, Single_Parameter => True); -- EQU-6
        Put (Clocks_Per_Second_Key & "    = 5000000");
        Put (Park_Azimuth_Key & "         = 170°12'00.0""");
        Put (Park_Altitude_Key & "        = 0°42'00.0""");
        Put (Meridian_Flip_Offset_Key & " = 10°");
        Put (Moving_Speed_List_Key & "    = 6""/s, 1'/s, 10'/s, 3°/s");
        Put (First_Acceleration_Key & "   = 2°/s²");
        Put (Second_Acceleration_Key & "  = 2°/s²");
        Put (Synch_On_Targets_Key & "     = True");
        Put (Expert_Mode_Key & "          = True");
      else
        Put ("[" & Telescope_Id & "]");
        Put (Name_Key & "                        = M-Zero");
        Put (Ip_Address_Key & "                  = None");
        Put_Steps_Per_Revolution (1232086);
        Put (Clocks_Per_Second_Key & "           = 5000000");
        Put (Park_Azimuth_Key & "                = 180°00'");
        Put (Park_Altitude_Key & "               = 0°00'");
        Put (Meridian_Flip_Offset_Key & "        = 30°");
        Put (Moving_Speed_List_Key & "           = 6""/s, 1'/s, 10'/s, 6°/s");
        Put (First_Acceleration_Key & "          = 3°/s²");
        Put (Second_Acceleration_Key & "         = 3°/s²");
        Put (Synch_On_Targets_Key & "            = True");
        Put (Expert_Mode_Key & "                 = False");
      end if;
      Put ("");
      Put ("[" & Lx200_Id & "]");
      Put (Port_Key & " = 4030");
      Put ("");
      Put ("[" & Picture_Id & "]");
      Put (Filename_Key & " = " & Default_Picture_Filename);
      if Site.Is_Defined then -- EQ6
        Put (Height_Key & "   = 0.66" & Degree_Unit);
        Put (Width_Key & "    = 1.00" & Degree_Unit);
      else -- M-Zero
        Put (Height_Key & "   = 2.97" & Degree_Unit);
        Put (Width_Key & "    = 4.46" & Degree_Unit);
      end if;
      Put ("");
      Put ("[" & Stellarium_Id & "]");
      Put (Port_Key & "      = 10001");
      case Os.Family is
      when Os.Windows =>
        Put (Program_Key & "   = C:\Program Files\Stellarium\Stellarium.exe");
      when Os.Osx =>
        Put (Program_Key & "   = /Applications/Stellarium.app/Contents/MacOS/stellarium");
      when Os.Linux =>
        null;
      end case;
      Put (Magnitude_Key & " = 8.0");
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
      Picture_Handle      : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Picture_Id);
      Stellarium_Handle   : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stellarium_Id);
      Localization_Handle : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Localization_Id);

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
            The_Telescope_Connection := (Kind => Is_Simulated);
            User.Show_Error ("Can't find the telescope " & Telescope);
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
            if Telescope /= "None" then
              Prepare_Udp (Telescope);
            end if;
          end;
        end if;
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
        if not Stellarium.Startup (Stellarium_Filename, Stellarium_Port) then
          Error.Raise_With ("Stellarium not started");
        end if;
      end Startup_Stellarium;

    begin -- Read_Values
      Set (Localization_Handle);
      Standard.Language.Define (Language);

      Set (Telescope_Handle);
      The_Telescope_Name := Text.String_Of (String_Value_Of ("Name"));
      Log.Write ("Name: " & Telescope_Name);
      Is_Expert_Mode := Strings.Lowercase_Of (String_Of (Expert_Mode_Key)) = "true";
      Connect_Telescope;
      The_Park_Azimuth := Angle_Of (Park_Azimuth_Key);
      The_Park_Altitude := Angle_Of (Park_Altitude_Key);
      The_Meridian_Flip_Offset := Angle_Of (Meridian_Flip_Offset_Key);
      The_Moving_Speeds := Angles_Of (Moving_Speed_List_Key, Speed_Unit);
      if Natural(Angle_List.Length (The_Moving_Speeds)) < 2 then
        Error.Raise_With ("The speed list must contain at least two values");
      end if;
      The_First_Acceleration := Angle_Of (First_Acceleration_Key, Acceleration_Unit);
      The_Second_Acceleration := Angle_Of (Second_Acceleration_Key, Acceleration_Unit);
      Is_Synch_On_Targets := Strings.Lowercase_Of (String_Of (Synch_On_Targets_Key)) = "true";

      Set (Lx200_Handle);
      begin
        The_Lx200_Port := Network.Port_Number (Value_Of (Port_Key));
        Log.Write ("Lx200 Port:" & The_Lx200_Port'img);
      exception
      when others =>
        Error.Raise_With ("Lx200 port number out of range");
      end;

      Set (Picture_Handle);
      Picture.Define (Name   => String_Of (Filename_Key),
                      Height => Angle_Of (Height_Key),
                      Width  => Angle_Of (Width_Key));

      Set (Stellarium_Handle);
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
    end Read_Values;

  begin -- Read
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;


  ---------------
  -- Telescope --
  ---------------

  function Telescope_Name return String is
  begin
    return Text.String_Of (The_Telescope_Name);
  end Telescope_Name;


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


  function Meridian_Flip_Offset return Angle.Value is
  begin
    return The_Meridian_Flip_Offset;
  end Meridian_Flip_Offset;


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


  function Synch_On_Targets return Boolean is
  begin
    return Is_Synch_On_Targets;
  end Synch_On_Targets;


  function Expert_Mode return Boolean is
  begin
    return Is_Expert_Mode;
  end Expert_Mode;


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


  function Magnitude_Maximum return Stellarium.Magnitude is
  begin
    return The_Magnitude_Maximum;
  end Magnitude_Maximum;

end Parameter;
