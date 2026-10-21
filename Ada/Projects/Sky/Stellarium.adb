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
pragma Style_Astronomy;

with Ada.Environment_Variables;
with Ada.Text_IO;
with Applications;
with Configuration;
with Directory;
with Error;
with File;
with Network.Tcp.Servers;
with Os.Process;
with Site;
with Stellarium.Client;
with Time;
with Unsigned;

package body Stellarium is

  package IO renames Ada.Text_IO;


  function Actual_Data_Directory return String is

    package Application is new Applications (Product => "stellarium"); -- lower case for Linux

    Path_Variable : constant String := "STELLARIUM_DATA_PATH";

  begin
    if Ada.Environment_Variables.Exists (Path_Variable) then
      declare
        Data_Directory : constant String := Ada.Environment_Variables.Value (Path_Variable);
      begin
        if not Directory.Exists (Data_Directory) then
          Log.Error ("data directory " & Data_Directory & " not found");
          return "";
        end if;
        Log.Write ("data directory " & Data_Directory & " used from " & Path_Variable);
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

  Landscapes_Directory : constant String := Application_Data_Directory & File.Folder_Separator & "landscapes";
  Landscape_Directory  : constant File.Folder := Landscapes_Directory + Landscape_Name;


  function Telescope_Location return Site.Telescope_Location is
    Name : constant String := Text.Uppercase_Of (Landscape_Name);
  begin
    if Name in "CDK 1" | "CDK OST" then
      return Site.Cdk_East;
    elsif Name in "CDK 2" | "CDK WEST" then
      return Site.Cdk_West;
    elsif Name in "APO" then
      return Site.Apo;
    elsif Name in "HOME" | "BASADINGEN" then
      return Site.Home;
    else
      return Site.Unknown;
    end if;
  end Telescope_Location;


  function Landscape_Config_Name return String is
  begin
    declare
      Config_Name : constant String := File.Composure (Landscape_Directory, "landscape", "ini");
    begin
      if File.Exists (Config_Name) then
        Log.Write ("landscape configuration: " & Config_Name);
        return Config_Name;
      else
        Log.Warning ("landscape configuration " & Config_Name & " not defined by user");
        return "";
      end if;
    end;
  exception
  when others =>
    Log.Warning ("No landscape defined by user in " & Landscapes_Directory);
    return "";
  end Landscape_Config_Name;


  Landscape_Config  : constant Configuration.File_Handle := Configuration.Handle_For (Landscape_Config_Name);
  Landscape_Section : constant Configuration.Section_Handle := Configuration.Handle_For (Landscape_Config, "landscape");
  Location_Section  : constant Configuration.Section_Handle := Configuration.Handle_For (Landscape_Config, "location");

  Modules_Directory : constant String := Application_Data_Directory & File.Folder_Separator & "modules";
  Oculars_Directory : constant File.Folder := Modules_Directory + "Oculars";

  function Ocular_Config_Name return String is
  begin
    declare
      Config_Name : constant String := File.Composure (Oculars_Directory, "ocular", "ini");
    begin
      if File.Exists (Config_Name) then
        Log.Write ("ocular configuration: " & Config_Name);
        return Config_Name;
      else
        Log.Warning ("ocular configuration " & Config_Name & " not defined");
        return "";
      end if;
    end;
  exception
  when others =>
    Log.Warning ("Modules Oculars not found");
    return "";
  end Ocular_Config_Name;


  Ocular_Config  : constant Configuration.File_Handle := Configuration.Handle_For (Ocular_Config_Name);
  CCD_Section    : constant Configuration.Section_Handle := Configuration.Handle_For (Ocular_Config, "ccd");
  Lens_Section   : constant Configuration.Section_Handle := Configuration.Handle_For (Ocular_Config, "lens");
  Ocular_Section : constant Configuration.Section_Handle := Configuration.Handle_For (Ocular_Config, "ocular");
  Tele_Section   : constant Configuration.Section_Handle := Configuration.Handle_For (Ocular_Config, "telescope");

  function Prefix (Index : Property_Id) return String is (Text.Trimmed (Index'image) & '\');


  function Camera (Index : Property_Id) return Camera_Data is
    function Value_Of (Item : String) return String is (Configuration.Value_Of (CCD_Section, Prefix (Index) & Item));
    Name  : constant String := Value_Of ("name");
    Res_X : constant String := Value_Of ("resolutionX");
    Res_Y : constant String := Value_Of ("resolutionY");
    Bin_X : constant String := Value_Of ("binningX");
    Bin_Y : constant String := Value_Of ("binningY");
    Dim_X : constant String := Value_Of ("chip_width");
    Dim_Y : constant String := Value_Of ("chip_height");
  begin
    return (Name       => [Name],
            Resolution => (X => Units.Pixel_Count'value(Res_X), Y => Units.Pixel_Count'value(Res_Y)),
            Binning    => (X => Units.Pixel_Binning'value(Bin_X), Y => Units.Pixel_Binning'value(Bin_Y)),
            Dimension  => (X => Units.Chip_Size'value(Dim_X), Y => Units.Chip_Size'value(Dim_Y)));
  end Camera;


  function Lens (Index : Property_Id) return Lens_Data is
    function Value_Of (Item : String) return String is (Configuration.Value_Of (Lens_Section, Prefix (Index) & Item));
    Name       : constant String := Value_Of ("name");
    Multiplier : constant String := Value_Of ("multipler");
  begin
    return (Name       => [Name],
            Multiplier => Units.Lens_Multiplier'value(Multiplier));
  end Lens;


  function Ocular (Index : Property_Id) return Ocular_Data is
    function Value_Of (Item : String) return String is (Configuration.Value_Of (Ocular_Section, Prefix (Index) & Item));
    Name         : constant String := Value_Of ("name");
    Apparent_FOV : constant String := Value_Of ("afov");
  begin
    return (Name         => [Name],
            Apparent_FOV => Units.Field_Of_View'value(Apparent_FOV));
  end Ocular;


  function Telescope (Index : Property_Id) return Telescope_Data is
    function Value_Of (Item : String) return String is (Configuration.Value_Of (Tele_Section, Prefix (Index) & Item));
    Name         : constant String := Value_Of ("name");
    Focal_Length : constant String := Value_Of ("focalLength");
  begin
    return (Name         => [Name],
            Focal_Length => Units.Focal_Length'value(Focal_Length));
  end Telescope;


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
          Site.Define (Site.Data'(Location  => Site.Unknown,
                                  Latitude  => Value_Of (Latitude_Image),
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
        Site.Define (Site.Data'(Location  => Telescope_Location,
                                Latitude  => Angle.Value_Of (Latitude),
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
              Site.Define (Site.Data'(Location  => Telescope_Location,
                                      Latitude  => Value_Of (Lat_Image(Lat_Image'first .. Lat_Image'last - 1),
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


  function Port_Number return Network.Port_Number is
  begin
    return The_Port_Number;
  end Port_Number;


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


  The_Process_Id : Os.Process.Id;

  function Is_Started return Boolean is

    The_Listener : Network.Tcp.Listener_Socket;
    The_Client   : Network.Tcp.Socket;
    The_Address  : Network.Ip_Address;

  begin
    begin
      Network.Tcp.Create_Socket_For (The_Port_Number, Network.Tcp.Raw, The_Listener);
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
      Log.Write ("already started");
      return True;
    exception
    when others =>
      null;
    end;
    Log.Write ("start");
    The_Process_Id := Os.Process.Created (The_Filename.S);
    Time.Wait (1.0); -- wait for creation
    return True;
  exception
  when others =>
    return False;
  end Is_Started;


  procedure Startup is
    Filename : constant String := The_Filename.S;
  begin
    if Filename /= "" then
      Log.Write ("program file: """ & Filename & """");
      if not File.Exists (Filename) then
        Error.Raise_With ("Stellarium program file """ & Filename & """ not found");
      end if;
      if not Is_Started then
        Error.Raise_With ("Stellarium not started");
      end if;
    end if;
    begin
      Server.Start (Message_Handler'access, The_Port_Number);
      Log.Write ("start server - port:" & The_Port_Number'img);
    exception
    when Network.Tcp.Port_In_Use =>
      Error.Raise_With ("Stellarion - TCP port" & Stellarium.Port_Number'img & " in use");
    when others =>
      Error.Raise_With ("Stellarion - could not start server");
    end;
  end Startup;


  procedure Shutdown is
  begin
    Log.Write ("shutdown");
    Server.Close; -- noop if never started
    Os.Process.Terminate_With (The_Process_Id);
  exception
  when others =>
    Log.Write ("already terminated");
  end Shutdown;


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


  function Get return Target_Data renames Client.Get_Target;

  procedure Set_Focus (Target_Name : String;
                       Mode        : Focus_Mode := Mark) is
  begin
    Client.Set_Focus_To (Target_Name, Mode);
  end Set_Focus;


  procedure Center_On (Target : Name.Id) is
    Target_Name : constant String := Name.Image_For_Stellarium_Of (Target);
  begin
    if Target_Name /= "" then
      Client.Set_Focus_To (Target_Name, Center);
    end if;
  exception
  when others =>
    null;
  end Center_On;


  function Camera_Name return String is
  begin
    return Client.Get.Camera.Name.S;
  end Camera_Name;


  function Camera_Model return Units.Camera_Model is
    Name : constant String := Camera_Name;
  begin
    if Text.Location_Of ("EOS 6D (", Name) /= Text.Not_Found then -- not EOS 6D II
      return Units.Canon_Eos_6D;
    elsif Text.Location_Of ("EOS 60D", Name) /= Text.Not_Found then
      return Units.Canon_Eos_60D;
    elsif Text.Location_Of ("QHY600C", Name) /= Text.Not_Found then
      return Units.QHY600C;
    end if;
    return Units.Unknown;
  end Camera_Model;


  procedure Set_Camera (Model : Units.Camera_Model) is
    use type Units.Camera_Model;
  begin
   Log.Write ("Set Camera: " & Model'image);
   for Id in Property_Id loop
      Client.Set_CCD (Id);
      if Model = Camera_Model then
        return;
      end if;
    end loop;
    Log.Warning ("Camera " & Model'image & " not found");
    raise Not_Found;
  exception
  when Not_Found =>
    raise;
  when others =>
    raise Operation_Failed;
  end Set_Camera;


  function Chip_Resolution return Units.Chip_Resolution is
  begin
    return Client.Get.Camera.Resolution;
  end Chip_Resolution;


  function Chip_Size return Units.Chip_Dimension is
  begin
    return Client.Get.Camera.Dimension;
  end Chip_Size;


  function Chip_Binning return Units.Chip_Binning is
  begin
    return Client.Get.Camera.Binning;
  end Chip_Binning;


  function Pixel_Size return Units.Pixel_Size is
    Data   : constant Camera_Data := Client.Get.Camera;
    Factor : constant Float := 10.0**3; -- mm / um
    X_Size : constant Float := Factor * Float(Data.Dimension.X) / Float(Data.Resolution.X);
    Y_Size : constant Float := Factor * Float(Data.Dimension.Y) / Float(Data.Resolution.Y);
    Mean   : constant Float := (X_Size + Y_Size) / 2.0;
    Offset : constant Float := Float(Units.Pixel_Size'small) / 2.0;
  begin
    if abs (X_Size - Y_Size) > Float(Units.Pixel_Size'small) then
      Log.Write ("X_Size:" & X_Size'image);
      Log.Write ("Y_Size:" & Y_Size'image);
      Log.Warning ("Pixel not square");
    end if;
    return Units.Pixel_Size(Mean + Offset);
  end Pixel_Size;


  function Camera_Rotation return Units.Rotation is
  begin
    return Client.Get.Rotation;
  end Camera_Rotation;


  procedure Set_Camera_Rotation (Value : Units.Rotation) is
  begin
    Client.Set_Rotation (Value);
  end Set_Camera_Rotation;


  function Lens_Name return String is
  begin
    return Client.Get.Lens.Name.S;
  end Lens_Name;


  procedure Set_No_Lens is
  begin
    Client.Set_Lens (Not_Available);
  end Set_No_Lens;


  function Lens_Multiplier return Units.Lens_Multiplier is
  begin
    return Client.Get.Lens.Multiplier;
  end Lens_Multiplier;


  function Telescope_Name return String is
  begin
    return Client.Get.Telescope.Name.S;
  end Telescope_Name;


  function Telescope_Model return Units.Telescope_Model is
    Name : constant String := Telescope_Name;
  begin
    if Text.Location_Of ("Celestron C11", Name) /= Text.Not_Found then
      return Units.Celestron_C11;
    elsif Text.Location_Of ("Celestron C14", Name) /= Text.Not_Found then
      return Units.Celestron_C14;
    elsif Text.Location_Of ("CDK_700", Name) /= Text.Not_Found then
      return Units.CDK_700;
    end if;
    return Units.Unknown;
  end Telescope_Model;


  procedure Set_Telescope (Model : Units.Telescope_Model) is
    use type Units.Telescope_Model;
  begin
   Log.Write ("Set Telescope: " & Model'image);
   for Id in Property_Id loop
      Client.Set_Telescope (Id);
      if Model = Telescope_Model then
        return;
      end if;
    end loop;
    raise Not_Found;
  exception
  when Not_Found =>
    Log.Warning ("Telescope " & Model'image & " not found");
    raise;
  when others =>
    raise Operation_Failed;
  end Set_Telescope;


  function Telescope_Focal_Length return Units.Focal_Length is
  begin
    return Client.Get.Telescope.Focal_Length;
  end Telescope_Focal_Length;

begin
  Read_Location;
end Stellarium;
