-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Error;
with Network.Tcp;
with Satellite;
with Text;

package body Ten_Micron is

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

  Version_Delta : constant := 0.0001;

  type Firmware_Version is delta Version_Delta range 0.0000 .. 99.9999 with Small => Version_Delta;

  The_Firmware_Version : Firmware_Version := 0.0;

  Receive_Timeout  : constant Duration := 3.0;
  Flush_Timeout    : constant Duration := 0.5;
  Timeout_Detected : Boolean := False;

  The_Socket : Network.Tcp.Socket;
  The_Status : State := Disconnected;
  Last_State : State := Disconnected;

  Has_New_Alignment : Boolean := False;
  Is_Legacy         : Boolean := False;

  Actual_Rates : Tracking_Rate_Factors;


  function Is_Expert_Mode return Boolean is
  begin
    return Is_In_Expert_Mode;
  end Is_Expert_Mode;


  procedure Set_Status (Item : State) is
  begin
    Last_State := Item;
    case The_Status is
    when Capturing | Solving =>
      case Item is
      when Positioned | Tracking =>
        return;
      when others =>
        null;
      end case;
    when others =>
      null;
    end case;
    if The_Status /= Item then
      The_Status := Item;
      Log.Write ("Status: " & Item'image);
    end if;
  end Set_Status;


  procedure Disconnect_Device (Message : String) with No_Return is
  begin
    Has_New_Alignment := False;
    begin
      Network.Tcp.Close (The_Socket);
    exception
    when others =>
      null; -- ignore when disconnected
    end;
    Set_Status (Disconnected);
    Error.Raise_With (Message);
  end Disconnect_Device;


  function Not_Connected return Boolean is
  begin
    if The_Status = Disconnected then
      Log.Warning ("not connected");
      return True;
    end if;
    return False;
  end Not_Connected;


  function Received_String (Log_Enabled : Boolean := True) return String is
    Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Lx200.Terminator);
  begin
    if Log_Enabled or Timeout_Detected then
      Log.Write ("Reply " & Reply);
    end if;
    return Reply (Reply'first .. Reply'last - 1);
  end Received_String;


  function Received_Character return String is
    Item : constant Character := Network.Tcp.Raw_Character_From (The_Socket);
  begin
    Log.Write ("Reply " & Item);
    return "" & Item;
  end Received_Character;


  procedure Send (The_Command : String;
                  Log_Enabled : Boolean := True) is
  begin
    if Log_Enabled or Timeout_Detected then
      Log.Write ("Command " & The_Command);
    end if;
    Network.Tcp.Send (The_String  => The_Command,
                      Used_Socket => The_Socket);
  exception
  when Network.Tcp.No_Client =>
    Disconnect_Device ("no network client");
  when Network.Transmission_Error =>
    Disconnect_Device ("network transmission error");
  end Send;


  procedure Flush_Input is
  begin
    Send (Lx200.Flush_Input);
    declare
      Flushed : constant String := Network.Tcp.Raw_String_From (Used_Socket     => The_Socket,
                                                                Terminator      => Lx200.Terminator,
                                                                Receive_Timeout => Flush_Timeout);
    begin
      Log.Warning ("Flushed: " & Flushed);
    end;
  exception
  when others =>
    null;
  end Flush_Input;


  procedure Set_Device_Status is

    procedure Set_Transit_Status (The_State : in out State) is
    begin
      Send (Lx200.String_Of(Lx200.Get_Transit_Status), Log_Enabled => False);
      declare
        Reply : constant String := Received_String (Log_Enabled => False);
      begin
        if Reply'length = 1 then
          case Reply(Reply'first) is
          when 'V' =>
            The_State := Preparing;
            return;
          when 'P' =>
            The_State := Waiting;
            return;
          when 'S' =>
            The_State := Catching;
            return;
          when 'T' =>
            return; -- allready following
          when 'Q' =>
            The_State := Ended;
            return;
          when 'E' =>
            return; -- allready stopped
          when others =>
            null;
          end case;
        end if;
        Log.Warning ("Unexpected Transient State: " & Reply);
      end;
    end Set_Transit_Status;

  begin -- Set_Device_Status
    Send (Lx200.String_Of(Lx200.Get_Status), Log_Enabled => False);
    declare
      Reply : constant String := Received_String (Log_Enabled => False);
    begin
      declare
        State_Number : constant Natural := Natural'value(Reply);
        The_State    : State;
      begin
        case State_Number is
        when 98 =>
          The_State := Unknown;
        when 99 =>
          The_State := Failure;
        when others =>
          if State_Number > 11 then
            Log.Warning ("Overloaded State");
            The_State := Failure;
          else
            The_State := State'val(State_Number);
            if The_State = Following then
              Set_Transit_Status (The_State);
            end if;
          end if;
        end case;
        Set_Status (The_State);
      end;
      return;
    exception
    when others =>
      Disconnect_Device ("Unknown device status <" & Reply & ">");
    end;
  exception
  when Network.Tcp.No_Client =>
    Disconnect_Device ("Set_Device_Status - no client");
  when Network.Timeout =>
    Log.Warning ("Reply status timeout");
    Timeout_Detected := True;
    Flush_Input;
  end Set_Device_Status;


  procedure Set_Ultra_Precision_Mode is
  begin
    Send (Lx200.String_Of(Lx200.Set_Ultra_Precision_Mode));
  end Set_Ultra_Precision_Mode;


  function Reply_For (The_Command : Lx200.Extended_Command;
                      Parameter   : String := "") return String is
    use all type Lx200.Command;

    Log_Enabled : constant Boolean := not (The_Command in Get_Declination
                                                        | Get_Right_Ascension
                                                        | Get_Axis_Dec_Position
                                                        | Get_Axis_RA_Position
                                                        | Get_Air_Pressure
                                                        | Get_Temperature
                                                        | Get_Julian_Date
                                                        | Get_Pointing_State);

  begin
    Send (Lx200.String_Of (The_Command, Parameter), Log_Enabled);
    begin
      case The_Command is
      when Slew
         | Slew_To_Axis_Position
      =>
        declare
          Reply : constant String := Received_Character;
        begin
          if Reply /= Lx200.Slew_Ok then
            Log.Warning (Received_String);
            return Lx200.Slew_Ok; -- simulate ok
          end if;
          return Reply;
        end;
      when Set_Julian_Date
         | Set_Local_Time
         | Set_Time_Offset
         | Set_Altitude
         | Set_Azimuth
         | Set_Axis_RA_Position
         | Set_Axis_Dec_Position
         | Set_Right_Ascension
         | Set_Declination
         | Set_Air_Pressure
         | Set_Temperature
         | Set_Tracking_Rate_Dec_Factor
         | Set_Tracking_Rate_RA_Factor
      =>
        return Received_Character;
      when Get_Alignment_Information
         | Get_Product_Name
         | Get_Firmware_Number
         | Get_Firmware_Date
         | Get_Alignment_Status
         | Get_Latitude
         | Get_Longitude
         | Get_Sideral_Time
         | Get_Status
         | Get_Transit_Status
         | Get_Axis_RA_Position
         | Get_Axis_Dec_Position
         | Get_Air_Pressure
         | Get_Temperature
         | Get_Julian_Date
         | Get_Maximum_Slew_Rate
         | Get_Number_Of_Alignment_Stars
         | Get_Pointing_State
         | Get_Right_Ascension
         | Get_Declination
         | Get_Altitude
         | Get_Azimuth
         | Gps_Test_Synchronized
         | New_Alignment_Start
         | New_Alignment_Point
         | New_Alignment_End
         | Set_Latitude
         | Set_Longitude
         | Synchronize
         | Tle_Load_Satellite
         | Tle_Precalculate
         | Tle_Slew
         | Trajectory_Offset_Add
       =>
        return Received_String (Log_Enabled);
      when Set_Ultra_Precision_Mode
         | Stop
         | Slew_To_Park_Position
         | Unpark
         | Move_East
         | Move_North
         | Move_South
         | Move_West
         | Quit_Move_East
         | Quit_Move_North
         | Quit_Move_South
         | Quit_Move_West
         | Set_Lunar_Tracking_Rate
         | Set_Solar_Tracking_Rate
         | Set_Sideral_Tracking_Rate
         | Set_Guiding_Rate
         | Set_Centering_Rate
         | Set_Finding_Rate
         | Set_Slewing_Rate
         | Set_Centering_Rate_Factor
         | Set_Slewing_Rate_Factor
      =>
        return "";
      when Set_Polar_Alignment
         | Set_Alt_Az_Alignment
         | Quit_Move
      =>
        raise Program_Error; -- not implemented for 10micron
      end case;
    exception
    when Error.Occurred =>
      Disconnect_Device (Error.Message);
    when Network.Timeout =>
      Flush_Input;
      Timeout_Detected := True;
      Error.Raise_With ("Reply timeout");
    when Network.Tcp.No_Client =>
      Disconnect_Device ("Reply_For - no client");
    when Item: others =>
      Log.Termination (Item);
      Disconnect_Device ("Reply_For - unknown error");
    end;
  exception
  when Error.Occurred =>
    raise;
  when others =>
    Error.Raise_With ("Send Error");
  end Reply_For;


  procedure Define_Moving_Rate;

  procedure Startup_If_Disconnected is
  begin
    case The_Status is
    when Disconnected =>
      begin
        The_Socket := Network.Tcp.Socket_For (The_Address     => The_Server_Address,
                                              The_Port        => The_Server_Port,
                                              The_Protocol    => Socket_Protocol,
                                              Receive_Timeout => Receive_Timeout);
      exception
      when Network.Not_Found =>
        return;
      when Item: others =>
        Error.Raise_With (Network.Exception_Kind (Item)'image);
      end;
      begin
        declare
          Product  : constant String := Reply_For (Lx200.Get_Product_Name);
          Version  : constant String := Reply_For (Lx200.Get_Firmware_Number);
          Versions : constant Text.Strings := Text.Strings_Of (Version, '.');
          type Number_Part is range 0 .. 99;
          function Value (Item : String) return Firmware_Version is (Firmware_Version(Number_Part'value(Item)));
        begin
          Log.Write ("Product " & Product);
          begin
            The_Firmware_Version := Value(Versions(1)) + Value(Versions(2)) * 0.01 + Value (Versions(3)) * 0.0001;
            Log.Write ("Firmware " & Version);
          exception
          when others =>
            Error.Raise_With ("Incorrect firmware version " & Version);
          end;
          Is_Legacy := The_Firmware_Version < 3.0;
          Has_New_Alignment := True;
        end;
        Set_Ultra_Precision_Mode;
        Set_Device_Status;
        Define_Moving_Rate;
      exception
      when others =>
        Disconnect_Device ("Startup failed");
      end;
    when others =>
      null;
    end case;
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Startup_If_Disconnected;


  procedure Execute (The_Command : Lx200.Extended_Command;
                     Parameter   : String := "";
                     Expected    : String := "") is
    Reply : constant String := Reply_For (The_Command, Parameter);
  begin
    if Reply /= Expected then
      Error.Raise_With ("command " & The_Command'image & " failed with " & Reply);
    end if;
  end Execute;


  function Execute (The_Command : Lx200.Extended_Command;
                    Parameter   : String := "";
                    Ok          : String;
                    Failed      : String) return Boolean is
    Reply : constant String := Reply_For (The_Command, Parameter);
  begin
    if Reply = Ok then
      return True;
    elsif Reply = Failed then
      return False;
    else
      Error.Raise_With ("command " & The_Command'image & " failed with " & Reply);
    end if;
  end Execute;


  function Actual_Direction return Space.Direction is
  begin
    return Space.Direction_Of (Ra  => Lx200.Hours_Of (Reply_For (Lx200.Get_Right_Ascension)),
                               Dec => Lx200.Signed_Degrees_Of (Reply_For (Lx200.Get_Declination)));
  end Actual_Direction;


  function Actual_Position return Space.Direction is
  begin
    return Space.Direction_Of (Ra  => Lx200.Position_Of (Reply_For (Lx200.Get_Axis_RA_Position)),
                               Dec => Lx200.Position_Of (Reply_For (Lx200.Get_Axis_Dec_Position)));
  end Actual_Position;


  procedure Define (The_Air_Pressure : Refraction.Hectopascal) is
  begin
    Execute (Lx200.Set_Air_Pressure, Lx200.Air_Pressure_Of (The_Air_Pressure), Expected => "1");
    Refraction.Set (The_Air_Pressure);
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Define;


  procedure Define (The_Temperature : Refraction.Celsius) is
  begin
    Execute (Lx200.Set_Temperature, Lx200.Temperature_Of (The_Temperature), Expected => "1");
    Refraction.Set (The_Temperature);
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Define;


  procedure Set_Julian_Date (Item : Time.JD) is
  begin
    Execute (Lx200.Set_Julian_Date, Lx200.Julian_Date_Of (Item), Expected => "1");
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Set_Julian_Date;


  procedure Set_Time_Offset (Item : Duration) is
  begin
   Execute (Lx200.Set_Time_Offset, Lx200.Time_Offset_Of (Item), Expected => "1");
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Set_Time_Offset;


  function Air_Pressure return Refraction.Hectopascal is
  begin
    return Refraction.Hectopascal'value (Reply_For (Lx200.Get_Air_Pressure));
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return Refraction.Undefined_Air_Pressure;
   when others =>
    Log.Warning ("invalid air pressure");
    return Refraction.Undefined_Air_Pressure;
  end Air_Pressure;


  function Pier_Side return Character is
  begin
    declare
      Reply : constant String := Reply_For (Lx200.Get_Pointing_State);
    begin
      if Reply = "East" then
        return 'E';
      elsif Reply = "West" then
        return 'W';
      else
        Log.Warning ("unknown pier side");
        return Undefined_Pier;
      end if;
    end;
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return Undefined_Pier;
  end Pier_Side;


  function Temperature return Refraction.Celsius is
  begin
    return Refraction.Celsius'value (Reply_For (Lx200.Get_Temperature));
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return Refraction.Undefined_Temperature;
   when others =>
    Log.Warning ("invalid temperature");
    return Refraction.Undefined_Temperature;
  end Temperature;


  function Julian_Date return Time.JD is
  begin
    return Time.JD'value (Reply_For (Lx200.Get_Julian_Date));
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return Time.Julian_Date;
  when others =>
    Log.Warning ("invalid julian date");
    return Time.Julian_Date;
  end Julian_Date;


  function Gps_Is_Synchronized return Boolean is
  begin
    return Execute (Lx200.Gps_Test_Synchronized, Ok => "1", Failed => "0");
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return False;
  end Gps_Is_Synchronized;


  function Actual_Moving_Speed return Angle.Value;

  function Get return Information is
    The_Information : Information;
  begin
    if The_Status /= Disconnected then
      Set_Device_Status;
      Define_Moving_Rate;
      The_Information.Status := The_Status;
      The_Information.Direction := Actual_Direction;
      The_Information.Position := Actual_Position;
      The_Information.Pier_Side := Pier_Side;
      The_Information.Moving_Speed := Actual_Moving_Speed;
      if The_Status = Stopped then
        The_Information.Date_Time := Time.Ut_Of (Julian_Date);
        Refraction.Set (Air_Pressure);
        Refraction.Set (Temperature);
      else
        The_Information.Date_Time := Time.Universal;
      end if;
   end if;
    return The_Information;
  exception
  when Lx200.Protocol_Error =>
    Flush_Input;
    return The_Information;
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return The_Information;
  end Get;


  procedure Set_Tracking_Rates (Factors : Tracking_Rate_Factors := Sideral_Rate_Factors) is
    use all type Lx200.Command;
  begin
    if Actual_Rates /= Factors then
      Execute (Set_Tracking_Rate_Dec_Factor, Lx200.Factor_Of (Factors.Dec), Expected => "1");
      Execute (Set_Tracking_Rate_RA_Factor, Lx200.Factor_Of (Factors.Ra), Expected => "1");
      Actual_Rates := Factors;
    end if;
  end Set_Tracking_Rates;


  procedure Slew_To (Location  : Space.Direction;
                     Target    : Target_Kind := Other_Targets) is

    use all type Lx200.Command;

    function Neo_Prepared return Boolean is
      Reply : constant String := Reply_For (Tle_Precalculate, Lx200.Julian_Date_Of (Time.Julian_Date) & ",60");
    begin
      if Reply in "E" | "N" then
        Log.Warning ("TLE precalculation failed");
        return False;
      else
        Log.Write ("Precalculation result: " & Reply);
        return True;
      end if;
    end Neo_Prepared;

    procedure Neo_Slew is
      Reply : constant String := Reply_For (Tle_Slew);
    begin
      if Reply'length = 1 then
        case Reply(Reply'first) is
        when 'E' =>
          Log.Warning ("NEO no transient precalculated");
        when 'F' =>
          Log.Warning ("NEO mount parked");
        when 'Q' =>
          Log.Warning ("NEO transit already ended");
        when 'S' =>
          Log.Write ("NEO start to catch satellite");
        when 'V' =>
          Log.Write ("NEO slew to start position");
        when others =>
          Log.Warning ("Neo unknown relpy <" & Reply & '>');
        end case;
      else
        Log.Warning ("Neo bad relpy <" & Reply & '>');
      end if;
    end Neo_Slew;

  begin -- Slew_To
    if Not_Connected then
      return;
    end if;
    case Target is
    when Axis_Position =>
      Execute (Set_Axis_RA_Position, Lx200.Position_Of (Space.Ra_Of (Location)), Expected => "1");
      Execute (Set_Axis_Dec_Position, Lx200.Position_Of (Space.Dec_Of (Location)), Expected => "1");
      Execute (Slew_To_Axis_Position, Expected => Lx200.Slew_Ok);
    when Near_Earth_Object =>
      if Neo_Prepared then
        Neo_Slew;
      end if;
    when others =>
      Execute (Set_Right_Ascension, Lx200.Hours_Of (Space.Ra_Of (Location)), Expected => "1");
      Execute (Set_Declination, Lx200.Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
      Execute (Slew, Expected => Lx200.Slew_Ok);
    end case;
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Slew_To;


  procedure Synch_To (Location : Space.Direction) is
    use all type Lx200.Command;
  begin
    if Not_Connected then
      return;
    end if;
    Execute (Set_Right_Ascension, Lx200.Hours_Of (Space.Ra_Of (Location)), Expected => "1");
    Execute (Set_Declination, Lx200.Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
    Execute (Synchronize, Expected => Lx200.Synch_Ok);
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Synch_To;


  procedure Start_Capturing is
  begin
    Log.Write ("start capturing");
    The_Status := Capturing;
  end Start_Capturing;


  procedure Start_Solving is
  begin
    Log.Write ("start solving");
    The_Status := Solving;
  end Start_Solving;


  procedure End_Solving is
  begin
    Log.Write ("end solving");
    The_Status := Last_State;
  end End_Solving;


  procedure Load_Tle (Name : String) is

    function Two_Line_Element return String is
      Eol  : constant String := "$0A";
      Tle : constant Satellite.Tle := Satellite.Tle_Of (Name);
    begin
      return Name & Eol & Tle(1) & Eol & Tle(2) & Eol;
    end Two_Line_Element;

  begin
    Log.Write ("load TLE of satellite " & Name);
    Execute (Lx200.Tle_Load_Satellite, Two_Line_Element, Expected => "V");
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Load_Tle;


  procedure Park is
  begin
    Execute (Lx200.Slew_To_Park_Position);
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Park;


  procedure Stop is
  begin
    Execute (Lx200.Stop);
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Stop;


  procedure Unpark is
  begin
    Execute (Lx200.Unpark);
    Define_Moving_Rate;
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Unpark;


  subtype Move_Command is Command range Move_Left .. Move_End;

  procedure Move (The_Command : Lx200.Command) is
  begin
    if The_Status in Tracking | Slewing | Following | Stopped | Positioned | Outside then
      Execute (The_Command);
    end if;
  end Move;


  generic
    type Direction is (<>);
    with procedure Move (To : Direction);
    with procedure Quit_Move (To : Direction);
  package Direct is

    procedure Set (Item : Direction);

    procedure Clear (Item : Direction);

    function Is_Moving return Boolean;

  end Direct;


  package body Direct is

    type Direct_State is (Both_Clear, First_Moving, Second_Moving, Both_Set, First_Set, Second_Set);

    The_State : Direct_State := Both_Clear;

    procedure Set (Item : Direction) is
    begin
      case The_State is
      when Both_Clear =>
        Move (To => Item);
        if Item = Direction'first then
          The_State := First_Moving;
        else
          The_State := Second_Moving;
        end if;
      when First_Moving =>
        if Item = Direction'last then
          Quit_Move (Direction'first);
          The_State := Both_Set;
        end if;
      when Second_Moving =>
        if Item = Direction'first then
          Quit_Move (Direction'last);
          The_State := Both_Set;
        end if;
      when First_Set =>
        if Item = Direction'last then
          The_State := Both_Set;
        end if;
      when Second_Set =>
        if Item = Direction'first then
          The_State := Both_Set;
        end if;
      when Both_Set =>
        null;
      end case;
    end Set;


    procedure Clear (Item : Direction) is
    begin
      case The_State is
      when Both_Clear =>
        null;
      when First_Moving =>
        if Item = Direction'first then
          Quit_Move (Direction'first);
          The_State := Both_Clear;
        end if;
      when Second_Moving =>
        if Item = Direction'last then
          Quit_Move (Direction'last);
          The_State := Both_Clear;
        end if;
      when First_Set =>
        if Item = Direction'first then
          The_State := Both_Clear;
        end if;
      when Second_Set =>
        if Item = Direction'last then
          The_State := Both_Clear;
        end if;
      when Both_Set =>
        if Item = Direction'first then
          The_State := Second_Set;
        else
          The_State := First_Set;
        end if;
      end case;
    end Clear;


    function Is_Moving return Boolean is
    begin
      return The_State in First_Moving | Second_Moving;
    end Is_Moving;

  end Direct;


  type North_South is (North, South);

  procedure Move (To : North_South) is
  begin
    case To is
    when North =>
      Move (Lx200.Move_North);
    when South =>
      Move (Lx200.Move_South);
    end case;
  end Move;


  procedure End_Move (To : North_South) is
  begin
    case To is
    when North =>
      Move (Lx200.Quit_Move_North);
    when South =>
      Move (Lx200.Quit_Move_South);
    end case;
  end End_Move;


  package Direct_NS is new Direct (North_South, Move, End_Move);


  type East_West is (East, West);

  procedure Move (To : East_West) is
  begin
    case To is
    when East =>
      Move (Lx200.Move_East);
    when West =>
      Move (Lx200.Move_West);
    end case;
  end Move;


  procedure End_Move (To : East_West) is
  begin
    case To is
    when East =>
      Move (Lx200.Quit_Move_East);
    when West =>
      Move (Lx200.Quit_Move_West);
    end case;
  end End_Move;


  package Direct_EW is new Direct (East_West, Move, End_Move);


  procedure Move (The_Command : Move_Command) is
  begin
    case The_Command is
    when Move_Up =>
      Direct_NS.Set (North);
    when Move_Down =>
      Direct_NS.Set (South);
    when Move_Right =>
      Direct_EW.Set (East);
    when Move_Left =>
      Direct_EW.Set (West);
    when Move_Up_End =>
      Direct_NS.Clear (North);
    when Move_Down_End =>
      Direct_NS.Clear (South);
    when Move_Right_End =>
      Direct_EW.Clear (East);
    when Move_Left_End =>
      Direct_EW.Clear (West);
    when Move_End =>
      Direct_NS.Clear (North);
      Direct_NS.Clear (South);
      Direct_EW.Clear (East);
      Direct_EW.Clear (West);
    end case;
  end Move;


  function Moving_Rate_Change_Allowed return Boolean is
  begin
    return not (Direct_NS.Is_Moving or Direct_EW.Is_Moving) and then
      The_Status in Tracking | Stopped | Positioned | Following | Slewing | Outside;
  end Moving_Rate_Change_Allowed;


  type Moving_Factor is range 1 .. 255;

  type Moving_Factor_Table is array (Positive range <>) of Moving_Factor;

  Moving_Factors : constant Moving_Factor_Table := [Moving_Factor'first, 4, 16, 60, 240, Moving_Factor'last];

  Undefined : constant := 0;

  The_Moving_Index : Natural := Undefined;

  The_Maximum_Moving_Rate : Angle.Value := Angle.Zero;


  procedure Set_Moving_Rate (The_Factor : Moving_Factor) is
  begin
    if The_Factor = Moving_Factor'last then
      Execute (Lx200.Set_Slewing_Rate); -- set maximum slew rate
    else
      Execute (Lx200.Set_Centering_Rate_Factor, Text.Trimmed(The_Factor'image));
    end if;
  end Set_Moving_Rate;


  procedure Increase is
  begin
    if The_Moving_Index < Moving_Factors'last and then Moving_Rate_Change_Allowed then
      The_Moving_Index := @ + 1;
      Set_Moving_Rate (Moving_Factors(The_Moving_Index));
    end if;
  end Increase;


  procedure Decrease is
  begin
    if The_Moving_Index > Moving_Factors'first and then Moving_Rate_Change_Allowed then
      The_Moving_Index := @ - 1;
      Set_Moving_Rate (Moving_Factors(The_Moving_Index));
    end if;
  end Decrease;


  procedure Define_Moving_Rate is
  begin
    if The_Status in Parked | Disconnected then
      The_Moving_Index := Undefined;
    elsif The_Moving_Index = Undefined then
      The_Maximum_Moving_Rate := Lx200.Rate_Of (Reply_For (Lx200.Get_Maximum_Slew_Rate));
      The_Moving_Index := Moving_Factors'first + 2; -- first Increment at startup -> default rate = 60
      Increase;
    end if;
  end Define_Moving_Rate;


  function Actual_Moving_Speed return Angle.Value is
    use type Angle.Degrees;
    Sideral_Tracking_Rate : constant Angle.Degrees := 15.0 / 3600.0; -- per second
    use type Angle.Value;
  begin
    if The_Moving_Index = 0 then
      return Angle.Zero;
    elsif The_Moving_Index = Moving_Factors'last then
      return The_Maximum_Moving_Rate;
    end if;
    return +(Angle.Degrees(Moving_Factors(The_Moving_Index)) * Sideral_Tracking_Rate);
  end Actual_Moving_Speed;


  procedure Execute (The_Command : Command) is
  begin
    case The_Command is
    when Move_Command =>
      Move (The_Command);
    when Increase_Moving_Rate =>
      Increase;
    when Decrease_Moving_Rate =>
      Decrease;
    end case;
  end Execute;


  procedure Start_Alignment is
  begin
    Has_New_Alignment := False;
    Execute (Lx200.New_Alignment_Start, Expected => "V");
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
  end Start_Alignment;


  procedure Add_Alignment_Point (Mount   :     Space.Direction;
                                 Picture :     Space.Direction;
                                 Side    :     Character;
                                 Lmst    :     Time.Value;
                                 Points  : out Natural) is
  begin
    declare
      Mra       : constant String := Lx200.Hours_Of (Space.Ra_Of (Mount));
      Mdec      : constant String := Lx200.Signed_Degrees_Of (Space.Dec_Of (Mount));
      Pra       : constant String := Lx200.Hours_Of (Space.Ra_Of (Picture));
      Pdec      : constant String := Lx200.Signed_Degrees_Of (Space.Dec_Of (Picture));
      Sid_Time  : constant String := Lx200.Hours_Of (Lmst);
      Parameter : constant String := Mra & ',' & Mdec & ',' & Side & ',' & Pra & ',' & Pdec & ',' & Sid_Time;
      Reply     : constant String := Reply_For (Lx200.New_Alignment_Point, Parameter);
    begin
      if Reply = "E" then
        Log.Warning ("add alignment not valid");
        Points := 0;
      else
        Points := Natural'value(Reply);
      end if;
    end;
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    Points := 0;
  end Add_Alignment_Point;


  function End_Alignment return Boolean is
  begin
    Has_New_Alignment := True;
    return Execute (Lx200.New_Alignment_End, Ok => "V", Failed => "E");
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return False;
  end End_Alignment;


  function Has_New_Alignment_Info return Boolean is
  begin
    if Has_New_Alignment then
      if Is_Legacy then
        Execute (Lx200.New_Alignment_Start, Expected => "V");
        Execute (Lx200.New_Alignment_End, Expected => "E");
      end if;
      return True;
    end if;
    return False;
  end Has_New_Alignment_Info;


  function Alignment_Info return Alignment_Data is
    No_Information : constant Alignment_Data := (others => <>);
  begin
    Has_New_Alignment := False;
    declare
      Reply : constant String := Reply_For (Lx200.Get_Alignment_Information);
      Data  : constant Text.Strings := Text.Strings_Of (Reply, Separator => ',');

      function Parameter (Index   : Positive;
                          Default : String := "0.0") return String is
        Image : constant String := Data(Index);
      begin
        if Image = "E" then
          return Default;
        end if;
        return Image;
      end Parameter;

      use type Angle.Value;
    begin
      if Reply = "E" then
        return No_Information;
      end if;
      return (Ra_Axis_Direction => Earth.Direction_Of (Az  => +Angle.Degrees'value(Parameter(1)),
                                                       Alt => +Angle.Degrees'value(Parameter(2))),
              Polar_Align_Error   => Polar_Error'value(Parameter(3)),
              Ra_Axis_Angle       => Axis_Angle'value(Parameter(4)),
              Orthogonality_Error => Orthogonality'value(Parameter(5)),
              Az_Knob_Turns_Left  => Knob_Turns'value(Parameter(6)),
              Alt_Knob_Turns_Down => Knob_Turns'value(Parameter(7)),
              Modeling_Terms      => Natural'value(Parameter(8, "0")),
              Rms_Error           => Arc_Seconds'value(Parameter(9)));
    end;
  exception
  when Item: others =>
    Log.Termination (Item);
    return No_Information;
  end Alignment_Info;


  function Image_Of (Value : Time_Offset) return String is

    I : constant String := Value'image;
    F : constant Natural := I'first;

  begin
    return (if Value < 0.0 then '-' else '+') & I(F + 1) & I(F + 3 .. F + 5) & '.' & I(F + 6);
  end Image_Of;


  function Updated (Time_Change : Time_Offset) return Boolean is
  begin
    if Is_Legacy then
      Log.Warning ("Legacy Firmware for " & Lx200.String_Of (Lx200.Trajectory_Offset_Add));
      return False;
    else
      declare
        Time_Image : constant String := Image_Of (Time_Change);
      begin
        return Execute (Lx200.Trajectory_Offset_Add, Parameter => "4," & Time_Image, Ok => "V", Failed => "E");
      end;
    end if;
  exception
  when Error.Occurred =>
    Log.Warning (Error.Message);
    return False;
  end Updated;


  procedure Disconnect is
  begin
    Set_Status (Disconnected);
    Network.Tcp.Close (The_Socket);
  exception
  when others =>
    null;
  end Disconnect;

end Ten_Micron;
