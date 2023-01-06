-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Error;
with Lx200;
with Network.Tcp;
with Traces;

package body Ten_Micron is

  package Log is new Traces ("Ten_Micron");

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

  GM1000HPS       : constant String := "10micron GM1000HPS";
  GM4000HPS       : constant String := "10micron GM4000HPS";
  Firmware_GM1000 : constant String := "3.1.2";
  Firmware_GM4000 : constant String := "2.15.1";

  Receive_Timeout  : constant Duration := 3.0;
  Flush_Timeout    : constant Duration := 0.5;
  Timeout_Detected : Boolean := False;

  The_Socket : Network.Tcp.Socket;
  The_Status : State := Disconnected;
  Last_State : State := Disconnected;


  procedure Set_Status (Item : State) is
  begin
    Last_State := Item;
    case The_Status is
    when Solving =>
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
    Network.Tcp.Close (The_Socket);
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


  procedure Send (Command     : String;
                  Log_Enabled : Boolean := True) is
  begin
    if Log_Enabled or Timeout_Detected then
      Log.Write ("Command " & Command);
    end if;
    Network.Tcp.Send (The_String  => Command,
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
      Log.Write ("Flushed: " & Flushed);
    end;
  exception
  when others =>
    null;
  end Flush_Input;


  procedure Set_Device_Status is
  begin
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
          The_State := State'val(State_Number);
        end case;
        if The_State = Disconnected then
          Log.Error ("Overloaded State");
          The_State := Failure;
        end if;
        Set_Status (The_State);
      end;
      return;
    exception
    when others =>
      Error.Raise_With ("Unknown device status <" & Reply & ">");
    end;
  exception
  when Network.Timeout =>
    Log.Warning ("Reply timeout");
    Timeout_Detected := True;
    Flush_Input;
  end Set_Device_Status;


  procedure Set_Ultra_Precision_Mode is
  begin
    Send (Lx200.String_Of(Lx200.Set_Ultra_Precision_Mode));
  end Set_Ultra_Precision_Mode;


  function Reply_For (Command   : Lx200.Extended_Command;
                      Parameter : String := "") return String is
    use all type Lx200.Command;

    Log_Enabled : constant Boolean := not (Command in Get_Declination
                                                    | Get_Right_Ascension
                                                    | Get_Axis_Dec_Position
                                                    | Get_Axis_RA_Position
                                                    | Get_Air_Pressure
                                                    | Get_Temperature
                                                    | Get_Pointing_State);

  begin
    Send (Lx200.String_Of (Command, Parameter), Log_Enabled);
    begin
      case Command is
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
      when Set_Local_Time
         | Set_Time_Offset
         | Set_Altitude
         | Set_Azimuth
         | Set_Axis_RA_Position
         | Set_Axis_Dec_Position
         | Set_Right_Ascension
         | Set_Declination
         | Set_Air_Pressure
         | Set_Temperature
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
         | Get_Axis_RA_Position
         | Get_Axis_Dec_Position
         | Get_Air_Pressure
         | Get_Temperature
         | Get_Number_Of_Alignment_Stars
         | Get_Pointing_State
         | Get_Right_Ascension
         | Get_Declination
         | Get_Altitude
         | Get_Azimuth
         | New_Alignment_Start
         | New_Alignment_Point
         | New_Alignment_End
         | Set_Latitude
         | Set_Longitude
         | Synchronize
       =>
        return Received_String (Log_Enabled);
      when Set_Ultra_Precision_Mode
         | Stop
         | Slew_To_Park_Position
         | Unpark
      =>
        return "";
      when Set_Polar_Alignment
         | Set_Alt_Az_Alignment
         | Move_East
         | Move_North
         | Move_South
         | Move_West
         | Quit_Move_East
         | Quit_Move_North
         | Quit_Move_South
         | Quit_Move_West
         | Quit_Move
         | Set_Centering_Rate
         | Set_Guiding_Rate
         | Set_Finding_Rate
         | Set_Slewing_Rate
      =>
        raise Program_Error; -- not implemented for 10micron
      end case;
    exception
    when Error.Occurred =>
      Log.Error (Error.Message);
      Disconnect_Device ("Error.Message");
    when Network.Timeout =>
      Log.Warning ("Reply timeout");
      Flush_Input;
      Timeout_Detected := True;
      Error.Raise_With ("Reply Timeout");
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


  procedure Startup (Server_Address : Network.Ip_Address;
                     Server_Port    : Network.Port_Number) is
  begin
    case The_Status is
    when Disconnected =>
      begin
        The_Socket := Network.Tcp.Socket_For (The_Address     => Server_Address,
                                              The_Port        => Server_Port,
                                              The_Protocol    => Socket_Protocol,
                                              Receive_Timeout => Receive_Timeout);
      exception
      when Network.Not_Found =>
        return;
      when Item: others =>
        Error.Raise_With (Network.Exception_Kind (Item)'image);
      end;
      declare
        Product : constant String := Reply_For (Lx200.Get_Product_Name);
      begin
        if Product = GM1000HPS then
          if Reply_For (Lx200.Get_Firmware_Number) /= Firmware_GM1000 then
            Error.Raise_With ("expected version GM1000HPS " & Firmware_GM1000);
          end if;
        elsif Product = GM4000HPS then
          if Reply_For (Lx200.Get_Firmware_Number) /= Firmware_GM4000 then
            Error.Raise_With ("expected version GM4000HPS " & Firmware_GM4000);
          end if;
        else
          Error.Raise_With ("Incorrect 10micron product name " & Product);
        end if;
      end;
      Set_Ultra_Precision_Mode;
      Set_Device_Status;
    when others =>
      Log.Error ("already connected");
    end case;
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Startup;


  procedure Execute (Command   : Lx200.Extended_Command;
                     Parameter : String := "";
                     Expected  : String := "") is
    Reply : constant String := Reply_For (Command, Parameter);
  begin
    if Reply /= Expected then
      Error.Raise_With ("command " & Command'image & " failed with " & Reply);
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
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Define;


  procedure Define (The_Temperature : Refraction.Celsius) is
  begin
    Execute (Lx200.Set_Temperature, Lx200.Temperature_Of (The_Temperature), Expected => "1");
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Define;


  function Air_Pressure return Refraction.Hectopascal is
  begin
    return Refraction.Hectopascal'value (Reply_For (Lx200.Get_Air_Pressure));
  exception
  when others =>
    Log.Error ("invalid air pressure");
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
        Log.Error ("unknown pier side");
        return ' ';
      end if;
    end;
  exception
  when others =>
    Log.Error ("invalid pier side");
    return ' ';
  end Pier_Side;


  function Temperature return Refraction.Celsius is
  begin
    return Refraction.Celsius'value (Reply_For (Lx200.Get_Temperature));
  exception
  when others =>
    Log.Error ("invalid temperature");
    return Refraction.Undefined_Temperature;
  end Temperature;


  function Get return Information is
    The_Information : Information;
  begin
    if The_Status /= Disconnected then
      Set_Device_Status;
      The_Information.Status := The_Status;
      The_Information.Direction := Actual_Direction;
      The_Information.Position := Actual_Position;
      The_Information.Pier_Side := Pier_Side;
      Refraction.Set (Air_Pressure);
      Refraction.Set (Temperature);
    end if;
    return The_Information;
  exception
  when Lx200.Protocol_Error =>
    Flush_Input;
    return The_Information;
  when Error.Occurred =>
    Log.Error (Error.Message);
    return The_Information;
  end Get;


  procedure Slew_To (Location : Space.Direction;
                     Target   : Target_Kind := Other_Targets) is
    use all type Lx200.Command;
  begin
    if Not_Connected then
      return;
    end if;
    case Target is
    when Axis_Position =>
      Execute (Set_Axis_RA_Position, Lx200.Position_Of (Space.Ra_Of (Location)), Expected => "1");
      Execute (Set_Axis_Dec_Position, Lx200.Position_Of (Space.Dec_Of (Location)), Expected => "1");
      Execute (Slew_To_Axis_Position, Expected => Lx200.Slew_Ok);
    when Other_Targets =>
      Execute (Set_Right_Ascension, Lx200.Hours_Of (Space.Ra_Of (Location)), Expected => "1");
      Execute (Set_Declination, Lx200.Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
      Execute (Slew, Expected => Lx200.Slew_Ok);
    end case;
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
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
    Log.Error (Error.Message);
  end Synch_To;


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


  procedure Park is
  begin
    Execute (Lx200.Slew_To_Park_Position);
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Park;


  procedure Stop is
  begin
    Execute (Lx200.Stop);
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Stop;


  procedure Unpark is
  begin
    Execute (Lx200.Unpark);
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Unpark;


  procedure Start_Alignment is
  begin
    Execute (Lx200.New_Alignment_Start, Expected => "V");
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
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
    Log.Error (Error.Message);
    Points := 0;
  end Add_Alignment_Point;


  function End_Alignment return Boolean is
  begin
    Execute (Lx200.New_Alignment_End, Expected => "V");
    return True;
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
    return False;
  end End_Alignment;


  procedure Disconnect is
  begin
    Set_Status (Disconnected);
    Network.Tcp.Close (The_Socket);
  exception
  when others =>
    null;
  end Disconnect;

end Ten_Micron;
