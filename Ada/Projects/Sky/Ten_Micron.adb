-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

  Receive_Timeout   : constant Duration := 1.0;
  Number_Of_Retries : constant := 3;

  The_Socket : Network.Tcp.Socket;
  The_Status : State := Disconnected;


  procedure Set_Status (Item : State) is
  begin
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
  begin
    loop
      declare
        Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Lx200.Terminator);
      begin
        if Log_Enabled then
          Log.Write ("Reply " & Reply);
        end if;
        return Reply (Reply'first .. Reply'last - 1);
      end;
    end loop;
  end Received_String;


  function Received_Character return String is
  begin
    loop
      declare
        Item : constant Character := Network.Tcp.Raw_Character_From (The_Socket);
      begin
        Log.Write ("Reply " & Item);
        return "" & Item;
      end;
    end loop;
  end Received_Character;


  procedure Send (Command     : String;
                  Log_Enabled : Boolean := True) is
  begin
    if Log_Enabled then
      Log.Write ("Command " & Command);
    end if;
    Network.Tcp.Send (The_String  => Command,
                      Used_Socket => The_Socket);
  exception
  when Item: others =>
    Disconnect_Device ("no anwer <" & Network.Exception_Kind (Item)'image & '>');
  end Send;


  procedure Set_Device_Status is
  begin
    Send (Lx200.String_Of(Lx200.Get_Status), Log_Enabled => False);
    declare
      Reply : constant String := Received_String (Log_Enabled => False);
    begin
      Set_Status (State'val(Character'pos(Reply(Reply'first)) - Character'pos('0')));
    exception
    when others =>
      Error.Raise_With ("Unknown device status " & Reply);
    end;
  end Set_Device_Status;


  procedure Set_Ultra_Precision_Mode is
  begin
    Send (Lx200.String_Of(Lx200.Set_Ultra_Precision_Mode));
  end Set_Ultra_Precision_Mode;


  function Reply_For (Command   : Lx200.Extended_Command;
                      Parameter : String := "") return String is
    use Lx200;

    Log_Enabled : constant Boolean := not (Command in Get_Declination | Get_Right_Ascension);

  begin
    for Unused_Count in 1 .. Number_Of_Retries loop
      Send (String_Of (Command, Parameter), Log_Enabled);
      begin
        case Command is
        when Slew =>
          declare
            Reply : constant String := Received_Character;
          begin
            if Reply /= Slew_Ok then
              Log.Warning (Received_String);
              return Slew_Ok; -- simulate ok
            end if;
            return Reply;
          end;
        when Set_Local_Time
           | Set_Time_Offset
           | Set_Altitude
           | Set_Azimuth
           | Set_Right_Ascension
           | Set_Declination
        =>
          return Received_Character;
        when Get_Product_Name
           | Get_Firmware_Number
           | Get_Firmware_Date
           | Get_Alignment_Status
           | Get_Latitude
           | Get_Longitude
           | Get_Sideral_Time
           | Get_Status
           | Get_Right_Ascension
           | Get_Altitude
           | Get_Azimuth
           | Get_Declination
           | Set_Latitude
           | Set_Longitude
         =>
          return Received_String (Log_Enabled);
        when Set_Ultra_Precision_Mode
           | Slew_To_Park_Position
           | Unpark
        =>
          return "";
        when Set_Polar_Alignment
           | Set_Alt_Az_Alignment
           | Synchronize
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
        null;
      when Network.Timeout =>
        Log.Warning ("Reply timeout");
      when Item: others =>
        Log.Termination (Item);
        Disconnect_Device ("Reply - unknown error");
      end;
    end loop;
    Disconnect_Device ("Reply timeout");
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
            Error.Raise_With ("expected version GM1000HPS " & Firmware_GM1000);
          end if;
        else
          Error.Raise_With ("Incorrect 10micron product name " & Product);
        end if;
        Set_Ultra_Precision_Mode;
        Set_Device_Status;
      end;
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


  function Get return Information is
    The_Information : Information;
  begin
    if The_Status /= Disconnected then
      Set_Device_Status;
      The_Information.Status := The_Status;
      The_Information.Direction := Actual_Direction;
    end if;
    return The_Information;
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
    return The_Information;
  end Get;


  procedure Slew_To (Location : Space.Direction) is
    use Lx200;
  begin
    if Not_Connected then
      return;
    end if;
    Execute (Set_Right_Ascension, Hours_Of (Space.Ra_Of (Location)), Expected => "1");
    Execute (Set_Declination, Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
    Execute (Slew, Expected => Slew_Ok);
  exception
  when Error.Occurred =>
    Log.Error (Error.Message);
  end Slew_To;


  procedure Park is
  begin
    Execute (Lx200.Slew_To_Park_Position);
  end Park;


  procedure Unpark is
  begin
    Execute (Lx200.Unpark);
  end Unpark;


  procedure Disconnect is
  begin
    Set_Status (Disconnected);
    Network.Tcp.Close (The_Socket);
  exception
  when others =>
    null;
  end Disconnect;

end Ten_Micron;