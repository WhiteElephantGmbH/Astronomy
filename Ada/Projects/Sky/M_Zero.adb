-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;
with Earth;
with Exceptions;
with Lx200;
with Network.Tcp;
with Objects;
with Site;
with Text;
with Time;
with Traces;

package body M_Zero is

  package Log is new Traces ("M_Zero");

  Device_Disconnected : exception;

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

  Server_Port     : constant Network.Port_Number := 4030;
  Receive_Timeout : constant Duration := 3.0;

  The_Socket : Network.Tcp.Socket;
  The_Status : State := Disconnected;
  Stop_State : State := Disconnected;
  The_Error  : Text.String;

  procedure Set_Status (Item : State) is
  begin
    case Item is
    when Connected | Initialized | Tracking =>
      Stop_State := Item;
    when others =>
      null;
    end case;
    The_Status := Item;
  end Set_Status;


  procedure Error (Message : String) is
  begin
    The_Error := Text.String_Of (Message);
    Log.Error (Message);
    Set_Status (Error);
  end Error;


  procedure Disconnect_Device with No_Return is
  begin
    Error ("Device not Connected");
    Network.Tcp.Close (The_Socket);
    raise Device_Disconnected;
  end Disconnect_Device;


  function Error_Message return String is
    Message : constant String := Text.String_Of (The_Error);
  begin
    if Message = "" then
      return "No Error";
    end if;
    Text.Clear (The_Error);
    Disconnect;
    return Message;
  end Error_Message;


  function Not_Initialized return Boolean is
  begin
    if The_Status < Initialized then
      Log.Warning ("not initialized");
      return True;
    end if;
    return False;
  end Not_Initialized;


  function Received_String return String is
  begin
    loop
      declare
        Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Lx200.Terminator);
      begin
        Log.Write ("Reply " & Reply);
        if Reply = "ge" & Lx200.Terminator then -- M-Zero extension
          Set_Status (Tracking);
        else
          return Reply (Reply'first .. Reply'last - 1);
        end if;
      end;
    end loop;
  end Received_String;


  function Received_Character return String is
  begin
    loop
      declare
        Item : constant Character := Network.Tcp.Raw_Character_From (The_Socket);
      begin
        if Item = 'g' then
          declare
            Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Lx200.Terminator);
          begin
            Log.Write ("Reply g" & Reply);
            if The_Status = Approaching and then Reply = "e" & Lx200.Terminator then -- M-Zero extension
              Set_Status (Tracking);
            else
              return 'g' & Reply(Reply'first .. Reply'last - 1);
            end if;
          end;
        else
          Log.Write ("Reply " & Item);
          return "" & Item;
        end if;
      end;
    end loop;
  end Received_Character;


  function Reply_For (Command   : Lx200.Command;
                      Parameter : String := "") return String is
    use Lx200;

    Command_String : constant String := String_Of (Command, Parameter);

  begin
    Log.Write ("Command " & Command_String);
    begin
      Network.Tcp.Send (The_String  => Command_String,
                        Used_Socket => The_Socket);
    exception
    when Item: others =>
      Error ("no anwer <" & Network.Exception_Kind (Item)'image & '>');
      Disconnect_Device;
    end;
    case Command is
    when Slew =>
      declare
        Reply : constant String := Received_String;
      begin
        if Reply = "0" then
          Set_Status (Approaching);
        end if;
        return Reply;
      end;
    when Set_Alt_Az_Alignment
       | Set_Polar_Alignment
       | Set_Centering_Rate
       | Set_Guiding_Rate
       | Set_Finding_Rate
       | Set_Slewing_Rate
       | Move_North
       | Move_South
       | Move_East
       | Move_West
       | Quit_Move_North
       | Quit_Move_South
       | Quit_Move_East
       | Quit_Move_West
       | Quit_Move
    =>
      return "";
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
       | Get_Right_Ascension
       | Get_Altitude
       | Get_Azimuth
       | Get_Declination
       | Set_Latitude
       | Set_Longitude
       | Synchronize
    =>
      return Received_String;
    end case;
  exception
  when Device_Disconnected =>
    raise;
  when Network.Timeout =>
    Log.Error ("Reply_For timeout");
    Disconnect_Device;
  when Item: others =>
    Log.Error (Exceptions.Information_Of (Item));
    Disconnect_Device;
  end Reply_For;


  procedure Connect (Server_Address : Network.Ip_Address) is
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
        Log.Error ("<Connect Failed> " & Exceptions.Information_Of (Item));
        Disconnect_Device;
      end;
      if Reply_For (Lx200.Get_Product_Name) = "Avalon" and then
         Reply_For (Lx200.Get_Firmware_Number) = "56.3"
      then
        Set_Status (Connected);
      else
        Error ("unknown device");
      end if;
    when Error =>
      null;
    when others =>
      Log.Warning ("already connected");
    end case;
  exception
  when Device_Disconnected =>
    null;
  end Connect;


  procedure Execute (Command   : Lx200.Command;
                     Parameter : String := "";
                     Expected  : String := "") is
    Reply : constant String := Reply_For (Command, Parameter);
  begin
    if Reply /= Expected then
      Error ("command " & Command'image & " failed with " & Reply);
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
    The_Information.Status := The_Status;
    if The_Status > Connected then
      The_Information.Direction := Actual_Direction;
    end if;
    return The_Information;
  exception
  when others =>
    return The_Information;
  end Get;


  procedure Initialize is

    function Home return Space.Direction is
      South : constant Earth.Direction := Earth.Direction_Of (Alt => Angle.Zero, Az => Angle.Semi_Circle);
    begin
      return Objects.Direction_Of (South, Time.Universal);
    end Home;

   begin -- Intialize
    case The_Status is
    when Error =>
      null;
    when Disconnected =>
      Error ("not connected");
    when Connected =>
      declare
        Direction   : constant Space.Direction := Actual_Direction;
        Declination : constant Angle.Value := Space.Dec_Of (Direction);
        use type Angle.Value;
      begin
        Log.Write ("Initialize - Declination " & Angle.Image_Of (Declination));
        if Declination in Angle.Zero | Angle.Quadrant then
          if Reply_For (Lx200.Get_Alignment_Status) /= "PT0" then
            Execute (Lx200.Set_Polar_Alignment);
            delay 1.0; -- give avalon time to initialize
            if Reply_For (Lx200.Get_Alignment_Status) /= "PT0" then
              Error ("alignment not polar alignment");
              return;
            end if;
          end if;
          Execute (Lx200.Set_Latitude, Lx200.Signed_Degrees_Of (Site.Latitude), Expected => "0");
          Execute (Lx200.Set_Longitude, Lx200.Signed_Degrees_Of (Site.Longitude, Front_Digits => 3), Expected => "0");
          Set_Status (Initialized);
          Synch_To (Home);
          delay 0.5; -- give avalon time to settle.
          declare
            Home_Declination : constant Angle.Value := Space.Dec_Of (Actual_Direction);
          begin
            Log.Write ("Initialize - Home Declination " & Angle.Image_Of (Home_Declination));
            if Home_Declination /= Angle.Quadrant then
              Synch_To (Direction); -- already initialized
            end if;
          end;
        else
          Set_Status (Tracking);
        end if;
      end;
    when others =>
      Log.Warning ("Already Initialized");
    end case;
  exception
  when Device_Disconnected =>
    null;
  end Initialize;


  procedure Set_Rate (Rate : Moving_Rate) is
  begin
    if Not_Initialized then
      return;
    end if;
    case Rate is
    when Centering =>
      Execute (Lx200.Set_Centering_Rate);
    when Guiding =>
      Execute (Lx200.Set_Guiding_Rate);
    when Finding =>
      Execute (Lx200.Set_Finding_Rate);
    when Slewing =>
      Execute (Lx200.Set_Slewing_Rate);
    end case;
  exception
  when Device_Disconnected =>
    null;
  end Set_Rate;


  procedure Start_Move (Direction : Directions) is
  begin
    if Not_Initialized then
      return;
    end if;
    case Direction is
    when Up =>
      Execute (Lx200.Move_North);
    when Down =>
      Execute (Lx200.Move_South);
    when Left =>
      Execute (Lx200.Move_East);
    when Right =>
      Execute (Lx200.Move_West);
    end case;
    Set_Status (Moving);
  exception
  when Device_Disconnected =>
    null;
  end Start_Move;


  procedure Stop_Move (Direction : Directions) is
  begin
    if Not_Initialized then
      return;
    end if;
    case Direction is
    when Up =>
      Execute (Lx200.Quit_Move_North);
    when Down =>
      Execute (Lx200.Quit_Move_South);
    when Left =>
      Execute (Lx200.Quit_Move_East);
    when Right =>
      Execute (Lx200.Quit_Move_West);
    end case;
    Set_Status (Stop_State);
  exception
  when Device_Disconnected =>
    null;
  end Stop_Move;


  procedure Stop_Move is
  begin
    if Not_Initialized then
      return;
    end if;
    Execute (Lx200.Quit_Move, Expected => "");
    Set_Status (Stop_State);
  exception
  when Device_Disconnected =>
    null;
  end Stop_Move;


  procedure Slew_To (Location : Space.Direction) is
    use Lx200;
  begin
    if Not_Initialized then
      return;
    end if;
    Execute (Set_Right_Ascension, Hours_Of (Space.Ra_Of (Location)), Expected => "1");
    Execute (Set_Declination, Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
    Execute (Slew, Expected => "0");
  exception
  when Device_Disconnected =>
    null;
  end Slew_To;


  procedure Synch_To (Location : Space.Direction) is
    use Lx200;
  begin
    if Not_Initialized then
      return;
    end if;
    Execute (Set_Right_Ascension, Hours_Of (Space.Ra_Of (Location)), Expected => "1");
    Execute (Set_Declination, Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
    Execute (Synchronize, Expected => "0");
  exception
  when Device_Disconnected =>
    null;
  end Synch_To;


  procedure Disconnect is
  begin
    Set_Status (Disconnected);
    Network.Tcp.Close (The_Socket);
  exception
  when others =>
    null;
  end Disconnect;

end M_Zero;
