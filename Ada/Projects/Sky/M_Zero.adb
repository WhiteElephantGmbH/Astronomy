-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;
with Earth;
with Lx200;
with Network.Tcp;
with Objects;
with Picture;
with Site;
with Text;
with Time;
with Traces;

package body M_Zero is

  package Log is new Traces ("M_Zero");

  Error_Set : exception;

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

  Product_Name    : constant String := "Avalon";
  Firmware_Number : constant String := "65.6";

  Receive_Timeout   : constant Duration := 1.0;
  Number_Of_Retries : constant := 3;

  -- LX200 command extesions

  Get_Device_Status   : constant String := "X3C";
  Set_Lunar_Guiding   : constant String := "TL";
  Set_Solar_Guiding   : constant String := "TS";
  Set_Sideral_Guiding : constant String := "TQ";
  Set_Stop_Guiding    : constant String := "X120";

  The_Socket : Network.Tcp.Socket;
  The_Status : State := Disconnected;
  Last_State : State := Disconnected;

  type Alignment is (Not_Aligned, Unused_1, Unused_2, North_Aligned);

  The_Alignment   : Alignment;
  The_Target_Kind : Target_Kind;

  The_Error : Text.String;

  procedure Set_Status (Item : State) is
  begin
    case Item is
    when Disconnected | Connected | Initialized | Stopped | Tracking =>
      Last_State := Item;
    when others =>
      null;
    end case;
    The_Status := Item;
    Log.Write ("Status: " & Item'image);
  end Set_Status;


  procedure Set_Error (Message : String) is
  begin
    The_Error := Text.String_Of (Message);
    Log.Error (Message);
    Set_Status (Error);
  end Set_Error;


  procedure Raise_Error (Message : String) with No_Return is
  begin
    Set_Error (Message);
    raise Error_Set;
  end Raise_Error;


  procedure Disconnect_Device (Message : String) with No_Return is
  begin
    Network.Tcp.Close (The_Socket);
    Last_State := Disconnected;
    Raise_Error (Message);
  end Disconnect_Device;


  function Error_Message return String is
    Message : constant String := Text.String_Of (The_Error);
  begin
    if Message = "" then
      return "No Error";
    end if;
    Text.Clear (The_Error);
    Set_Status (Last_State);
    return Message;
  end Error_Message;


  function Not_Connected return Boolean is
  begin
    if The_Status < Connected then
      Log.Warning ("not connected");
      return True;
    end if;
    return False;
  end Not_Connected;


  function Not_Initialized return Boolean is
  begin
    if The_Status < Initialized then
      Log.Warning ("not initialized");
      return True;
    end if;
    return False;
  end Not_Initialized;


  procedure Set_Status is
  begin
    case The_Target_Kind is
    when Landmark =>
      Set_Status (Stopped);
    when others =>
      Set_Status (Tracking);
    end case;
  end Set_Status;


  function Received_String (Log_Enabled : Boolean := True) return String is
  begin
    loop
      declare
        Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Lx200.Terminator);
      begin
        if Log_Enabled then
          Log.Write ("Reply " & Reply);
        end if;
        if Reply = "ge" & Lx200.Terminator then -- M-Zero extension
          Set_Status;
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
              Set_Status;
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


  procedure Execute (Command : String) is
  begin
    Send (Lx200.Command_For (Command));
  end Execute;


  procedure Set_Device_Status is

    type Guiding_Kind is (Stopped, Lunar, Solar, Sideral);

    The_Guiding : Guiding_Kind;

  begin
    Execute (Get_Device_Status);
    declare                                       --  12345
      Reply : constant String := Received_String; -- :Z1ag2#
    begin
      The_Alignment   := Alignment'val(Natural'value("" & Reply(Reply'first + 3)));
      Log.Write ("Alignment " & The_Alignment'image);
      The_Guiding := Guiding_Kind'val(Natural'value("" & Reply(Reply'first + 4)));
      Log.Write ("Guiding " & The_Guiding'image);
      case The_Guiding is
      when Stopped =>
        The_Target_Kind := Landmark;
      when Lunar =>
        The_Target_Kind := Moon;
      when Solar =>
        The_Target_Kind := Sun;
      when others =>
        The_Target_Kind := Other_Targets;
      end case;
    exception
    when others =>
      Set_Error ("Unknown device status " & Reply & "#");
      raise Error_Set;
    end;
  end Set_Device_Status;



  function Reply_For (Command   : Lx200.Command;
                      Parameter : String := "") return String is

    use all type Lx200.Command;

    Log_Enabled : constant Boolean := not (Command in Get_Declination | Get_Right_Ascension);

  begin
    for Unused_Count in 1 .. Number_Of_Retries loop
      Send (Lx200.String_Of (Command, Parameter), Log_Enabled);
      begin
        case Command is
        when Slew =>
          declare
            Reply : constant String := Received_String (Log_Enabled);
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
          return Received_String (Log_Enabled);
        end case;
      exception
      when Error_Set =>
        raise;
      when Network.Timeout =>
        Log.Warning ("Reply timeout");
      when Item: others =>
        Log.Termination (Item);
        Disconnect_Device ("Reply - unknown error");
      end;
    end loop;
    Disconnect_Device ("Reply timeout");
  end Reply_For;


  procedure Check_Site is
  begin
    if not Site.Is_Defined then
      Picture.Set_Site;
    end if;
  exception
  when Picture.File_Not_Found =>
    Raise_Error ("Picture " & Picture.Filename & " not found to evaluate site");
  when Picture.Invalid_File | Picture.Undefined_Value =>
    Raise_Error ("No site information in downloaded picture");
  when Site.Not_Defined =>
    Raise_Error ("Site not defined");
  end Check_Site;


  procedure Startup (Server_Address : Network.Ip_Address;
                     Server_Port    : Network.Port_Number) is
  begin
    case The_Status is
    when Disconnected =>
      Check_Site;
      begin
        The_Socket := Network.Tcp.Socket_For (The_Address     => Server_Address,
                                              The_Port        => Server_Port,
                                              The_Protocol    => Socket_Protocol,
                                              Receive_Timeout => Receive_Timeout);
      exception
      when Item: others =>
        Raise_Error ("M-Zero " & Network.Exception_Kind (Item)'image);
      end;
      if Reply_For (Lx200.Get_Product_Name) = Product_Name and then
         Reply_For (Lx200.Get_Firmware_Number) = Firmware_Number
      then
        Set_Status (Connected);
      else
        Raise_Error ("device not M-Zero version " & Firmware_Number);
      end if;
    when Error =>
      null;
    when others =>
      Log.Warning ("already connected");
    end case;
  exception
  when Error_Set =>
    null;
  end Startup;


  procedure Execute (Command   : Lx200.Command;
                     Parameter : String := "";
                     Expected  : String := "") is
    Reply : constant String := Reply_For (Command, Parameter);
  begin
    if Reply /= Expected then
      Set_Error ("command " & Command'image & " failed with " & Reply);
    end if;
  end Execute;


  function Actual_Direction return Space.Direction is
  begin
    return Space.Direction_Of (Ra  => Lx200.Hours_Of (Reply_For (Lx200.Get_Right_Ascension)),
                               Dec => Lx200.Signed_Degrees_Of (Reply_For (Lx200.Get_Declination)));
  end Actual_Direction;


  function Status_Ok return Boolean is
  begin
    return Reply_For (Lx200.Get_Alignment_Status) = "PT0";
  end Status_Ok;


  function Get return Information is
    The_Information : Information;
  begin
    if The_Status > Disconnected then
      if not Site.Is_Defined then
        The_Status := Connected;
      end if;
    end if;
    The_Information.Status := The_Status;
    if Last_State > Connected then
      The_Information.Direction := Actual_Direction;
    elsif Last_State > Disconnected and then Status_Ok then
      The_Information.Direction := Space.Unknown_Direction;
    end if;
    return The_Information;
  exception
  when others =>
    return The_Information;
  end Get;


  procedure Synchronize_To (Location : Space.Direction) is
    use Lx200;
  begin
    Execute (Set_Right_Ascension, Hours_Of (Space.Ra_Of (Location)), Expected => "1");
    Execute (Set_Declination, Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
    Execute (Synchronize, Expected => "0");
  end Synchronize_To;


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
      Set_Error ("not connected");
    when Connected =>
      Check_Site;
      declare
        Direction   : constant Space.Direction := Actual_Direction;
        Declination : constant Angle.Value := Space.Dec_Of (Direction);
      begin
        Set_Device_Status;
        Log.Write ("Initialize - Declination " & Angle.Image_Of (Declination));
        if The_Alignment = Not_Aligned then
          if not Status_Ok then
            Execute (Lx200.Set_Polar_Alignment);
            delay 1.0; -- give avalon time to initialize
            if not Status_Ok then
              Set_Error ("alignment not polar");
              return;
            end if;
          end if;
          Execute (Lx200.Set_Latitude, Lx200.Signed_Degrees_Of (Site.Latitude), Expected => "0");
          Execute (Lx200.Set_Longitude, Lx200.Signed_Degrees_Of (Site.Longitude, Front_Digits => 3), Expected => "0");
          Set_Status (Initialized);
          Synchronize_To (Home);
          delay 0.5; -- give avalon time to settle.
        else
          Set_Status;
        end if;
      end;
    when others =>
      Log.Warning ("Already Initialized");
    end case;
  exception
  when Network.Timeout =>
    Disconnect_Device ("Reply timeout");
  when Error_Set =>
    null;
  end Initialize;


  procedure Set_Rate (Rate : Moving_Rate) is
  begin
    if Not_Connected then
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
  when Error_Set =>
    null;
  end Set_Rate;


  procedure Start_Moving (Direction : Moving_Direction) is
  begin
    if Not_Connected then
      return;
    end if;
    case Direction is
    when Up =>
      Execute (Lx200.Move_North);
    when Down =>
      Execute (Lx200.Move_South);
    when Left =>
      Execute (Lx200.Move_West);
    when Right =>
      Execute (Lx200.Move_East);
    end case;
    Set_Status (Moving);
  exception
  when Error_Set =>
    null;
  end Start_Moving;


  procedure Stop_Moving (Direction : Moving_Direction) is
  begin
    if Not_Connected then
      return;
    end if;
    case Direction is
    when Up =>
      Execute (Lx200.Quit_Move_North);
    when Down =>
      Execute (Lx200.Quit_Move_South);
    when Left =>
      Execute (Lx200.Quit_Move_West);
    when Right =>
      Execute (Lx200.Quit_Move_East);
    end case;
    Set_Status (Last_State);
  exception
  when Error_Set =>
    null;
  end Stop_Moving;


  procedure Stop_Moving is
  begin
    if Not_Connected then
      return;
    end if;
    Execute (Lx200.Quit_Move, Expected => "");
    Set_Status (Last_State);
  exception
  when Error_Set =>
    null;
  end Stop_Moving;


  procedure Set_Guiding_Speed_For (Kind : Target_Kind) is
  begin
    if The_Target_Kind /= Kind then
      The_Target_Kind := Kind;
      Log.Write ("Set guiding speed for " & Kind'image);
      case Kind is
      when Landmark =>
        Execute (Set_Stop_Guiding);
      when Moon =>
        Execute (Set_Lunar_Guiding);
      when Sun =>
        Execute (Set_Solar_Guiding);
      when Other_Targets =>
        Execute (Set_Sideral_Guiding);
      end case;
      Set_Device_Status;
    end if;
  end Set_Guiding_Speed_For;


  procedure Slew_To (Location : Space.Direction;
                     Kind     : Target_Kind := Other_Targets) is
    use Lx200;
  begin
    if Not_Initialized then
      return;
    end if;
    Set_Guiding_Speed_For (Kind);
    Execute (Set_Right_Ascension, Hours_Of (Space.Ra_Of (Location)), Expected => "1");
    Execute (Set_Declination, Signed_Degrees_Of (Space.Dec_Of (Location)), Expected => "1");
    Execute (Slew, Expected => "0");
  exception
  when Error_Set =>
    null;
  end Slew_To;


  procedure Synch_To (Location : Space.Direction;
                      Kind     : Target_Kind := Other_Targets) is
  begin
    if Not_Initialized then
      return;
    end if;
    Set_Guiding_Speed_For (Kind);
    Synchronize_To (Location);
    Set_Status;
  exception
  when Error_Set =>
    null;
  end Synch_To;


  procedure Start_Solving is
  begin
    Set_Status (Solving);
  end Start_Solving;


  procedure End_Solving is
  begin
    Set_Status (Last_State);
  end End_Solving;


  procedure Disconnect is
  begin
    Set_Status (Disconnected);
    Network.Tcp.Close (The_Socket);
  exception
  when others =>
    null;
  end Disconnect;

end M_Zero;
