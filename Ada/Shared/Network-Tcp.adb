-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Calendar;
with Ada.Unchecked_Conversion;
with Log;
with System;
with Text;

package body Network.Tcp is

  Max_Data_Size : constant := 1_000_000;

  function Socket_For (The_Address     : Ip_Address;
                       The_Port        : Port_Number;
                       The_Protocol    : Protocol;
                       Receive_Timeout : Positive_Duration := Forever) return Socket is
    The_Socket : Net.Socket_Type;
    The_Status : Net.Selector_Status;
    use type Net.Selector_Status;
  begin
    Net.Create_Socket (Socket => The_Socket,
                       Family => Net.Family_Inet,
                       Mode   => Net.Socket_Stream);
    --
    -- Note: We use Connect_Socket with timeout because the default timeout is otherwise too long
    --       Under Win10 an attempt to connect to a unopened port on a valid IP address takes 21 seconds to timeout.
    --
    Net.Connect_Socket (Socket  => The_Socket,
                        Server  => (Family => Net.Family_Inet,
                                    Addr   => The_Address,
                                    Port   => The_Port),
                        Timeout => 1.0,
                        Status  => The_Status);
    if The_Status = Net.Completed then
      Net.Set_Socket_Option (The_Socket, Net.Socket_Level, (Net.Receive_Timeout, Net.Forever));
      return (The_Socket, Receive_Timeout, The_Protocol);
    else
      Net.Close_Socket (The_Socket);
      raise Not_Found;
    end if;
  end Socket_For;


  function Socket_For (Name_Or_Address : String;
                       The_Port        : Port_Number;
                       The_Protocol    : Protocol;
                       Receive_Timeout : Positive_Duration := Forever) return Socket is
  begin
    return Socket_For (Net.Addresses (Net.Get_Host_By_Name (Name_Or_Address)), The_Port, The_Protocol, Receive_Timeout);
  end Socket_For;


  procedure Set_No_Delay (Used_Socket : in out Socket) is
  begin
    Net.Set_Socket_Option (Used_Socket.The_Socket, Net.Socket_Level, (Net.No_Delay, True));
  end Set_No_Delay;


  procedure Change_Protocol (Used_Socket  : in out Socket;
                             The_Protocol :        Protocol) is
  begin
    Used_Socket.The_Protocol := The_Protocol;
  end Change_Protocol;


  procedure Change_Receive_Timeout (Used_Socket     : in out Socket;
                                    Receive_Timeout :        Positive_Duration) is
  begin
    Used_Socket.The_Timeout := Receive_Timeout;
  end Change_Receive_Timeout;


  procedure Handle_Send_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) with No_Return is
    Error : constant Net.Error_Type := Net.Resolve_Exception (Occurrence);
  begin
    case Error is
    when Net.Connection_Reset_By_Peer =>
      raise No_Client;
    when others =>
      Log.Write ("Network.Tcp.Handle_Send_Error: " & Error'img);
      Log.Write ("Network.Tcp.Handle_Send_Error: ", Occurrence);
    end case;
    raise Transmission_Error;
  end Handle_Send_Error;


  procedure Send (The_Data   : Data;
                  The_Socket : Net.Socket_Type) is
    Unused  : Index;
  begin
    -- Note: Transmission is made without timeout
    Net.Send_Socket (The_Socket, The_Data, Unused);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  when Occurrence: others =>
    Log.Write ("Network.Tcp.Send", Occurrence);
    raise;
  end Send;


  procedure Send_Header_To (Used_Socket  : Socket;
                            The_Value    : Natural) is
  begin
    case Used_Socket.The_Protocol is
    when Raw =>
      null;
    when LE16_Included =>
      declare
        use type Unsigned.Word;
        Header : aliased constant Unsigned.Word := Unsigned.Word(The_Value) + 2;
        subtype Word_Data is Data (1 .. Index(Unsigned.Word'size / System.Storage_Unit));
        function Convert is new Ada.Unchecked_Conversion (Unsigned.Word, Word_Data);
      begin
        Send (Convert (Header), Used_Socket.The_Socket);
      end;
    when LE32_Excluded =>
      declare
        Header : aliased constant Unsigned.Longword := Unsigned.Longword(The_Value);
        subtype Longword_Data is Data (1 .. Index(Unsigned.Longword'size / System.Storage_Unit));
        function Convert is new Ada.Unchecked_Conversion (Unsigned.Longword, Longword_Data);
      begin
        Send (Convert (Header), Used_Socket.The_Socket);
      end;
    end case;
  end Send_Header_To;


  procedure Send_Message (The_Message : Message;
                          Used_Socket : Socket) is
    use type Index;
    Data_Size : constant Index := The_Message'size / System.Storage_Unit;
    subtype Message_Data is Data (1 .. Data_Size);
    function Convert is new Ada.Unchecked_Conversion (Message, Message_Data);
    The_Data : constant Message_Data := Convert (The_Message);
  begin
    if Used_Socket.The_Protocol /= Raw then
      Send_Header_To (Used_Socket, The_Data'length);
    end if;
    Send (The_Data, Used_Socket.The_Socket);
  end Send_Message;


  procedure Send_Elements (The_Elements : Elements;
                           Used_Socket  : Socket) is
    Elements_Size : constant Natural := The_Elements'size / System.Storage_Unit;
  begin
    if Used_Socket.The_Protocol /= Raw then
      Send_Header_To (Used_Socket, Elements_Size);
    end if;
    if The_Elements'length > 0 then
      declare
        The_Data : aliased constant Elements := The_Elements;
        subtype Elements_Data is Data (1 .. Offset(Elements_Size));
        function Convert is new Ada.Unchecked_Conversion (Elements, Elements_Data);
      begin
        Send (Convert (The_Data), Used_Socket.The_Socket);
      end;
    end if;
  end Send_Elements;



  procedure Handle_Receive_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) with No_Return is
    Error : constant Net.Error_Type := Net.Resolve_Exception (Occurrence);
  begin
    case Error is
    when Net.Connection_Timed_Out =>
      raise Timeout;
    when Net.Connection_Reset_By_Peer =>
      raise No_Client;
    when Net.Software_Caused_Connection_Abort =>
      raise Aborted;
    when others =>
      Log.Write ("Network.Tcp.Handle_Receive_Error: " & Error'img);
    end case;
    raise Receive_Error;
  end Handle_Receive_Error;


  procedure Wait_For (The_Socket   : Net.Socket_Type;
                      The_Duration : Duration) is
    The_Status   : Net.Selector_Status;
    R_Socket_Set : Net.Socket_Set_Type;
    W_Socket_Set : Net.Socket_Set_Type;
  begin
    if The_Duration < Net.Forever then
      Net.Set (R_Socket_Set, The_Socket);
      Net.Check_Selector (Selector     => Net.Null_Selector,
                          R_Socket_Set => R_Socket_Set,
                          W_Socket_Set => W_Socket_Set,
                          Status       => The_Status,
                          Timeout      => The_Duration);
      case The_Status is
      when Net.Completed =>
        null; -- Socket is ready for reading
      when Net.Expired =>
        raise Timeout;
      when Net.Aborted =>
        raise Receive_Error;
      end case;
    end if;
  end Wait_For;


  function Get_Data_From  (The_Socket  : Net.Socket_Type;
                           The_Amount  : Index;
                           The_Timeout : Duration) return Data is
    use type Ada.Calendar.Time;
    use type Index;
    The_Deadline : constant Ada.Calendar.Time := Ada.Calendar.Clock + The_Timeout;
    The_Buffer   : Data (1.. The_Amount);
    The_Index    : Offset := The_Buffer'first;
    The_Last     : Index;
    Next_Timeout : Duration := The_Timeout;
  begin
    -- Note: Net.Wait_For_A_Full_Reception is not supported under Windows
    --       therefore we must loop to collect the expected amount
    -- Note: If nothing is recived by Receive_Socket within the timeout period it returns
    --       Connection_Timed_Out.
    loop
      Wait_For (The_Socket, Next_Timeout);
      Net.Receive_Socket (The_Socket, The_Buffer (The_Index .. The_Buffer'last), The_Last);
      exit when The_Last = The_Buffer'last;  -- All sent
      if (The_Last = 0) and (Ada.Calendar.Clock < The_Deadline) then
        -- Last=0 could mean either no data was sent or that connection has been broken.
        -- If Receive_Socket returns before the timeout has expired with Last=0 we assume
        -- that this because the connection has been broken.
        -- However sometimes breaking the connection raises an exception with reason
        -- Connection_Reset_By_Peer so this needs to be handled as well.
        raise No_Client;
      end if;
      The_Index := The_Last + 1;
      if The_Timeout /= Net.Forever then
        Next_Timeout := The_Deadline - Ada.Calendar.Clock;
        if Next_Timeout <= 0.0 then
          raise Timeout;
        end if;
      end if;
    end loop;
    return The_Buffer;
  exception
  when No_Client =>
    raise;
  when Timeout =>
    if (The_Amount > 1) and (The_Index > The_Buffer'first) then
      -- A counted protocol that has already received part of the counted data.
      -- A timeout here effectively breaks the protocol so we need to close the session.
      Net.Close_Socket (The_Socket);
    end if;
    raise;
  when Occurrence: Net.Socket_Error =>
    Handle_Receive_Error (Occurrence);
  when Occurrence: others => -- Unexpected exception
    Log.Write ("Network.Tcp.Get_Data_From", Occurrence);
    raise;
  end Get_Data_From;


  function Receive_Header_From (Used_Socket  : Socket;
                                With_Timeout : Duration) return Index is
  begin
    case Used_Socket.The_Protocol is
    when Raw =>
      raise Program_Error;
    when LE16_Included =>
      declare
        The_Size : Unsigned.Word;
        function Convert is new Ada.Unchecked_Conversion (Data, Unsigned.Word);
        use type Unsigned.Word;
      begin
        The_Size := Convert(Get_Data_From (Used_Socket.The_Socket,
                                           Index (The_Size'size / System.Storage_Unit),
                                           With_Timeout));
        if The_Size < 2 then
          raise Bad_Protocol;
        end if;
        return Index(The_Size - 2);
      end;
    when LE32_Excluded =>
      declare
        The_Size : Unsigned.Longword;
        function Convert is new Ada.Unchecked_Conversion (Data, Unsigned.Longword);
      begin
        The_Size := Convert(Get_Data_From (Used_Socket.The_Socket,
                                           Index (The_Size'size / System.Storage_Unit),
                                           With_Timeout));
        return Index(The_Size);
      end;
    end case;
  end Receive_Header_From;


  function Message_From (Used_Socket     : Socket;
                         Receive_Timeout : Duration := Use_Socket_Timeout) return Message is
    use type Ada.Calendar.Time;
    use type Index;
    Max_Message_Size : constant Index  := (Message'size / System.Storage_Unit);
    The_Message_Size : Index;
    The_Timeout      : Duration;
    The_Deadline     : Ada.Calendar.Time;
  begin
    if Receive_Timeout = Use_Socket_Timeout then
      The_Timeout := Used_Socket.The_Timeout;
    else
      The_Timeout := Receive_Timeout;
    end if;
    if The_Timeout > Net.Forever then
      The_Timeout := Net.Forever;
    end if;
    if Used_Socket.The_Protocol = Raw then
      The_Message_Size := Max_Message_Size;
    elsif The_Timeout = Net.Forever then
      The_Message_Size := Receive_Header_From (Used_Socket, Net.Forever);
    elsif The_Timeout <= Net.Immediate then
      raise Timeout;
    else
      The_Deadline     := Ada.Calendar.Clock + The_Timeout;
      The_Message_Size := Receive_Header_From (Used_Socket, The_Timeout);
    end if;
    if (The_Message_Size = 0) or (The_Message_Size > Max_Message_Size) then
      raise Bad_Protocol;
    end if;
    if (Used_Socket.The_Protocol /= Raw) and (The_Timeout /= Net.Forever) then
      The_Timeout := The_Deadline - Ada.Calendar.Clock;
    end if;
    if The_Timeout <= Net.Immediate then
      raise Timeout;
    end if;
    begin
      declare
        The_Data : constant Data := Get_Data_From (Used_Socket.The_Socket, The_Message_Size, The_Timeout);
        function Convert is new Ada.Unchecked_Conversion (Data, Message);
      begin
        return Convert(The_Data);
      end;
    exception
    when Timeout =>
      if Used_Socket.The_Protocol /= Raw then -- We have read the header of a counted protocol
        -- If we timeout on the contents we will break the protocol. Therefore need to close the session
        Net.Close_Socket (Used_Socket.The_Socket);
      end if;
      raise;
    end;
  end Message_From;


  function Elements_From (Used_Socket     : Socket;
                          Receive_Timeout : Duration := Use_Socket_Timeout) return Elements is
    use type Index;
    use type Ada.Calendar.Time;
    The_Size     : Index;
    The_Timeout  : Duration;
    The_Deadline : Ada.Calendar.Time;
  begin
    if Receive_Timeout = Use_Socket_Timeout then
      The_Timeout := Used_Socket.The_Timeout;
    else
      The_Timeout := Receive_Timeout;
    end if;
    if The_Timeout >= Net.Forever then
      The_Timeout := Net.Forever;
    else
      The_Deadline := Ada.Calendar.Clock + The_Timeout;
    end if;
    if Used_Socket.The_Protocol = Raw then
      raise Usage_Error;
    else
      The_Size := Receive_Header_From (Used_Socket, The_Timeout);
    end if;
    if The_Size > Max_Data_Size then
      raise Bad_Protocol;
    end if;
    if The_Size = 0 then
      declare
        No_Elements : constant Elements(1..0) := [];
      begin
        return No_Elements;
      end;
    end if;
    declare
      Data_Size   : constant Index := The_Size;
      The_Data    : Data (1..Data_Size);
      subtype Return_Elements is Elements (1 .. Positive(The_Size));
      function Convert is new Ada.Unchecked_Conversion (Data, Return_Elements);
    begin
      if The_Timeout /= Net.Forever then
        The_Timeout := The_Deadline - Ada.Calendar.Clock;
      end if;
      if The_Timeout <= Net.Immediate then
        raise Timeout;
      end if;
      The_Data := Get_Data_From (Used_Socket.The_Socket, Data_Size, The_Timeout);
      return Convert(The_Data);
    exception
    when Timeout =>
      -- We have read the header of a counted protocol so if we timeout on the contents we will break the protocol
      -- and therefore need to close the session
      Net.Close_Socket (Used_Socket.The_Socket);
      raise;
    end;
  end Elements_From;


  function Received_String is new Elements_From (Character, String);


  function String_From (Used_Socket     : Socket;
                        Receive_Timeout : Duration := Use_Socket_Timeout) return String is
  begin
    return Received_String (Used_Socket, Receive_Timeout);
  end String_From;


  function Received_Byte_String is new Elements_From (Unsigned.Byte, Unsigned.Byte_String);

  function String_From (Used_Socket     : Socket;
                        Receive_Timeout : Duration := Use_Socket_Timeout) return Unsigned.Byte_String is
  begin
    return Received_Byte_String (Used_Socket, Receive_Timeout);
  end String_From;


  function Raw_Character_From (Used_Socket     : Socket;
                               Receive_Timeout : Duration := Use_Socket_Timeout) return Character is
    use type Ada.Calendar.Time;
    The_Timeout  : Duration;
    The_Deadline : Ada.Calendar.Time;
    The_Data     : Data (1..1);
    function Convert is new Ada.Unchecked_Conversion (Data_Item, Character);
  begin
    if Used_Socket.The_Protocol /= Raw then
      raise Usage_Error;
    end if;
    if Receive_Timeout = Use_Socket_Timeout then
      The_Timeout := Used_Socket.The_Timeout;
    else
      The_Timeout := Receive_Timeout;
    end if;
    if The_Timeout >= Net.Forever then
      The_Timeout := Net.Forever;
    else
      The_Deadline := Ada.Calendar.Clock + The_Timeout;
    end if;
    The_Data := Get_Data_From (Used_Socket.The_Socket, 1, The_Timeout);
    if The_Timeout /= Net.Forever then
      The_Timeout := The_Deadline - Ada.Calendar.Clock;
    end if;
    if The_Timeout <= Net.Immediate then
      raise Timeout;
    end if;
    return Convert(The_Data(The_Data'first));
  end Raw_Character_From;


  function Raw_String_From (Used_Socket     : Socket;
                            Terminator      : Character;
                            Receive_Timeout : Duration := Use_Socket_Timeout) return String is
    use type Ada.Calendar.Time;
    use type Text.String;
    The_Timeout  : Duration;
    The_Deadline : Ada.Calendar.Time;
  begin
    if Used_Socket.The_Protocol /= Raw then
      raise Usage_Error;
    end if;
    if Receive_Timeout = Use_Socket_Timeout then
      The_Timeout := Used_Socket.The_Timeout;
    else
      The_Timeout := Receive_Timeout;
    end if;
    if The_Timeout >= Net.Forever then
      The_Timeout := Net.Forever;
    else
      The_Deadline := Ada.Calendar.Clock + The_Timeout;
    end if;
    declare
      The_Data      : Data (1..1);
      The_Character : Character;
      The_String    : Text.String;
      function Convert is new Ada.Unchecked_Conversion (Data_Item, Character);
    begin
      loop
        The_Data := Get_Data_From (Used_Socket.The_Socket, 1, The_Timeout);
        if The_Timeout /= Net.Forever then
          The_Timeout := The_Deadline - Ada.Calendar.Clock;
        end if;
        if The_Timeout <= Net.Immediate then
          raise Timeout;
        end if;
        The_Character := Convert(The_Data(The_Data'first));
        The_String.Append (The_Character);
        exit when The_Character = Terminator;
      end loop;
      return +The_String;
    end;
  end Raw_String_From;


  procedure Send_String is new Send_Elements (Character, String);

  procedure Send (The_String  : String;
                  Used_Socket : Socket) is
  begin
    Send_String (The_String, Used_Socket);
  end Send;


  procedure Send_Byte_String is new Send_Elements (Unsigned.Byte, Unsigned.Byte_String);

  procedure Send (The_String  : Unsigned.Byte_String;
                  Used_Socket : Socket) is
  begin
    Send_Byte_String (The_String, Used_Socket);
  end Send;


  procedure Close (Used_Socket : Socket) is
  begin
    Net.Close_Socket (Used_Socket.The_Socket);
  end Close;


  procedure Create_Socket_For (The_Port     :     Port_Number;
                               The_Protocol :     Protocol;
                               The_Listener : out Listener_Socket) is

    The_Address  : constant Address := Address_Of (Any_Address, The_Port);

  begin
    Net.Create_Selector (The_Listener.The_Selector);
    Net.Create_Socket (Socket => The_Listener.The_Socket,
                       Family => Net.Family_Inet,
                       Mode   => Net.Socket_Stream);
    Net.Set_Socket_Option (Socket => The_Listener.The_Socket,
                           Level  => Net.Socket_Level,
                           Option => (Net.Reuse_Address, True));
    Net.Bind_Socket (The_Listener.The_Socket, The_Address);
    Net.Listen_Socket (The_Listener.The_Socket);
    The_Listener.The_Protocol := The_Protocol;
  exception
  when others =>
    raise Port_In_Use;
  end Create_Socket_For;


  procedure Accept_Client_From (The_Listener :     Listener_Socket;
                                The_Client   : out Socket) is
    Unused_Client_Address : Ip_Address;
  begin
    Accept_Client_From (The_Listener, The_Client, Unused_Client_Address);
  end Accept_Client_From;


  procedure Accept_Client_From (The_Listener   :     Listener_Socket;
                                The_Client     : out Socket;
                                Client_Address : out Ip_Address;
                                The_Timeout    :     Duration := Forever) is

    The_Select_Timeout : Duration := The_Timeout;

    The_Status   : Net.Selector_Status;
    R_Socket_Set : Net.Socket_Set_Type;
    W_Socket_Set : Net.Socket_Set_Type;

    The_Client_Socket  : Socket := (Net.No_Socket, Forever, The_Listener.The_Protocol);
    The_Client_Address : Address;

  begin
    if The_Select_Timeout > Net.Forever then
      The_Select_Timeout := Net.Forever;
    elsif The_Select_Timeout <= Net.Immediate then
      raise Timeout;
    end if;
    Net.Set (R_Socket_Set, The_Listener.The_Socket);
    Net.Check_Selector (Selector     => The_Listener.The_Selector,
                        R_Socket_Set => R_Socket_Set,
                        W_Socket_Set => W_Socket_Set,
                        Status       => The_Status,
                        Timeout      => The_Select_Timeout);
    case The_Status is
    when Net.Completed =>
      null; -- Socket is ready for reading
    when Net.Expired =>
      raise Timeout;
    when Net.Aborted =>
      raise Aborted;
    end case;
    Net.Accept_Socket (The_Listener.The_Socket, The_Client_Socket.The_Socket, The_Client_Address);
    Client_Address := The_Client_Address.Addr;
    The_Client := The_Client_Socket;
  end Accept_Client_From;


  procedure Close (The_Listener : Listener_Socket) is
  begin
    Net.Close_Socket (The_Listener.The_Socket);
    Net.Abort_Selector (The_Listener.The_Selector);
  exception
  when others =>
    null;
  end Close;

end Network.Tcp;
