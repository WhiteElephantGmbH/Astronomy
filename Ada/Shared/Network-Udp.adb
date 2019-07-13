-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Log;

package body Network.Udp is


  Unbound_Socket : Net.Socket_Type;

  Null_Item : constant Data (1..0) := (others => Data_Item(0));

  function Stream_Of (The_Address : System.Address;
                      The_Size    : Natural) return Data is

    subtype Bytes is Network.Data (1..Index(The_Size));
    type Bytes_Access is access all Bytes;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Bytes_Access);

  begin
    if The_Size = 0 then
      return Null_Item;
    else
      return Convert (The_Address).all;
    end if;
  end Stream_Of;


  function Socket_For (Name_Or_Address : String := "";
                       Port            : Port_Number;
                       Receive_Timeout : Duration := 1.0) return Socket is
    The_Socket : Net.Socket_Type;
  begin
    Net.Create_Socket (Socket => The_Socket,
                       Family => Net.Family_Inet,
                       Mode   => Net.Socket_Datagram);
    if Name_Or_Address = "" then
      Net.Bind_Socket (The_Socket, (Family => Net.Family_Inet,
                                    Addr   => Net.Any_Inet_Addr,
                                    Port   => Port));
    else
      Net.Connect_Socket (The_Socket, (Family => Net.Family_Inet,
                                       Addr   => Net.Addresses (Net.Get_Host_By_Name (Name_Or_Address)),
                                       Port   => Port));
    end if;
    return Socket'(The_Socket  => The_Socket,
                   The_Timeout => Receive_Timeout);
  exception
  when others =>
    raise Not_Found;
  end Socket_For;


  function Socket_For (The_Address     : Ip_Address;
                       Port            : Port_Number;
                       Receive_Timeout : Duration := 1.0) return Socket is
    The_Socket : Net.Socket_Type;
  begin
    Net.Create_Socket (Socket => The_Socket,
                       Family => Net.Family_Inet,
                       Mode   => Net.Socket_Datagram);
    Net.Connect_Socket (The_Socket, (Family => Net.Family_Inet,
                                     Addr   => The_Address,
                                     Port   => Port));
    return Socket'(The_Socket  => The_Socket,
                   The_Timeout => Receive_Timeout);
  exception
  when others =>
    raise Not_Found;
  end Socket_For;


  function Broadcast_Socket_For (Home_Address    : Ip_Address;
                                 Port            : Port_Number;
                                 Receive_Timeout : Duration := 1.0) return Socket is
    The_Socket : Net.Socket_Type;
  begin
    Net.Create_Socket (Socket => The_Socket,
                       Family => Net.Family_Inet,
                       Mode   => Net.Socket_Datagram);
    Net.Set_Socket_Option (The_Socket, Net.Socket_Level, (Net.Reuse_Address, True));
    Net.Set_Socket_Option (The_Socket, Net.Socket_Level, (Net.Broadcast, True));
    Net.Bind_Socket (The_Socket, (Family => Net.Family_Inet,
                                  Addr   => Home_Address,
                                  Port   => Port));
    return Socket'(The_Socket  => The_Socket,
                   The_Timeout => Receive_Timeout);
  end Broadcast_Socket_For;


  function Port_Of (Used_Socket : Socket) return Port_Number is
  begin
    return Net.Get_Socket_Name (Used_Socket.The_Socket).Port;
  end Port_Of;


  procedure Handle_Send_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) is
    Error : constant Net.Error_Type := Net.Resolve_Exception (Occurrence);
  begin
    case Error is
    when others =>
      Log.Write ("Network.Udp.Send: " & Error'img, Occurrence);
      raise Transmission_Error;
    end case;
  end Handle_Send_Error;


  procedure Send (Message     : System.Address;
                  Size        : Natural;
                  Used_Socket : Socket) is
    Unused_Last : Offset;
  begin
    Net.Send_Socket (Socket => Used_Socket.The_Socket,
                     Item   => Stream_Of (Message, Size),
                     Last   => Unused_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  end Send;


  procedure Send (Message  : System.Address;
                  Size     : Natural;
                  To       : Address) is
    Unused_Last : Offset;
  begin
    Net.Send_Socket (Socket => Unbound_Socket,
                     To     => To,
                     Item   => Stream_Of (Message, Size),
                     Last   => Unused_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  end Send;


  procedure Send (Message     : System.Address;
                  Size        : Natural;
                  To          : Address;
                  Used_Socket : Socket) is
    Unused_Last : Offset;
  begin
    Net.Send_Socket (Socket => Used_Socket.The_Socket,
                     To     => To,
                     Item   => Stream_Of (Message, Size),
                     Last   => Unused_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  end Send;


  procedure Broadcast (Message     : System.Address;
                       Size        : Natural;
                       Used_Socket : Socket) is
    Unused_Last : Offset;
    To          : Address := Net.Get_Socket_Name (Used_Socket.The_Socket);
  begin
    To.Addr := Broadcast_Address;
    Net.Send_Socket (Socket => Used_Socket.The_Socket,
                     To     => To,
                     Item   => Stream_Of (Message, Size),
                     Last   => Unused_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  end Broadcast;


  procedure Send_Datagram_To (The_Datagram : Datagram;
                              The_Address  : Address;
                              Used_Socket  : Socket) is
    Unused_Last : Offset;
  begin
    Net.Send_Socket (Socket => Used_Socket.The_Socket,
                     To     => The_Address,
                     Item   => Stream_Of (The_Datagram'address, The_Datagram'size / Data_Item'size),
                     Last   => Unused_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  end Send_Datagram_To;


  procedure Send_Datagram (The_Datagram : Datagram;
                           Used_Socket  : Socket) is
    Unused_Last : Offset;
  begin
    Net.Send_Socket (Socket => Used_Socket.The_Socket,
                     Item   => Stream_Of (The_Datagram'address, The_Datagram'size / Data_Item'size),
                     Last   => Unused_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Send_Error (Occurrence);
  end Send_Datagram;


  procedure Assign_To (The_Buffer : System.Address;
                       The_Bytes  : Network.Data) is
    type Byte_Access is access Network.Data(The_Bytes'first..The_Bytes'last);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Byte_Access);
    The_Buffer_Ptr : constant Byte_Access := Convert (The_Buffer);
  begin
    if The_Buffer_Ptr /= null then
      The_Buffer_Ptr.all := The_Bytes;
    end if;
  end Assign_To;


  procedure Handle_Receive_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) with No_Return is
     Error : constant Net.Error_Type := Net.Resolve_Exception (Occurrence);
  begin
    case Error is
    when Net.Connection_Timed_Out =>
      raise Timeout;
    when Net.Message_Too_Long =>
      raise Message_Too_Long;
    when Net.Connection_Reset_By_Peer =>
      -- This error is raised when a datagram is received from a source other than the expected partner
      -- or the partner has used a source port other than what was expected.
      -- Log.Write ("Network.Udp.Receive: Connection Reset By Peer");
      raise Receive_Error;
    when Net.Cannot_Resolve_Error =>
      -- This error can be raised when a socket is closed whilst it is being read
      raise Receive_Error;
    when Net.Socket_Operation_On_Non_Socket =>
      -- The socket is no longer open.
      raise Receive_Error;
    when others =>
      Log.Write ("Network.Udp.Receive: " & Error'img);
      raise Receive_Error;
    end case;
  end Handle_Receive_Error;


  procedure Wait_For (The_Socket   : Net.Socket_Type;
                      The_Duration : Duration) is
  --
  -- Note: Wait_For does NOT wait if the duration is infinite.
  --       This is because the receive following the call to this procedure is assumed to have an infinite wait
  --
    The_Status   : Net.Selector_Status;
    R_Socket_Set : Net.Socket_Set_Type;
    W_Socket_Set : Net.Socket_Set_Type;
  begin
    if The_Duration /= Infinite then
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


  procedure Receive (Message         :        System.Address;
                     Size            : in out Natural;
                     From            : out    Address;
                     Receive_Timeout :        Duration;
                     Used_Socket     :        Socket) is
    The_Last   : Offset;
    The_Item   : Network.Data (1..Offset(Size));
  begin
    Wait_For (Used_Socket.The_Socket, Receive_Timeout);
    Net.Receive_Socket (Socket => Used_Socket.The_Socket,
                        From   => From,
                        Item   => The_Item,
                        Last   => The_Last);
    Assign_To (Message, The_Item (1 .. The_Last));
    Size := Natural(The_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Size := 0;
    Handle_Receive_Error (Occurrence);
  end Receive;


  procedure Receive (Message         :        System.Address;
                     Size            : in out Natural;
                     Receive_Timeout :        Duration;
                     Used_Socket     :        Socket) is
    The_Last : Offset;
    The_Item : Network.Data (1..Offset(Size));
  begin
    Wait_For (Used_Socket.The_Socket, Receive_Timeout);
    Net.Receive_Socket (Socket => Used_Socket.The_Socket,
                        Item   => The_Item,
                        Last   => The_Last);
    Assign_To (Message, The_Item (1 .. The_Last));
    Size := Natural(The_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Size := 0;
    Handle_Receive_Error (Occurrence);
  end Receive;


  procedure Receive (Message     :        System.Address;
                     Size        : in out Natural;
                     From        : out    Address;
                     Used_Socket :        Socket) is
    The_Last   : Offset;
    The_Item   : Network.Data (1..Offset(Size));
  begin
    Wait_For (Used_Socket.The_Socket, Used_Socket.The_Timeout);
    Net.Receive_Socket (Socket => Used_Socket.The_Socket,
                        From   => From,
                        Item   => The_Item,
                        Last   => The_Last);
    Assign_To (Message, The_Item (1 .. The_Last));
    Size := Natural(The_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Size := 0;
    Handle_Receive_Error (Occurrence);
  end Receive;


  procedure Receive (Message     :        System.Address;
                     Size        : in out Natural;
                     Used_Socket :        Socket) is
    The_Last : Offset;
    The_Item : Network.Data (1..Offset(Size));
  begin
    Wait_For (Used_Socket.The_Socket, Used_Socket.The_Timeout);
    Net.Receive_Socket (Socket => Used_Socket.The_Socket,
                        Item   => The_Item,
                        Last   => The_Last);
    Assign_To (Message, The_Item (1 .. The_Last));
    Size := Natural(The_Last);
  exception
  when Occurrence: Net.Socket_Error =>
    Size := 0;
    Handle_Receive_Error (Occurrence);
  end Receive;


  function Datagram_From (Used_Socket : Socket) return Datagram is
    use type Offset;
    Max_Size : constant Offset := Datagram'size / System.Storage_Unit;
    The_Data : Data (1..Max_Size);
    The_Last : Offset;
  begin
    Wait_For (Used_Socket.The_Socket, Used_Socket.The_Timeout);
    Net.Receive_Socket (Socket => Used_Socket.The_Socket,
                        Item   => The_Data,
                        Last   => The_Last);
    declare
      subtype Actual_Data is Data(1 .. The_Last);
      function Convert is new Ada.Unchecked_Conversion (Actual_Data, Datagram);
    begin
      return Convert (The_Data (1..The_Last));
    end;
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Receive_Error (Occurrence);
  end Datagram_From;


  procedure Receive_Datagram (The_Datagram : out Datagram;
                              From         : out Address;
                              Used_Socket  :     Socket) is
    use type Offset;
    Max_Size : constant Offset := Datagram'size / System.Storage_Unit;
    The_Data : Data (1..Max_Size);
    The_Last : Offset;
  begin
    Wait_For (Used_Socket.The_Socket, Used_Socket.The_Timeout);
    Net.Receive_Socket (Socket => Used_Socket.The_Socket,
                        Item   => The_Data,
                        Last   => The_Last,
                        From   => From);
    declare
      subtype Actual_Data is Data(1 .. The_Last);
      function Convert is new Ada.Unchecked_Conversion (Actual_Data, Datagram);
    begin
      The_Datagram := Convert (The_Data (1..The_Last));
    end;
  exception
  when Occurrence: Net.Socket_Error =>
    Handle_Receive_Error (Occurrence);
  end Receive_Datagram;


  procedure Close (Used_Socket : Socket) is
  begin
    Net.Close_Socket (Used_Socket.The_Socket);
  exception
  when others =>
    null;
  end Close;

begin
  Net.Create_Socket (Unbound_Socket, Net.Family_Inet, Net.Socket_Datagram);
  Net.Set_Socket_Option (Unbound_Socket, Net.Socket_Level, (Net.Reuse_Address, True));
end Network.Udp;
