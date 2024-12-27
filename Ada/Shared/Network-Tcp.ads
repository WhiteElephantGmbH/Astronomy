-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Unsigned;

package Network.Tcp is

  Aborted      : exception;
  Bad_Protocol : exception;
  Port_In_Use  : exception;
  No_Client    : exception;
  Usage_Error  : exception;

  type Protocol is (Raw, Counted, LE16_Included, LE32_Excluded);

  type Connection is (Local, Remote);
  --
  -- A tcp connection has two sockets, one local the other remote
  --

  type Socket is private;

  No_Socket : constant Socket;

  Forever            : constant Duration := Duration'last;
  Use_Socket_Timeout : constant Duration := Duration'first;

  subtype Positive_Duration is Duration range Duration'delta .. Forever;

  function Socket_For (Name_Or_Address : String;
                       The_Port        : Port_Number;
                       The_Protocol    : Protocol;
                       Receive_Timeout : Positive_Duration := Forever) return Socket;

  function Socket_For (The_Address     : Ip_Address;
                       The_Port        : Port_Number;
                       The_Protocol    : Protocol;
                       Receive_Timeout : Positive_Duration := Forever) return Socket;

  function Socket_For (The_Address     : Address;
                       The_Protocol    : Protocol;
                       Receive_Timeout : Positive_Duration := Forever) return Socket;

  function Port_Of (The_Socket     : Socket;
                    The_Connection : Connection := Remote) return Port_Number;
  -- Returns the IP address

  function Ip_Address_Of (The_Socket     : Socket;
                          The_Connection : Connection := Remote) return Ip_Address;
  -- Returns the IP address

  function Image_Of (The_Socket     : Socket;
                     The_Connection : Connection := Remote) return String;
  -- Returns the remote IP address in dot notation followed by a colon and then the port number in decimal

  procedure Set_No_Delay (The_Socket : Socket);
  -- Do not delay sends (Coalesce data = TCP_NODELAY)

  procedure Set_Keep_Alive (The_Socket : Socket);
  -- Send periodic TCP Keep_Alive packets to establish whether the connection is still active
  -- Note that on windows the first Keep_Alive will be sent according to the Registery REG_DWORD entry
  -- HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\KeepAliveTime
  -- If this registry entry is not present then the system defaults to 2 hours (7'200'000 milliseconds)
  --

  procedure Set_Size_Of_Transmit_Buffer (The_Socket : Socket;
                                         The_Size   : Natural);

  procedure Change_Protocol (Used_Socket  : in out Socket;
                             The_Protocol :        Protocol);

  procedure Change_Receive_Timeout (Used_Socket     : in out Socket;
                                    Receive_Timeout :        Positive_Duration);

  generic
    type Message (<>) is private;
  function Message_From (Used_Socket     : Socket;
                         Receive_Timeout : Duration := Use_Socket_Timeout) return Message;
  generic
    type Element is private;
    type Elements is array (Positive range <>) of Element;
  function Elements_From (Used_Socket     : Socket;
                          Receive_Timeout : Duration := Use_Socket_Timeout) return Elements;
  generic
    type Message (<>) is private;
  procedure Send_Message (The_Message : Message;
                          Used_Socket : Socket);

  generic
    type Element is private;
    type Elements is array (Positive range <>) of Element;
  procedure Send_Elements (The_Elements : Elements;
                           Used_Socket  : Socket);

  function String_From (Used_Socket     : Socket;
                        Receive_Timeout : Duration := Use_Socket_Timeout) return String;
  -- Note: Protocol can't be raw

  function String_From (Used_Socket     : Socket;
                        Receive_Timeout : Duration := Use_Socket_Timeout) return Unsigned.Byte_String;
  -- Note: Protocol can't be raw

  function Raw_Character_From (Used_Socket     : Socket;
                               Receive_Timeout : Duration := Use_Socket_Timeout) return Character;

  function Raw_String_From (Used_Socket     : Socket;
                            Terminator      : Character;
                            Receive_Timeout : Duration := Use_Socket_Timeout) return String;

  procedure Send (The_String  : Unsigned.Byte_String;
                  Used_Socket : Socket);

  procedure Send (The_String  : String;
                  Used_Socket : Socket);

  procedure Close (Used_Socket : Socket);

  type Listener_Socket is limited private;

  procedure Create_Socket_For (The_Port     :     Port_Number;
                               The_Protocol :     Protocol;
                               The_Listener : out Listener_Socket);

  procedure Accept_Client_From (The_Listener :     Listener_Socket;
                                The_Client   : out Socket);
  -- Note: The_Client inherits the protocol of the Listener_Socket with infinite timeout

  procedure Accept_Client_From (The_Listener   :     Listener_Socket;
                                The_Client     : out Socket;
                                Client_Address : out Ip_Address;
                                The_Timeout    :     Duration := Forever);
  -- Note: The_Client inherits the protocol of the Listener_Socket with infinite timeout

  procedure Close (The_Listener : Listener_Socket);
  -- aborts the procedure Accept_Client_From

private
  type Listener_Socket is limited record
    The_Selector : Net.Selector_Type;
    The_Socket   : Net.Socket_Type := Net.No_Socket;
    The_Protocol : Protocol := Raw;
  end record;

  type Socket is record
    The_Socket   : Net.Socket_Type;
    The_Timeout  : Duration;
    The_Protocol : Protocol;
  end record;

  No_Socket   : constant Socket := (Net.No_Socket, 0.0, Raw);

end Network.Tcp;
