-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with System;

package Network.Udp is

  Message_Too_Long : exception;

  type Socket is private;

  No_Socket : constant Socket;

  Infinite : constant Duration := 0.0;  -- Timeout durations of zero specify infinite timeouts rather than immediate.


  function Socket_For (Name_Or_Address : String := "";
                       Port            : Port_Number;
                       Receive_Timeout : Duration := 1.0) return Socket;

  function Socket_For (The_Address     : Ip_Address;
                       Port            : Port_Number;
                       Receive_Timeout : Duration := 1.0) return Socket;

  function Broadcast_Socket_For (Home_Address    : Ip_Address;
                                 Port            : Port_Number;
                                 Receive_Timeout : Duration := 1.0) return Socket;

  function Port_Of (Used_Socket : Socket) return Port_Number;

  procedure Broadcast (Message     : System.Address;
                       Size        : Natural;
                       Used_Socket : Socket);

  procedure Send (Message  : System.Address;
                  Size     : Natural;
                  To       : Address);

  procedure Send (Message     : System.Address;
                  Size        : Natural;
                  To          : Address;
                  Used_Socket : Socket);

  procedure Send (Message     : System.Address;
                  Size        : Natural;
                  Used_Socket : Socket);

  generic
  type Datagram (<>) is private;
  procedure Send_Datagram_To (The_Datagram : Datagram;
                              The_Address  : Address;
                              Used_Socket  : Socket);

  generic
  type Datagram (<>) is private;
  procedure Send_Datagram (The_Datagram : Datagram;
                           Used_Socket  : Socket);


  procedure Receive (Message         :        System.Address;
                     Size            : in out Natural;
                     From            : out    Address;
                     Receive_Timeout :        Duration;
                     Used_Socket     :        Socket);

  procedure Receive (Message         :        System.Address;
                     Size            : in out Natural;
                     Receive_Timeout :        Duration;
                     Used_Socket     :        Socket);

  procedure Receive (Message     :        System.Address;
                     Size        : in out Natural;
                     From        : out    Address;
                     Used_Socket :        Socket);

  procedure Receive (Message     :        System.Address;
                     Size        : in out Natural;
                     Used_Socket :        Socket);

  generic
  type Datagram (<>) is private;
  function Datagram_From (Used_Socket : Socket) return Datagram;

  generic
  type Datagram (<>) is private;
  procedure Receive_Datagram (The_Datagram : out Datagram;
                              From         : out Address;
                              Used_Socket  :     Socket);

  procedure Close (Used_Socket : Socket);


private

  type Socket is record
    The_Socket  : Net.Socket_Type;
    The_Timeout : Duration;
  end record;

  No_Socket : constant Socket:= Socket'(Net.No_Socket, Infinite);

end Network.Udp;
