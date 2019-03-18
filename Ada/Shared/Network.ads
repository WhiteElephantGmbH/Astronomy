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

with Ada.Exceptions;
with Ada.Streams;
with GNAT.Sockets;

package Network is

  package Net renames GNAT.Sockets;

  Host_Error         : exception renames Net.Host_Error;
  Not_Found          : exception;
  Receive_Error      : exception;
  Timeout            : exception;
  Transmission_Error : exception;

  subtype Exception_Type is Net.Error_Type;

  function Exception_Kind (Occurrence : Ada.Exceptions.Exception_Occurrence) return Exception_Type
    renames Net.Resolve_Exception;

  subtype Port_Number is Net.Port_Type;

  subtype Address is Net.Sock_Addr_Type;

  subtype Ip_Address is Net.Inet_Addr_Type (Net.Family_Inet);

  Any_Address       : constant Ip_Address := Net.Any_Inet_Addr;
  Broadcast_Address : constant Ip_Address := Net.Broadcast_Inet_Addr;
  Loopback_Address  : constant Ip_Address := Net.Loopback_Inet_Addr;

  Ephemeral_Port : constant Port_Number := Net.Any_Port;

  type Ip_Addresses is array (Positive range <>) of Ip_Address;

  function Image_Of (Addr : Ip_Address) return String renames Net.Image;

  function Ip_Address_Of (Dot_Notation : String) return Ip_Address renames Net.Inet_Addr;

  function Ip_Address_Of_Host (Host_Name : String) return Ip_Address;
  --
  -- Returns the IP address of the host specified either by name or by dot notation.
  -- The exception Host_Error is raised if the specfied host is not available.
  --

  function Address_Of (Addr : Ip_Address;
                       Port : Port_Number) return Address;

  function Home_Addresses return Ip_Addresses;

private

  subtype Data_Item is Ada.Streams.Stream_Element;
  subtype Data      is Ada.Streams.Stream_Element_Array;
  subtype Offset    is Ada.Streams.Stream_Element_Offset;
  subtype Index     is Ada.Streams.Stream_Element_Count;

end Network;
