-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with System;

package Os.Pipe is

  Access_Denied    : exception; -- wrong access mode
  Bad_Pipe         : exception; -- invalid pipe state
  Broken           : exception; -- broken pipe
  Invalid_Name     : exception; -- incorrect pipe name
  More_Data        : exception; -- buffer too small
  Name_In_Use      : exception; -- open pipe with same same
  No_Data          : exception; -- no more data (server closed)
  No_Handle        : exception; -- pipe not opened
  No_Server        : exception; -- no server available
  Not_Server       : exception; -- no timeout for client open
  Timeout          : exception;
  Unknown_Error    : exception;
  Write_Incomplete : exception; -- write operation not completed

  type Role is (Client, Server);

  type Access_Mode is (Duplex, Inbound, Outbound);

  type Handle is private;

  type Get_Callback is access procedure (Item : String);

  Time_Delta : constant := 0.1;
  type Timer is delta Time_Delta range Time_Delta .. 100.0 + Time_Delta with Small => Time_Delta;

  Minimum_Timeout : constant Timer := Timer'first;
  Maximum_Timeout : constant Timer := Timer'last - Time_Delta;
  Forever         : constant Timer := Timer'last;

  Maximum_Client_Wait_Time : constant Timer := 10.0;

  procedure Open (The_Pipe                 : in out Handle;
                  Name                     :        String;
                  Kind                     :        Role;
                  Mode                     :        Access_Mode;
                  Size                     :        Natural;     -- maximum data buffer size
                  Wait_Time                :        Timer := Forever;
                  Get_Call                 :        Get_Callback := null;
                  Allow_Remote_Connections :        Boolean := False)
  with Pre => Kind = Server or Wait_Time <= Maximum_Client_Wait_Time;
  -- opens or reopens a pipe

  procedure Close (The_Pipe : in out Handle);
  -- no exceptions

  procedure Read (From_Pipe :     Handle;
                  Data      :     System.Address;
                  Length    : out Natural;
                  Wait_Time :     Timer := Forever);

  procedure Write (To_Pipe : Handle;
                   Data    : System.Address;
                   Length  : Natural);

  procedure Put (To_Pipe : Handle;
                 Item    : String);

private
  type Named_Pipe;
  type Handle is access Named_Pipe;
end Os.Pipe;
