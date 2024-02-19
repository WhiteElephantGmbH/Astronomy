-- *********************************************************************************************************************
-- *                               (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Error;
with Section;

package body Clock.Parameter is

  Ip_Address_Key : constant String := Section.Ip_Address_Key;
  Port_Key       : constant String := Section.Port_Key;


  procedure Connect_Clock is

    Name_Or_Address  : constant String := Section.String_Value_Of (Ip_Address_Key);
    Datagram_Timeout : constant Duration := 0.3;

  begin
    if Name_Or_Address /= "" then
      The_Udp_Socket := Network.Udp.Socket_For (Name_Or_Address => Name_Or_Address,
                                                Port            => Section.Port_For (Id),
                                                Receive_Timeout => Datagram_Timeout);
      Log.Write ("Clock connected to " & Name_Or_Address);
    end if;
  exception
  when Network.Not_Found =>
    Error.Raise_With ("Clock not connected to " & Name_Or_Address);
  end Connect_Clock;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    Connect_Clock;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Ip_Address_Key & " = 169.254.42.43");
    Put (Port_Key & "       = 44422");
  end Defaults;

end Clock.Parameter;
