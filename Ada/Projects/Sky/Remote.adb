-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Network.Tcp;
with Parameter;
with Traces;

package body Remote is

  package Log is new Traces ("Remote");

  Is_Tracking : Boolean := False;


  procedure Send (Info : String) is
    The_Socket : Network.Tcp.Socket;
  begin
    Log.Write (Info);
    The_Socket := Network.Tcp.Socket_For (The_Address     => Parameter.Remote_Address,
                                          The_Port        => Parameter.Remote_Port,
                                          The_Protocol    => Network.Tcp.LE16_Included,
                                          Receive_Timeout => 0.1);
    Network.Tcp.Send (Info, The_Socket);
    declare
      Reply : constant String := Network.Tcp.String_From (The_Socket);
    begin
      if Reply = Info then
        Log.Write ("information accepted");
      else
        Log.Error ("bad reply for " & Info & ": " & Reply );
      end if;
    end;
    Network.Tcp.Close (The_Socket);
  exception
  when Network.Not_Found
    |  Network.Host_Error
    |  Network.Tcp.No_Client
    |  Network.Timeout
    |  Network.Transmission_Error =>
    begin
      Network.Tcp.Close (The_Socket);
    exception
    when others =>
      null;
    end;
    Log.Warning ("server not found");
  when Occurrence: others =>
    Log.Error (Ada.Exceptions.Exception_Name (Occurrence) & ": " & Network.Net.Resolve_Exception (Occurrence)'img);
  end Send;


  procedure Define (Target : String;
                    State  : Telescope.State) is
    use type Telescope.State;
  begin
    if Is_Tracking then
      if State /= Telescope.Tracking then
        Send (Parameter.Telescope_Name);
        Is_Tracking := False;
      end if;
    else
      if State = Telescope.Tracking then
        Send (Parameter.Telescope_Name & ":" & Target);
        Is_Tracking := True;
      end if;
    end if;
  end Define;

end Remote;
