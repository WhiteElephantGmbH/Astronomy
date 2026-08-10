-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Lx200;
with Traces;
with Network.Tcp;

package body Ten_Micron is

  Id : constant String := "10micron";

  package Log is new Traces (Id);

  Loop_Time    : constant Duration := 0.5;
  Unknown_Time : constant Time.JD := 0.0;


  task Handler is
    entry Get (The_Time : out Time.JD);
    entry Shutdown;
  end Handler;


  function Has_New (The_Time : out Time.JD) return Boolean is
    use type Time.JD;
  begin
    select
      Handler.Get (The_Time);
      return The_Time /= Unknown_Time;
    else
      return False;
    end select;
  end Has_New;


  The_Socket : Network.Tcp.Socket;

  function Set (Item : Time.JD) return Boolean is
    Command : constant String := Lx200.String_Of (Lx200.Set_Julian_Date, Lx200.Julian_Date_Of (Item));
  begin
    Network.Tcp.Send (The_String  => Command,
                      Used_Socket => The_Socket);
    if Network.Tcp.Raw_Character_From (The_Socket) = '1' then
      return True;
    end if;
    return False;
  exception
  when others =>
    return False;
  end Set;


  procedure Shutdown is
  begin
    Handler.Shutdown;
  end Shutdown;


  task body Handler is

    Is_Connected : Boolean := False;

    procedure Connect is
      Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
      Receive_Timeout : constant Duration := Loop_Time;
    begin
      The_Socket := Network.Tcp.Socket_For (The_Address     => Network.Ip_Address_Of ("169.254.42.42"),
                                            The_Port        => Network.Port_Number (3490),
                                            The_Protocol    => Socket_Protocol,
                                            Receive_Timeout => Receive_Timeout);
      Is_Connected := True;
      Log.Write ("Connected");
    exception
    when others =>
      Is_Connected := False;
    end Connect;


    procedure Disconnect is
    begin
      begin
        Network.Tcp.Close (The_Socket);
      exception
      when others =>
        null;
      end;
      Is_Connected := False;
      Log.Write ("Disconnected");
    end Disconnect;


    function Julian_Date return Time.JD is
      Command : constant String := Lx200.String_Of (Lx200.Get_Julian_Date);
    begin
      Network.Tcp.Send (The_String  => Command,
                        Used_Socket => The_Socket);
      declare
        Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Lx200.Terminator);
      begin
        return Time.JD'value (Reply(Reply'first .. Reply'last - 1));
      end;
    exception
    when others =>
      Disconnect;
      return Unknown_Time;
    end Julian_Date;

  begin -- Handler
    Log.Write ("Handler started");
    loop
      select
        accept Shutdown;
        Disconnect;
        exit;
      or
        when Is_Connected => accept Get (The_Time : out Time.JD) do
          The_Time := Julian_Date;
        end Get;
      or
        when not Is_Connected => delay until Time.In_Future (Loop_Time);
        Connect;
      end select;
    end loop;
    Log.Write ("Handler terminated");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Handler;

end Ten_Micron;
