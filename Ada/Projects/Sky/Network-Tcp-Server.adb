-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Ada.Unchecked_Deallocation;
with Definite_Doubly_Linked_Lists;
with Traces;

package body Network.Tcp.Server is

  package Log is new Traces ("Server");


  task type Listener is

    entry Finish;

  end Listener;


  task type Reader is

    entry Start (With_Socket : Network.Tcp.Socket);

    entry Finish;

  end Reader;


  type Reader_Access is access Reader;

  task type Clients is

    entry Add (The_Socket : Network.Tcp.Socket);

    entry Remove (The_Socket : Network.Tcp.Socket);

    entry Close_All;

    entry Finish;

  end Clients;


  function Exception_Reason (Item : Ada.Exceptions.Exception_Occurrence) return String is
  begin
    return " " & Ada.Exceptions.Exception_Name (Item) & " " & Net.Resolve_Exception (Item)'img;
  end Exception_Reason;


  task body Clients is

    procedure Deallocate is new Ada.Unchecked_Deallocation (Reader, Reader_Access);

    type Client is record
      The_Reader : Reader_Access;
      The_Socket : Network.Tcp.Socket;
    end record;

    function "=" (Left, Right : Client) return Boolean is
      use type Net.Socket_Type;
    begin
      return Left.The_Socket.The_Socket = Right.The_Socket.The_Socket;
    end "=";

    package Client_List is new Definite_Doubly_Linked_Lists (Client);

    The_List : Client_List.Item;

    The_Terminating_Client : Client;

    procedure Log_Write (The_Message : String) is
    begin
      Log.Write ("Clients " & The_Message);
    end Log_Write;

    use type Client_List.Item;

    Is_Closing : Boolean := False;

  begin -- Clients
    Log_Write ("started");
    loop
      select
        accept Add (The_Socket : Network.Tcp.Socket) do
          declare
            The_Client : Client;
          begin
            The_Client := (new Reader, The_Socket);
            The_Client.The_Reader.Start (The_Socket);
            The_List := The_List + The_Client;
          end;
        end Add;
      or
        accept Remove (The_Socket : Network.Tcp.Socket) do
          for The_Client of The_List loop
            if The_Client.The_Socket = The_Socket then
              The_Terminating_Client := The_Client;
              exit;
            end if;
          end loop;
        end Remove;
        The_Terminating_Client.The_Reader.Finish;
        while not The_Terminating_Client.The_Reader'terminated loop
          delay 0.01;
        end loop;
        Deallocate (The_Terminating_Client.The_Reader);
        The_List := The_List - The_Terminating_Client;
        exit when Is_Closing and then The_List.Is_Empty;
      or
        accept Close_All;
        exit when The_List.Is_Empty;
        Is_Closing := True;
        for The_Client of The_List loop
          begin
            Network.Tcp.Close (The_Client.The_Socket);
          exception
          when Item: others =>
            Log_Write ("shutdown exception" & Exception_Reason (Item));
          end;
        end loop;
      end select;
    end loop;
    accept Finish;
    Log_Write ("finished");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Clients;


  The_Clients         : access Clients;
  The_Listener        : access Listener;
  The_Listener_Socket : Network.Tcp.Listener_Socket;


  task body Listener is

    The_Socket  : Network.Tcp.Socket;
    The_Address : Network.Ip_Address;

    procedure Log_Write (The_Message : String) is
    begin
      Log.Write ("Listener " & The_Message);
    end Log_Write;

  begin -- Listener
    Log_Write ("started");
    Main: loop
      begin
        Accept_Client_From (The_Listener   => The_Listener_Socket,
                            The_Client     => The_Socket,
                            Client_Address => The_Address);
      exception
      when others =>
        Log_Write ("termination");
        exit Main;
      end;
      Log_Write ("accepted: " & Network.Image_Of (The_Address));
      The_Clients.Add (The_Socket);
    end loop Main;
    accept Finish;
    Log_Write ("finished");
  end Listener;


  The_Handler    : Protocol_Handler;
  The_Terminator : Character;

  task body Reader is

    procedure Log_Write (The_Message : String) is
    begin
      Log.Write ("Reader " & The_Message);
    end Log_Write;

    The_Socket : Network.Tcp.Socket;

  begin -- Reader
    accept Start (With_Socket : Network.Tcp.Socket) do
      The_Socket := With_Socket;
    end Start;
    Log_Write ("started");
    loop
      The_Handler (Raw_String_From (The_Socket, The_Terminator), The_Socket);
    end loop;
  exception
  when others =>
    Log_Write ("termination");
    The_Clients.Remove (The_Socket);
    accept Finish;
    Log_Write ("finished");
  end Reader;


  procedure Start (Handler    : Protocol_Handler;
                   Used_Port  : Port_Number;
                   Terminator : Character) is
  begin
    Log.Write ("start");
    The_Handler := Handler;
    The_Terminator := Terminator;
    Network.Tcp.Create_Socket_For (Used_Port, Raw, The_Listener_Socket);
    The_Clients := new Clients;
    The_Listener := new Listener;
  end Start;


  procedure Reply (Answer : String;
                   To     : Socket) is
  begin
    Log.Write ("reply <" & Answer & ">");
    Send (Answer, To);
  exception
  when others =>
    null;
  end Reply;


  procedure Close is
  begin
    if The_Listener /= null then
      begin
        Network.Tcp.Close (The_Listener_Socket);
      exception
      when others =>
        null;
      end;
      The_Listener.Finish;
      The_Clients.Close_All;
      The_Clients.Finish;
    end if;
  end Close;

end Network.Tcp.Server;
