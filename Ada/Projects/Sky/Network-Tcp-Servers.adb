-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Traces;

package body Network.Tcp.Servers is

  package Log is new Traces ("Tcp");


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

    entry Send_To_All (The_Message : Outgoing);

    entry Close_All;

    entry Finish;

  end Clients;


  function Exception_Reason (Item : Ada.Exceptions.Exception_Occurrence) return String is
  begin
    return " " & Ada.Exceptions.Exception_Name (Item) & " " & Net.Resolve_Exception (Item)'img;
  end Exception_Reason;


  task body Clients is

    procedure Send is new Send_Message (Outgoing);

    type Client is record
      The_Reader : Reader_Access;
      The_Socket : Network.Tcp.Socket;
    end record;

    function "=" (Left, Right : Client) return Boolean is
      use type Net.Socket_Type;
    begin
      return Left.The_Socket.The_Socket = Right.The_Socket.The_Socket;
    end "=";

    procedure Deallocate is new Ada.Unchecked_Deallocation (Reader, Reader_Access);

    package Clients is new Ada.Containers.Doubly_Linked_Lists (Client);

    The_List : Clients.List;

    The_Terminating_Client : Client;

    procedure Log_Write (The_Message : String) is
    begin
      Log.Write ("Clients " & The_Message);
    end Log_Write;

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
            The_List.Append (The_Client);
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
        declare
          The_Cursor : Clients.Cursor := The_List.Find (The_Terminating_Client);
        begin
          The_List.Delete (The_Cursor);
        end;
        exit when Is_Closing and then The_List.Is_Empty;
      or
        accept Send_To_All (The_Message : Outgoing) do
          for The_Client of The_List loop
            begin
              Send (The_Message, The_Client.The_Socket);
            exception
            when others =>
              null;
            end;
          end loop;
        end Send_To_All;
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
  exception
  when Item: others =>
    Log.Termination (Item);
  end Listener;


  The_Handler : Message_Handler;

  task body Reader is

    procedure Log_Write (The_Message : String) is
    begin
      Log.Write ("Reader " & The_Message);
    end Log_Write;

    The_Socket : Network.Tcp.Socket;

    function Incoming_Message_From is new Message_From (Incoming);

  begin -- Reader
    accept Start (With_Socket : Network.Tcp.Socket) do
      The_Socket := With_Socket;
    end Start;
    Log_Write ("started");
    loop
      The_Handler (Incoming_Message_From (The_Socket));
    end loop;
  exception
  when others =>
    Log_Write ("termination");
    The_Clients.Remove (The_Socket);
    accept Finish;
    Log_Write ("finished");
  end Reader;


  procedure Start (Handler   : Message_Handler;
                   Used_Port : Port_Number) is
  begin
    Log.Write ("start");
    The_Handler := Handler;
    Network.Tcp.Create_Socket_For (Used_Port, The_Protocol, The_Listener_Socket);
    The_Clients := new Clients;
    The_Listener := new Listener;
  end Start;


  procedure Send (The_Message : Outgoing) is
  begin
    if The_Clients /= null then
      The_Clients.Send_To_All (The_Message);
    end if;
  end Send;


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
  exception
  when others =>
    null;
  end Close;

end Network.Tcp.Servers;
