-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Soudronic;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Network.Tcp;

package body Tcp is

  package Io renames Ada.Text_IO;

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
  Server_Port     : constant Network.Port_Number := 4030;

  Max_Retries : constant := 3;


  procedure Client (Server : String) is
    The_Socket : Network.Tcp.Socket;
    Nr_Retries : Natural;
  begin
    begin
      Nr_Retries := 0;
      loop
        begin
          The_Socket := Network.Tcp.Socket_For (Network.Ip_Address_Of (Server), Server_Port, Socket_Protocol, 20.0);
          exit;
        exception
        when Item: others =>
          Io.Put_Line ("Client Error: " & Network.Net.Resolve_Exception (Item)'img);
          Io.Put_Line (Exceptions.Information_Of (Item));
        end;
        Nr_Retries := Nr_Retries + 1;
        if Nr_Retries > Max_Retries then
          return;
        end if;
      end loop;
      Io.Put_Line (">>> connected to " & Server);
      loop
        Ada.Text_IO.Put (">");
        declare
          Command : constant String := Io.Get_Line;
        begin
          Network.Tcp.Send (The_String  => Command,
                            Used_Socket => The_Socket);
          exit when Command = "";
          declare
            Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => '#');
          begin
            Io.Put_Line (Reply);
          end;
        end;
      end loop;
      Network.Tcp.Close (The_Socket);
    exception
    when Network.Not_Found
      |  Network.Host_Error
      |  Network.Tcp.No_Client =>
      Io.Put_Line ("!!! Server not responding");
      Network.Tcp.Close (The_Socket);
    end;
  exception
  when Occurrence: others =>
    Io.Put_Line ("Client Error: " & Network.Net.Resolve_Exception (Occurrence)'img);
    Io.Put_Line ("Traceback: " & Exceptions.Information_Of (Occurrence));
  end Client;


  procedure Server is
    Listener_Socket   : Network.Tcp.Listener_Socket;
    The_Client_Socket : Network.Tcp.Socket;
    Client_Address    : Network.Ip_Address;
  begin
    Network.Tcp.Create_Socket_For (The_Port     => Server_Port,
                                   The_Protocol => Socket_Protocol,
                                   The_Listener => Listener_Socket);
    Network.Tcp.Accept_Client_From (Listener_Socket,
                                    The_Client_Socket,
                                    Client_Address);
    Io.Put_Line ("Client connected. Ip Address =" & Network.Image_Of (Client_Address));
    begin
      loop
        declare
          Command : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => '#');
        begin
          exit when Command = "";
          Io.Put_Line (Command);
          Network.Tcp.Send (':' & Command, The_Client_Socket);
        end;
      end loop;
    exception
    when Network.Tcp.No_Client =>
      Io.Put_Line ("Client has disconnected");
    end;
  exception
  when Item: others =>
    Io.Put_Line ("Server Error: " & Network.Net.Resolve_Exception (Item)'img);
  end Server;


  procedure Work is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Io.Put_Line ("TCP Test program in server mode.");
      Server;
    elsif Nr_Of_Arguments > 1 then
      Io.Put_Line ("Incorrect number of parameters");
    else
      Client (Ada.Command_Line.Argument(1));
    end if;
  exception
  when Event : others =>
    Io.Put_Line ("Work exception = " & Exceptions.Information_Of (Event));
  end Work;

end Tcp;
