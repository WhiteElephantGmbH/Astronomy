-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with Network.Tcp;
with Response;
with Unsigned;

package body Simulator is

  procedure Put_Line (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put_Line;


  procedure Server is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
    Server_Port     : constant Network.Port_Number := 80;

    Listener_Socket   : Network.Tcp.Listener_Socket;
    The_Client_Socket : Network.Tcp.Socket;
    Client_Address    : Network.Ip_Address;

  begin -- Server
    Network.Tcp.Create_Socket_For (The_Port     => Server_Port,
                                   The_Protocol => Socket_Protocol,
                                   The_Listener => Listener_Socket);
    loop
      Network.Tcp.Accept_Client_From (Listener_Socket,
                                      The_Client_Socket,
                                      Client_Address);
      Put_Line ("ENC 2302 connected. Ip Address " & Network.Image_Of (Client_Address));
      begin
        declare
          -- command http://192.168.10.160/statusjsn.js?components=513&cmd=1&p.3&s.1
          Command  : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Ascii.Lf);
          Hex_Data : constant String := Unsigned.Hex_Image_Of (Unsigned.String_Of (Command));
        begin
          Put_Line ("<<<Command:>>>" & Command);
          Put_Line ("<<<Hex_Cmd:>>>" & Hex_Data);
          Put_Line ("<<<Reply>>>" & Response.Item);
          Network.Tcp.Send (Response.Item & Ascii.Cr & Ascii.Lf, The_Client_Socket);
        end;
        declare
          Extra    : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Ascii.Lf);
          Hex_Data : constant String := Unsigned.Hex_Image_Of (Unsigned.String_Of (Extra));
        begin
          Put_Line ("<<<Extra:>>>" & Extra);
          Put_Line ("<<<InHex:>>>" & Hex_Data);
          Network.Tcp.Send ("end" & Ascii.Cr & Ascii.Lf, The_Client_Socket);
        end;
      exception
      when Network.Tcp.No_Client =>
        Put_Line ("GM 4000 HPS has disconnected");
      end;
    end loop;
  exception
  when Item: others =>
    Log.Write (Item);
    declare
      Network_Error : constant Network.Exception_Type := Network.Net.Resolve_Exception (Item);
      use all type Network.Exception_Type;
    begin
      if Network_Error /= Cannot_Resolve_Error then
        Put_Line ("Network Error: " & Network_Error'image);
      end if;
      Put_Line (Exceptions.Name_Of (Item));
    end;
  end Server;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Put_Line ("ENC 2302 Simulator.");
      Server;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Simulator;
