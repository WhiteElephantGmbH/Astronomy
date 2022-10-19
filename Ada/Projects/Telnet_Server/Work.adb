-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Network.Tcp;
with Unsigned;

package body Work is

  procedure Put_Line (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put_Line;


  procedure Server (Port : String) is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

    Terminator : constant Character := Ascii.Lf;

    Listener_Socket   : Network.Tcp.Listener_Socket;
    The_Client_Socket : Network.Tcp.Socket;
    Client_Address    : Network.Ip_Address;

    procedure Send (Item : String) is
    begin
      Network.Tcp.Send (Item, The_Client_Socket);
    end Send;

  begin -- Server
    Network.Tcp.Create_Socket_For (The_Port     => Network.Port_Number'value(Port),
                                   The_Protocol => Socket_Protocol,
                                   The_Listener => Listener_Socket);
    Main: loop
      Network.Tcp.Accept_Client_From (Listener_Socket,
                                      The_Client_Socket,
                                      Client_Address);
      Put_Line ("# connected. Ip Address " & Network.Image_Of (Client_Address));
      Send ("Hello" & Ascii.Cr & Ascii.Lf & '>');
      begin
        loop
          declare
            Data     : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Terminator);
            Hex_Data : constant String := Unsigned.Hex_Image_Of (Unsigned.String_Of (Data));
          begin
            Put_Line ("< " & Hex_Data);
            Send ("Response" & Ascii.Cr & Ascii.Lf & '>');
            exit Main when Data = Ascii.Cr & Ascii.Lf;
          end;
        end loop;
      exception
      when Network.Tcp.No_Client =>
        Put_Line ("# disconnected");
      end;
    end loop Main;
  end Server;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 1 then
      declare
        Port : constant String := Ada.Command_Line.Argument (1);
      begin
        Put_Line ("Telnet Server on port " & Port);
        Server (Port);
      end;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Work;
