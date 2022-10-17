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


  procedure Client (Ip_Addrees : String;
                    Port       : String) is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;

    Terminator : constant Character := Ascii.Lf;

    The_Socket : Network.Tcp.Socket;

    procedure Send (Item : String) is
    begin
      Network.Tcp.Send (Item, The_Socket);
    end Send;

    procedure Receive is
    begin
      declare
        Data : constant String := Network.Tcp.Raw_String_From (The_Socket,
                                                               Terminator      => Terminator,
                                                               Receive_Timeout => 3.0);
      begin
        Ada.Text_IO.Put_Line ("< " & Unsigned.Hex_Image_Of (Unsigned.String_Of (Data)));
      end;
    exception
    when Network.Timeout =>
      Ada.Text_IO.Put_Line ("< TIMEOUT");
    end Receive;

  begin -- Client
    The_Socket := Network.Tcp.Socket_For (Ip_Addrees, Network.Port_Number'value(Port), Socket_Protocol);
    Receive;
    loop
      Ada.Text_IO.Put (">");
      declare
        Data : constant String := Ada.Text_IO.Get_Line;
      begin
        Send (Data & Ascii.Cr & Ascii.Lf);
        Receive;
        exit when Data = "";
      end;
    end loop;
  end Client;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 2 then
      declare
        Ip_Address : constant String := Ada.Command_Line.Argument (1);
        Port       : constant String := Ada.Command_Line.Argument (2);
      begin
        Ada.Text_IO.Put_Line ("Telnet Client");
        Client (Ip_Address, Port);
      end;
    else
      Ada.Text_IO.Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Ada.Text_IO.Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Work;
