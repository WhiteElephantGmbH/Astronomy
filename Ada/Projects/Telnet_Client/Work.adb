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

    The_Socket : Network.Tcp.Socket;

    procedure Send (Item : String) is
    begin
      Network.Tcp.Send (Item, The_Socket);
    end Send;

    procedure Receive is
    begin
      loop
        declare
          Data : constant Character := Network.Tcp.Raw_Character_From (The_Socket,
                                                                       Receive_Timeout => 0.3);
        begin
          Ada.Text_IO.Put_Line ("<" & Unsigned.Hex_Image_Of (Unsigned.Byte(Character'pos(Data))) & ">");
        end;
      end loop;
    exception
    when Network.Timeout =>
      null;
    end Receive;

  begin -- Client
    The_Socket := Network.Tcp.Socket_For (Ip_Addrees, Network.Port_Number'value(Port), Socket_Protocol);
    loop
      Receive;
      declare
        Data : constant String := Ada.Text_IO.Get_Line;
      begin
        exit when Data = "";
        if Data(Data'first) = '/' then
          Send ([Character'val(Unsigned.Byte'(Unsigned.Hex_Value_Of (Data(Data'first + 1 .. Data'first + 2))))]);
        else
          Send (Data);
        end if;
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
