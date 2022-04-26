-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Network.Tcp;

package body Test is

  procedure Put_Line (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put_Line;


  procedure Client is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
    Receive_Timeout : constant Duration := 1.0;

    The_Socket : Network.Tcp.Socket;

  begin -- Client
    begin
      The_Socket := Network.Tcp.Socket_For (The_Address     => Network.Ip_Address_Of ("192.168.178.99"),
                                            The_Port        => Network.Port_Number (3490),
                                            The_Protocol    => Socket_Protocol,
                                            Receive_Timeout => Receive_Timeout);
    exception
    when Item: others =>
      Put_Line ("!!! no connection <" & Network.Exception_Kind (Item)'image & '>');
      return;
    end;
    Main: loop
      Ada.Text_IO.Put ('>');
      declare
        Command  : constant String := Ada.Text_IO.Get_Line;
        type Reply_Kind is (Normal, Single, None);
        The_Kind  : Reply_Kind := Normal;
        The_First : Natural := Command'first + 1;
      begin
        if Command = "" then
          exit Main;
        elsif Command = "a" then
          The_Kind := Single;
          Network.Tcp.Send (The_String  => "" & Ascii.Ack,
                            Used_Socket => The_Socket);
        else
          begin
            case Command (Command'first) is
            when ':' =>
              The_First := Command'first;
            when 's' =>
              The_Kind := Single;
            when 'n' =>
              The_Kind := None;
            when others =>
              Put_Line ("!!! command must start with ':' (normal), 'a' (ack), 's' (single response) or 'n' (no reply)");
              exit;
            end case;
            Network.Tcp.Send (The_String  => Command(The_First .. Command'last),
                              Used_Socket => The_Socket);
          exception
          when Item: others =>
            Put_Line ("!!! no anwer <" & Network.Exception_Kind (Item)'image & '>');
            exit Main;
          end;
        end if;
        begin
          case The_Kind is
          when None =>
            Put_Line ("<done>");
          when Normal =>
            declare
              Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => '#');
            begin
              Put_Line ("reply = <" & Reply & ">");
            end;
          when Single =>
            declare
              Reply : constant Character := Network.Tcp.Raw_Character_From (The_Socket);
            begin
              Put_Line ("reply = <" & Reply & ">");
            end;
          end case;
        exception
        when Network.Timeout =>
          Put_Line ("!!! reply timeout");
        when Item: others =>
          Put_Line ("!!! reply error <" &Network.Exception_Kind (Item)'image & '>');
          exit Main;
        end;
      end;
    end loop Main;
    Network.Tcp.Close (The_Socket);
  end Client;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Put_Line ("10micron");
      Client;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Test;
