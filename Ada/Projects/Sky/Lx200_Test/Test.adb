-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Network.Tcp;

package body Test is

  package Io renames Ada.Text_IO;

  Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
  Server_Port     : constant Network.Port_Number := 4030;

  Terminator : constant Character := '#';

  Max_Retries : constant := 3;


  procedure Client (Server : String) is
    The_Socket : Network.Tcp.Socket;
    Nr_Retries : Natural;
  begin
    begin
      Nr_Retries := 0;
      loop
        begin
          The_Socket := Network.Tcp.Socket_For (Network.Ip_Address_Of (Server), Server_Port, Socket_Protocol, 1.0);
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
          Network.Tcp.Send (The_String  => ':' & Command & Terminator,
                            Used_Socket => The_Socket);
          exit when Command = "";
          case Command(Command'first) is
          when 'A' | 'M' | 'Q' | 'R' =>
            if Command in "MS" | "MA" then -- MA -> Timeout
              declare
                Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Terminator);
              begin
                Io.Put_Line (Reply);
              end;
              declare
                Reply : constant String
                  := Network.Tcp.Raw_String_From (The_Socket, Terminator => Terminator, Receive_Timeout => 40.0);
              begin
                Io.Put_Line (Reply);
              end;
            end if;
          when 'S' =>
            case Command(Command'first + 1) is
            when 'g' | 't' =>
              loop
                declare
                  Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Terminator);
                begin
                  Io.Put_Line (Reply);
                  exit when Reply /= "#";
                end;
              end loop;
            when others =>
              declare
                Reply : constant Character := Network.Tcp.Raw_Character_From (The_Socket);
              begin
                Io.Put_Line ("" & Reply);
              end;
            end case;
          when others =>
            declare
              Reply : constant String := Network.Tcp.Raw_String_From (The_Socket, Terminator => Terminator);
            begin
              Io.Put_Line (Reply);
            end;
          end case;
        exception
        when Network.Timeout =>
          Io.Put_Line ("<Timeout>");
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

    procedure Message_Handler (Data : String) is

      procedure Execute (Command : String) is
      begin
        Io.Put_Line ("Execute " & Command);
      end Execute;

      procedure Send (Item :  String) is
      begin
        Network.Tcp.Send (Item, The_Client_Socket);
      end Send;

    begin -- Message_Handler
      if Data'length > 3  and then Data(Data'last) = Terminator then
        declare
          Command : constant String := Data(Data'first .. Data'first + 2);
        begin
          if Command = ":GW" then
            Io.Put_Line ("Get Alignment Status");
            Send ("PT0#");
          elsif Command = ":GV" then
            case Command(Command'first + 3) is
            when 'P' => -- GVP
              Io.Put_Line ("Get Product Name");
              Send ("Avalon#");
            when 'D' => -- GVD
              Io.Put_Line ("Get Firmware Date");
              Send ("d02102017#");
            when 'N' => -- GVN
              Io.Put_Line ("Get Firmware Number");
              Send ("56.3#");
            when others =>
              Io.Put_Line ("Unknown GV Command");
            end case;
          elsif Command = ":AA" then
            Io.Put_Line ("Set AltAz Alignment");
          elsif Command = ":AP" then
            Io.Put_Line ("Set Polar Alignment");
          elsif Command = ":Gg" then
            Io.Put_Line ("Get Longitude");
            Send ("+012*34:18G#");
          elsif Command = ":Gt" then
            Io.Put_Line ("Get Latitude");
            Send ("+41t37:23");
          elsif Command = ":GS" then
            Io.Put_Line ("Get Sideral Time");
            Send ("00:34:56");
          elsif Command = ":GA" then
            Io.Put_Line ("Get Telescope Altitude");
            Send ("+48*22'37");
          elsif Command = ":GD" then
            Io.Put_Line ("Get Telescope DEC");
            Send ("+00*00:00#");
          elsif Command = ":GR" then
            Io.Put_Line ("Get Telescope RA");
            Send ("00:16:25#");
          elsif Command = ":GZ" then
            Io.Put_Line ("Get Azimuth");
            Send ("179*59'59#");
          elsif Command = ":Sr" then -- :Sr02:44:50#
            Io.Put_Line ("Set Object RA");
            Send ("1");
          elsif Command = ":Sd" then -- :Sd+02*55:54#
            Io.Put_Line ("Set Object DEC");
            Send ("1");
          elsif Command = ":Sa" then -- :Sa+20*23'42#
            Io.Put_Line ("Set Object Altitude");
            Send ("1");
          elsif Command = ":Sz" then -- :Sz123*20'30#
            Io.Put_Line ("Set Object Azimuth");
            Send ("1");
          elsif Command = ":SL" then -- :SL09:45:00#
            Io.Put_Line ("Set Local Time");
            Send ("1");
          elsif Command = ":SG" then -- :SG+02.0#
            Io.Put_Line ("Set UTC - Local Time");
            Send ("1");
          elsif Command = ":Sg" then -- :Sg+010g39:42#
            Io.Put_Line ("Set Longitude");
            Send ("0#");
          elsif Command = ":St" then -- :Gt+47*40:35#
            Io.Put_Line ("Set Latitude");
            Send ("0#");
          elsif Command = ":MS" then
            Io.Put_Line ("Slew to Target Object");
            Send ("0#"); -- OK
            Io.Put_Line ("slewing..");
            delay (3.0);
            Send ("ge#"); -- OK
            Io.Put_Line ("tracking");
          elsif Command = ":CM" then
            Io.Put_Line ("Synch");
            Send ("0#"); -- OK
          elsif Command = ":Me" then
            Execute ("Move_Left");
          elsif Command = ":Mn" then
            Execute ("Move_Up");
          elsif Command = ":Ms" then
            Execute ("Move_Down");
          elsif Command = ":Mw" then
            Execute ("Move_Right");
          elsif Command = ":Qe" then
            Execute ("End_Move_Left");
          elsif Command = ":Qn" then
            Execute ("End_Move_Up");
          elsif Command = ":Qs" then
            Execute ("End_Move_Down");
          elsif Command = ":Qw" then
            Execute ("End_Move_Right");
          elsif Command = ":RC" then
            Execute ("Set_Centering_Rate");
          elsif Command = ":RG" then
            Execute ("Set_Guiding_Rate");
          elsif Command = ":RM" then
            Execute ("Set_Finding_Rate");
          elsif Command = ":RS" then
            Execute ("Set_Slewing_Rate");
          elsif Command = ":RC" then
            Execute ("Set_Centering_Rate");
          elsif Command = ":RG" then
            Execute ("Set_Guiding_Rate");
          elsif Command = ":RM" then
            Execute ("Set_Finding_Rate");
          elsif Command = ":RS" then
            Execute ("Set_Slewing_Rate");
          else
            Io.Put_Line ("Unknown Command");
          end if;
        end;
      elsif Data = ":Q#" then
        Execute ("Stop");
      else
        Io.Put_Line ("Unknown");
      end if;
    end Message_Handler;

  begin
    Network.Tcp.Create_Socket_For (The_Port     => Server_Port,
                                   The_Protocol => Socket_Protocol,
                                   The_Listener => Listener_Socket);
    Network.Tcp.Accept_Client_From (Listener_Socket,
                                    The_Client_Socket,
                                    Client_Address);
    Io.Put_Line ("Client connected. Ip Address " & Network.Image_Of (Client_Address));
    begin
      loop
        declare
          Command : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Terminator);
        begin
          exit when Command = ":#";
          Io.Put_Line (Command);
          Message_Handler (Command);
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


  procedure Execute is
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
  end Execute;

end Test;