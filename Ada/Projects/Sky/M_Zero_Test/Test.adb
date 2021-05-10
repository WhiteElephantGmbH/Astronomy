-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Network.Tcp;
with Log;

package body Test is

  package Io renames Ada.Text_IO;


  procedure Server is

    Socket_Protocol : constant Network.Tcp.Protocol := Network.Tcp.Raw;
    Server_Port     : constant Network.Port_Number := 4030;

    Terminator : constant Character := '#';

    Listener_Socket   : Network.Tcp.Listener_Socket;
    The_Client_Socket : Network.Tcp.Socket;
    Client_Address    : Network.Ip_Address;

    type State is (Startup, Initialize, Tracking);

    Is_Polar           : Boolean := True;
    The_State          : State := Startup;
    Send_Ge_Is_Pending : Boolean := False;
    Send_Delay         : Integer;


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
          if Send_Ge_Is_Pending then
            Send_Delay := Send_Delay - 1;
            if Send_Delay = 0 then
              Send ("ge#");
              Io.Put_Line ("tracking");
              Send_Ge_Is_Pending := False;
            end if;
          end if;
          if Command = ":GW" then
            Io.Put_Line ("Get Alignment Status");
            if Is_Polar then
              Send ("PT0#");
            else
              Send ("AT0#");
            end if;
          elsif Command = ":GV" then
            case Data(Data'first + 3) is
            when 'P' => -- GVP
              The_State := Startup;
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
            Is_Polar := False;
            Io.Put_Line ("Set AltAz Alignment");
          elsif Command = ":AP" then
            Is_Polar := True;
            Io.Put_Line ("Set Polar Alignment");
          elsif Command = ":Gg" then
            Io.Put_Line ("Get Longitude");
            Send ("+012*34:18#");
          elsif Command = ":Gt" then
            Io.Put_Line ("Get Latitude");
            Send ("+41*37:23");
          elsif Command = ":GS" then
            Io.Put_Line ("Get Sideral Time");
            Send ("00:34:56");
          elsif Command = ":GA" then
            Io.Put_Line ("Get Telescope Altitude");
            Send ("+48*22'37");
          elsif Command = ":GD" then
            Io.Put_Line ("Get Telescope DEC");
            case The_State is
            when Startup =>
              Send ("+00*00:00#");
            when Initialize =>
              Send ("+90*00:00#");
            when Tracking =>
              Send ("+30*00:00#");
            end case;
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
          elsif Command = ":St" then -- :St+47*40:35#
            Io.Put_Line ("Set Latitude");
            Send ("0#");
          elsif Command = ":MS" then
            The_State := Tracking;
            Io.Put_Line ("Slew to Target Object");
            Send ("0#"); -- OK
            Io.Put_Line ("slewing..");
            Send_Ge_Is_Pending := True;
            Send_Delay := 7;
          elsif Command = ":CM" then
            Io.Put_Line ("Synch");
            Send ("0#"); -- OK
            case The_State is
            when Startup =>
              The_State := Initialize;
            when Initialize =>
              The_State := Tracking;
            when Tracking =>
              null;
            end case;
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
        Io.Put_Line ("Unknown " & Data);
      end if;
    end Message_Handler;

  begin -- Server
    Network.Tcp.Create_Socket_For (The_Port     => Server_Port,
                                   The_Protocol => Socket_Protocol,
                                   The_Listener => Listener_Socket);
    Main: loop
      Network.Tcp.Accept_Client_From (Listener_Socket,
                                      The_Client_Socket,
                                      Client_Address);
      Io.Put_Line ("Client connected. Ip Address " & Network.Image_Of (Client_Address));
      begin
        loop
          declare
            Command : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Terminator);
          begin
            exit Main when Command = "";
            Io.Put_Line (Command);
            Message_Handler (Command);
          end;
        end loop;
      exception
      when Network.Tcp.No_Client =>
        Io.Put_Line ("Client has disconnected");
      end;
    end loop Main;
  exception
  when Item: others =>
    Log.Write (Item);
    Io.Put_Line ("Server Error: " & Network.Net.Resolve_Exception (Item)'img);
  end Server;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Io.Put_Line ("TCP Test program in server mode.");
      Server;
    else
      Io.Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Io.Put_Line ("Work exception = " & Exceptions.Information_Of (Event));
  end Execute;

end Test;
