-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with Network.Tcp;
with Strings;

package body Test is

  Start_Hiding : Boolean := False;
  Hiding       : Boolean := False;


  procedure Put_Line (Item : String) is
  begin
    if not Hiding then
      Ada.Text_IO.Put_Line (Item);
    end if;
  end Put_Line;


  procedure End_Hiding is
  begin
    if Hiding then
      Hiding := False;
      Put_Line ("");
    end if;
    Start_Hiding := False;
  end End_Hiding;


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

    Ra_Image  : String := "00:00:00#";
    Dec_Image : String := "+00:00:00#";

    type Alignment is (Not_Aligned, Unused_1, Unused_2, North_Aligned);

    The_Alignment : Alignment := Not_Aligned;

    type Guiding_Kind is (Stopped, Lunar, Solar, Sideral);

    The_Guiding : Guiding_Kind := Sideral;


    procedure Message_Handler (Data : String) is

      procedure Execute (Command : String) is
      begin
        Put_Line ("Execute " & Command);
      end Execute;

      procedure Send (Item :  String) is
      begin
        if Hiding then
          Ada.Text_IO.Put ('.');
        else
          Put_Line ("->" & Item);
        end if;
        Network.Tcp.Send (Item, The_Client_Socket);
      end Send;

      function Image_Of is new Strings.Image_Of (Natural);

    begin -- Message_Handler
      if Data'length > 3  and then Data(Data'last) = Terminator then
        declare
          Command : constant String := Data(Data'first .. Data'first + 2);
          Image   : constant String := Data(Data'first + 3 .. Data'last);
        begin
          if Send_Ge_Is_Pending then
            Send_Delay := Send_Delay - 1;
            if Send_Delay = 0 then
              Send ("ge#");
              Put_Line ("tracking");
              Send_Ge_Is_Pending := False;
            end if;
          end if;
          if Command = ":GW" then
            Put_Line ("Get Alignment Status");
            if Is_Polar then
              Send ("PT0#");
            else
              Send ("AT0#");
            end if;
          elsif Command = ":GV" then
            case Data(Data'first + 3) is
            when 'P' => -- GVP
              The_State := Startup;
              The_Alignment := Not_Aligned;
              Put_Line ("Get Product Name");
              Send ("Avalon#");
            when 'D' => -- GVD
              Put_Line ("Get Firmware Date");
              Send ("d02102017#");
            when 'N' => -- GVN
              Put_Line ("Get Firmware Number");
              Send ("62.0#");
            when others =>
              Put_Line ("Unknown GV Command");
            end case;
          elsif Command =":X3" then
            Put_Line ("Get_Device_Status");
            Send (":Z1" & Image_Of (Alignment'pos(The_Alignment)) & Image_Of (Guiding_Kind'pos(The_Guiding)) & "2#");
          elsif Command = ":AA" then
            Is_Polar := False;
            Put_Line ("Set AltAz Alignment");
          elsif Command = ":AP" then
            Is_Polar := True;
            Put_Line ("Set Polar Alignment");
          elsif Command = ":Gg" then
            Put_Line ("Get Longitude");
            Send ("+012*34:18#");
          elsif Command = ":Gt" then
            Put_Line ("Get Latitude");
            Send ("+41*37:23");
          elsif Command = ":GS" then
            Put_Line ("Get Sideral Time");
            Send ("00:34:56");
          elsif Command = ":GA" then
            Put_Line ("Get Telescope Altitude");
            Send ("+48*22'37");
          elsif Command = ":GD" then
            Put_Line ("Get Telescope DEC");
            case The_State is
            when Startup =>
              Send ("+00*00:00#");
            when Initialize =>
              Send ("+90*00:00#");
            when Tracking =>
              Send (Dec_Image);
            end case;
          elsif Command = ":GR" then
            Put_Line ("Get Telescope RA");
            Send (Ra_Image);
          elsif Command = ":GZ" then
            Put_Line ("Get Azimuth");
            Send ("179*59'59#");
          elsif Command = ":Sr" then
            Ra_Image := Image;
            Put_Line ("Set Object RA " & Image);
            Send ("1");
          elsif Command = ":Sd" then
            Dec_Image := Image;
            Put_Line ("Set Object DEC " & Image);
            Send ("1");
          elsif Command = ":Sa" then -- :Sa+20*23'42#
            Put_Line ("Set Object Altitude");
            Send ("1");
          elsif Command = ":Sz" then -- :Sz123*20'30#
            Put_Line ("Set Object Azimuth");
            Send ("1");
          elsif Command = ":SL" then -- :SL09:45:00#
            Put_Line ("Set Local Time");
            Send ("1");
          elsif Command = ":SG" then -- :SG+02.0#
            Put_Line ("Set UTC - Local Time");
            Send ("1");
          elsif Command = ":Sg" then -- :Sg+010g39:42#
            Put_Line ("Set Longitude");
            Send ("0#");
          elsif Command = ":St" then -- :St+47*40:35#
            Put_Line ("Set Latitude");
            Send ("0#");
          elsif Command = ":MS" then
            The_State := Tracking;
            Put_Line ("Slew to Target Object");
            Send ("0#"); -- OK
            Put_Line ("slewing..");
            Send_Ge_Is_Pending := True;
            Send_Delay := 20;
          elsif Command = ":CM" then
            Put_Line ("Synch");
            Send ("0#"); -- OK
            case The_State is
            when Startup =>
              The_State := Initialize;
              The_Alignment := North_Aligned;
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
          elsif Command =":TL" then
            Execute ("Set_Lunar_Guiding");
            The_Guiding := Lunar;
          elsif Command =":TS" then
            Execute ("Set_Solar_Guiding");
            The_Guiding := Solar;
          elsif Command =":TQ" then
            Execute ("Set_Sideral_Guiding");
            The_Guiding := Sideral;
          elsif Command =":X1" then
            Execute ("Set_Stop_Guiding");
            The_Guiding := Stopped;
          else
            Put_Line ("Unknown Command");
          end if;
        end;
      elsif Data = ":Q#" then
        Execute ("Stop");
      else
        Put_Line ("Unknown " & Data);
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
      Put_Line ("M-Zero connected. Ip Address " & Network.Image_Of (Client_Address));
      begin
        loop
          declare
            Command : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Terminator);
          begin
            exit Main when Command = "";
            if Command in ":GW#" | ":GD#" | ":GR#" then
              if not Hiding then
                Start_Hiding := True;
              end if;
            else
              End_Hiding;
            end if;
            Put_Line (Command);
            Message_Handler (Command);
            if Start_Hiding then
              Hiding := True;
            end if;
          end;
        end loop;
      exception
      when Network.Tcp.No_Client =>
        End_Hiding;
        Put_Line ("M-Zero has disconnected");
      end;
    end loop Main;
  exception
  when Item: others =>
    Log.Write (Item);
    Put_Line ("Error: " & Network.Net.Resolve_Exception (Item)'img);
  end Server;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Put_Line ("M-Zero Simulator.");
      Server;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Test;
