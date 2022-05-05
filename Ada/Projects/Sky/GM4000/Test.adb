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
    Server_Port     : constant Network.Port_Number := 3490;

    Terminator : constant Character := '#';

    Listener_Socket   : Network.Tcp.Listener_Socket;
    The_Client_Socket : Network.Tcp.Socket;
    Client_Address    : Network.Ip_Address;

    type State is (Parked, Slewing, Tracking, Stopped);

    The_State  : State := Parked;
    Send_Delay : Integer;

    Ra_Park_Image  : constant String := "00:00:00.00#";
    Dec_Park_Image : constant String := "+90:00:00.0#";

    Ra_Image  : String := Ra_Park_Image;
    Dec_Image : String := Dec_Park_Image;

    procedure Message_Handler (Data : String) is

      procedure Send (Item :  String) is
      begin
        if Item'length > 1 and then Item(Item'last) /= '#' then
          raise Program_Error with "Incorrect Send Item: " & Item;
        end if;
        if Hiding then
          Ada.Text_IO.Put ('.');
        else
          Put_Line ("->" & Item);
        end if;
        Network.Tcp.Send (Item, The_Client_Socket);
      end Send;

    begin -- Message_Handler
      if Data'length > 3  and then Data(Data'last) = Terminator then
        declare
          Command : constant String := Data(Data'first .. Data'first + 2);
          Image   : constant String := Data(Data'first + 3 .. Data'last);
        begin
          if Data = ":Gstat#" then
            Put_Line ("Get Status");
            case The_State is
            when Parked =>
              Send ("5#");
            when Stopped =>
              Send ("1#");
            when Slewing =>
              Send ("6#");
              Send_Delay := Send_Delay - 1;
              if Send_Delay = 0 then
                The_State := Tracking;
              end if;
            when Tracking =>
              Send ("0#");
            end case;
          elsif Data = ":STOP#" then
            Put_Line ("Stop");
            The_State := Stopped;
          elsif Command = ":GV" then
            case Data(Data'first + 3) is
            when 'P' => -- GVP
              The_State := Parked;
              Put_Line ("Get Product Name");
              Send ("10micron GM4000HPS#");
            when 'N' => -- GVN
              Put_Line ("Get Firmware Number");
              Send ("2.15.1#");
            when others =>
              Put_Line ("Unknown GV Command");
            end case;
          elsif Command = ":U2" then
            Put_Line ("Set Ultra Precesion Mode");
          elsif Command = ":PO" then
            Put_Line ("Unpark");
            The_State := Tracking;
          elsif Command = ":KA" then
            Put_Line ("Park");
            The_State := Parked;
          elsif Command = ":Gg" then
            Put_Line ("Get Longitude");
            Send ("-008*37:32.0#");
          elsif Command = ":Gt" then
            Put_Line ("Get Latitude");
            Send ("+47*42:22.0");
          elsif Command = ":GS" then
            Put_Line ("Get Sideral Time");
            Send ("07:34:56.42");
          elsif Command = ":GA" then
            Put_Line ("Get Telescope Altitude");
            Send ("+47:42:22.0");
          elsif Command = ":GD" then
            Put_Line ("Get Telescope DEC");
            case The_State is
            when Parked =>
              Dec_Image := Dec_Park_Image;
            when others =>
              null;
            end case;
            Send (Dec_Image);
          elsif Command = ":GR" then
            Put_Line ("Get Telescope RA");
            case The_State is
            when Parked =>
              Ra_Image := Ra_Park_Image;
            when others =>
              null;
            end case;
            Send (Ra_Image);
          elsif Command = ":GZ" then
            Put_Line ("Get Azimuth");
            Send ("179:59:59.00#");
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
          elsif Command = ":MS" then
            The_State := Slewing;
            Put_Line ("Slew to Target Object");
            Send ("0"); -- OK
            Send_Delay := 20;
          else
            Put_Line ("Unknown Command " & Data);
          end if;
        end;
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
      Put_Line ("GM 4000 HPS connected. Ip Address " & Network.Image_Of (Client_Address));
      begin
        loop
          declare
            Command : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Terminator);
          begin
            exit Main when Command = "";
            if Command in ":Gstat#" | ":GD#" | ":GR#" then
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
        Put_Line ("GM 4000 HPS has disconnected");
      end;
    end loop Main;
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
      Put_Line ("GM 4000 Simulator.");
      Server;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Test;
