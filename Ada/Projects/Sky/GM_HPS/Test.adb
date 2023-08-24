-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with Network.Tcp;
with Strings;
with Time;

package body Test is

  Start_Hiding : Boolean := False;
  Hiding       : Boolean := False;
  Log_All      : Boolean := False;

  use type Strings.Element;
  use type Time.JD;

  type Mount is (GM_1000, GM_4000);

  Mount_Type : Mount := GM_1000;
  Firmware   : Float := 0.0;

  Error : exception;


  procedure Put_Line (Item : String) is
  begin
    if not Hiding then
      Ada.Text_IO.Put_Line (Item);
    end if;
  end Put_Line;


  function Minimum_Version (Version : Float) return Boolean is
  begin
    if Firmware >= Version then
      return True;
    else
      Put_Line ("Command not supported");
      return False;
    end if;
  end Minimum_Version;


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

    Delay_Counter : constant := 10;

    None : constant String := "    ";

    Listener_Socket   : Network.Tcp.Listener_Socket;
    The_Client_Socket : Network.Tcp.Socket;
    Client_Address    : Network.Ip_Address;

    type State is (Parked, Slewing, Tracking, Following, Stopped, Positioning, Positioned);

    The_State  : State := Parked;
    Send_Delay : Integer;

    Ra_Park_Image  : constant String := "00:00:00.00#";
    Dec_Park_Image : constant String := "+90:00:00.0#";

    Ra_Image  : String := Ra_Park_Image;
    Dec_Image : String := Dec_Park_Image;

    RA_Park_Axis_Image  : constant String := "+090.0000#";
    Dec_Park_Axis_Image : constant String := "+000.0000#";

    RA_Axis_Image  : String := RA_Park_Axis_Image;
    Dec_Axis_Image : String := Dec_Park_Axis_Image;

    Air_Pressure_Image : String := "1013.0#";
    Temperature_Image  : String := "+010.0#";

    Alignment_Information_2 : constant String := "000.0002,+47.6668,00.0002,322.82,E,+00.00,+00.00,E,E#";
    Alignment_Information_3 : constant String := "000.0002,+47.6668,00.0002,322.82,-00.0001,+00.00,+00.00,E,E#";
    Alignment_Information   : constant String := "000.0002,+47.6668,00.0002,322.82,-00.0001,+00.00,+00.00,02,00000.5#";

    The_Points_Count   : Natural := 0;
    Last_Points_Count : Natural := 0;

    Loaded_TLE_Data      : Strings.Element;
    Tle_Is_Precalculated : Boolean := False;
    Jd_Start             : Time.JD := 0.0;
    Jd_End               : Time.JD := 0.0;

    type Satellite_State is (Undefined, Preparing, Waiting, Catching, Tracking, Ended);

    The_Satellite_State  : Satellite_State := Undefined;

    type Offset is delta 0.1 range -9999.0 .. 9999.0 with Small => 0.1;

    The_Time_Offset : Offset := 0.0;


    function Julian_Date_Image return String is
    begin
      return Strings.Trimmed (Time.Julian_Date'image) & '#';
    end Julian_Date_Image;


    function Points_Image return String is
      Image : constant String := "00" & Strings.Trimmed (The_Points_Count'image);
    begin
      return Image(Image'last - 2 .. Image'last);
    end Points_Image;


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

      procedure Flush_Input is
      begin
        Put_Line ("Flush Input");
        declare
          Flushed : constant String := Network.Tcp.Raw_String_From (Used_Socket     => The_Client_Socket,
                                                                    Terminator      => Terminator,
                                                                    Receive_Timeout => 0.1);
        begin
          Log.Write ("  - Flushed: " & Flushed);
        end;
      exception
      when others =>
        null;
      end Flush_Input;


      The_Offset : Offset;

      procedure Get_Offset (Image   : String;
                            Maximum : Offset) is

        type Id is range 1 .. 4;

        The_Id : Id;

        Items : constant Strings.Item := Strings.Item_Of (Image, ',', Symbols => "#");

      begin -- Get_Offset
        if The_State = Following then
          The_Id := Id'value(Items(1));
          if The_Id /= 4 then
            Put_Line ("offset id" & The_Id'image & " not implemented");
            raise Error;
          end if;
          The_Offset := Offset'value(Items(2));
          if abs The_Offset > Maximum then
            raise Error;
          end if;
          Send ("V#");
        else
          Put_Line ("not following");
          raise Error;
        end if;
      end Get_Offset;

    begin -- Message_Handler
      if Data'length > 3  and then Data(Data'last) = Terminator then
        declare
          Command   : constant String := Data(Data'first .. Data'first + 2);
          Image     : constant String := Data(Data'first + 3 .. Data'last);
          Command_3 : constant String := (if Data'length > 4 then Data(Data'first .. Data'first + 3) else None);
          Image_3   : constant String := (if Data'length > 4 then Data(Data'first + 4 .. Data'last - 1) else "");
          Command_4 : constant String := (if Data'length > 5 then Data(Data'first .. Data'first + 4) else None);
          Image_4   : constant String := (if Data'length > 5 then Data(Data'first + 5 .. Data'last) else "");
          Command_5 : constant String := (if Data'length > 6 then Data(Data'first .. Data'first + 5) else None);
          Image_5   : constant String := (if Data'length > 6 then Data(Data'first + 6 .. Data'last) else "");
          Command_7 : constant String := (if Data'length > 8 then Data(Data'first .. Data'first + 7) else None);
          Command_8 : constant String := (if Data'length > 9 then Data(Data'first .. Data'first + 8) else None);
          Image_8   : constant String := (if Data'length > 9 then Data(Data'first + 9 .. Data'last) else "");
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
            when Positioning =>
              Send ("6#");
              Send_Delay := Send_Delay - 1;
              if Send_Delay = 0 then
                The_State := Positioned;
              end if;
            when Positioned =>
              Send ("7#");
            when Tracking =>
              Send ("0#");
            when Following =>
              Send ("10#");
              if The_Satellite_State = Ended then
                The_State := Stopped;
              end if;
            end case;
          elsif Data = ":STOP#" then
            Put_Line ("Stop");
            The_State := Stopped;
            The_Satellite_State := Undefined;
            The_Time_Offset := 0.0;
          elsif Data = ":GaXa#" then
            Put_Line ("Get RA Axis Angle");
            case The_State is
            when Parked =>
              RA_Axis_Image := RA_Park_Axis_Image;
            when others =>
              null;
            end case;
            Send (RA_Axis_Image);
          elsif Data = ":GaXb#" then
            Put_Line ("Get Dec Axis Angle");
            case The_State is
            when Parked =>
              Dec_Axis_Image := Dec_Park_Axis_Image;
            when others =>
              null;
            end case;
            Send (Dec_Axis_Image);
          elsif Data = ":GRPRS#" then
            Put_Line ("Get Air Pressure");
            Send (Air_Pressure_Image);
          elsif Data = ":GRTMP#" then
            Put_Line ("Get Temperature");
            Send (Temperature_Image);
          elsif Data = ":GJD1#" then
            Put_Line ("Get Julian Date");
            Send (Julian_Date_Image);
          elsif Data = ":gtg#" then
            Put_Line ("Gps Test Synchronized");
            case Mount_Type is
            when GM_1000 =>
              Send ("0#");
            when GM_4000 =>
              Send ("1#");
            end case;
          elsif Data = ":newalig#" then
            The_Points_Count := 0;
            Put_Line ("Start Alignment");
            Send ("V#");
          elsif Data = ":endalig#" then
            Put_Line ("End Alignment");
            Last_Points_Count := The_Points_Count;
            if The_Points_Count > 1 then
              The_Points_Count := 0;
              Send ("V#");
            else
              Send ("E#");
            end if;
          elsif Data = ":getain#" then
            Put_Line ("Get Alignment Information");
            case Last_Points_Count is
            when 0 | 1 =>
              Send ("E#");
            when 2 =>
              Send (Alignment_Information_2);
            when 3 =>
              Send (Alignment_Information_3);
            when others =>
              Send (Alignment_Information);
            end case;
          elsif Data = ":MaX#" then
            The_State := Positioning;
            Put_Line ("Slew to Axis Position");
            Send ("0"); -- OK
            Send_Delay := Delay_Counter;
          elsif Data = ":TLEG#" then
            if Strings.Is_Null (Loaded_TLE_Data) then
              Put_Line ("TLE not loaded");
              Send ("E#");
            else
              Put_Line ("Get loaded TLE");
              Send (+Loaded_TLE_Data & "#");
            end if;
          elsif Data = ":TLES#" then
            if Tle_Is_Precalculated then
              case The_State is
              when Parked =>
                Put_Line ("Telecope is parked");
                Send ("F#");
              when others =>
                The_State := Following;
                if Jd_End < Time.Julian_Date then
                  Send ("Q#");
                  The_Satellite_State := Undefined;
                  The_Time_Offset := 0.0;
                elsif Jd_Start < Time.Julian_Date then
                  Send ("S#");
                  The_Satellite_State := Catching;
                else
                  Send ("V#");
                  The_Satellite_State := Preparing;
                end if;
              end case;
            else
              Put_Line ("TLE not precalculated");
              Send ("E#");
            end if;
          elsif Data = ":TLESCK#" then
            Put_Line ("Satellite State: " & The_Satellite_State'image);
            case The_Satellite_State is
            when Undefined =>
              Send ("E#");
            when others =>
              if Jd_End < Time.Julian_Date then
                Send ("Q#");
                The_Satellite_State := Ended;
              elsif Jd_Start < Time.Julian_Date then
                Send ("T#");
                The_Satellite_State := Tracking;
                The_State := Following;
              else
                Send ("P#");
                The_Satellite_State := Waiting;
                The_State := Following;
              end if;
            end case;
          elsif Command_4 = ":SaXa" then
            Put_Line ("Set RA Axis Angle");
            RA_Axis_Image := Image_4;
            Send ("1");
          elsif Command_4 = ":SaXb" then
            Put_Line ("Set Dec Axis Angle");
            Dec_Axis_Image := Image_4;
            Send ("1");
          elsif Command_4 = ":TLEP" then
            Tle_Is_Precalculated := False;
            if Strings.Is_Null (Loaded_TLE_Data) then
              Put_Line ("TLE not loaded");
              Send ("E#");
            else
              declare
                Items     : constant Strings.Item := Strings.Item_Of (Image_4, ',', Symbols => "#");
                Jd_Image  : constant String := Items(1);
                Min_Image : constant String := Items(2);
                Jd        : constant Time.JD := Time.JD'value(Jd_Image);
              begin
                Jd_Start := Jd + 0.0005;
                if Jd_Start < Time.Julian_Date then
                  Put_Line ("Start time in the past");
                  Send ("N#");
                else
                  Jd_End := Jd_Start + 0.0005;
                  Put_Line ("TLE Precalculation within next " & Min_Image & " minutes");
                  Send (Strings.Trimmed (Jd_Start'image) & ',' & Strings.Trimmed (Jd_End'image)  & ",F#");
                  Tle_Is_Precalculated := True;
                end if;
              end;
            end if;
          elsif Command_5 = ":SRPRS" then
            Put_Line ("Set Air Pressure: " & Image_5);
            Air_Pressure_Image := Image_5;
            Send ("1");
          elsif Command_5 = ":TLEL0" then
            declare
              Tle_Data : constant String := Image_5;
            begin
              Loaded_TLE_Data := [Tle_Data(Tle_Data'first .. Tle_Data'last - 1)];
            end;
            Put_Line ("TLE data loaded");
            Send ("V#");
          elsif Command_5 = ":TLEDL" then
            begin
              declare
                Item   : constant String := Image_5;
                Number : constant Positive := Positive'value(Item(Item'first .. Item'last - 1));
              begin
                Put_Line ("No satellite loded at position " & Number'image);
              end;
            exception
            when others =>
              Put_Line ("Invalid TLE Command");
            end;
            Send ("E#");
          elsif Command_5 = ":SRTMP" then
            Put_Line ("Set Temperature: " & Image_5);
            Temperature_Image := Image_5;
            Send ("1");
          elsif Command_7 = ":newalpt" then
            The_Points_Count := @ + 1;
            Put_Line ("New Alignment Point");
            Send (Points_Image & "#");
          elsif Command_8 = ":TROFFADD" then
            if Minimum_Version (3.0) then
              begin
                Put_Line ("Add offset whilst following");
                Get_Offset (Image_8, Maximum => 1000.0);
                Put_Line ("Offset =" & The_Offset'image);
                begin
                  The_Time_Offset := @ + The_Offset;
                exception
                when others =>
                  Put_Line ("Time Offset out of range");
                  raise Error;
                end;
                Put_Line ("Time Offset =" & The_Time_Offset'image);
              exception
              when Error =>
                Send ("E#");
              end;
            end if;
          elsif Command_3 = ":SJD" then -- :SJD1234567.12345678#
            Put_Line ("Set Julian Date " & Time.Image_Of (Time.Ut_Of (Time.JD'value(Image_3))));
            Send ("1");
          elsif Command = ":GV" then
            case Data(Data'first + 3) is
            when 'P' => -- GVP
              The_State := Parked;
              Put_Line ("Get Product Name");
              case Mount_Type is
              when GM_1000 =>
                Send ("10micron GM1000HPS#");
              when GM_4000 =>
                Send ("10micron GM4000HPS#");
              end case;
            when 'N' => -- GVN
              Put_Line ("Get Firmware Number");
              case Mount_Type is
              when GM_1000 =>
                Send ("3.1.10#");
                Firmware := 3.1;
              when GM_4000 =>
                Send ("2.15.1#");
                Firmware := 2.15;
              end case;
            when others =>
              Put_Line ("Unknown GV Command");
            end case;
          elsif Command = ":U2" then
            Put_Line ("Set Ultra Precesion Mode");
          elsif Command = ":PO" then
            Put_Line ("Unpark");
            The_State := Tracking;
            The_Time_Offset := 0.0;
          elsif Command = ":KA" then
            Put_Line ("Park");
            The_Satellite_State := Undefined;
            The_State := Parked;
            The_Time_Offset := 0.0;
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
          elsif Command = ":pS" then
            declare
              Ra   : constant Natural := Natural'value (Ra_Image(Ra_Image'first .. Ra_Image'first + 1));
              Side : constant String := (if Ra > 12 then "West#" else "East#");
            begin
              Put_Line ("Get_Pointing_State");
              Send (Side);
            end;
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
          elsif Command = ":SG" then -- :SG-01.0#
            Put_Line ("Set Time Offset");
            Send ("1");
          elsif Command = ":MS" then
            The_State := Slewing;
            Put_Line ("Slew to Target Object");
            Send ("0"); -- OK
            Send_Delay := Delay_Counter;
            The_Time_Offset := 0.0;
          elsif Command = ":CM" then
            Put_Line ("Synchronize to Target Object");
            Send ("Coordinates     matched        #"); -- OK
          elsif Command = ":Mn" then
            Put_Line ("Move North");
          elsif Command = ":Ms" then
            Put_Line ("Move South");
          elsif Command = ":Me" then
            Put_Line ("Move East");
          elsif Command = ":Mw" then
            Put_Line ("Move West");
          elsif Command = ":Qn" then
            Put_Line ("Quit move North");
          elsif Command = ":Qs" then
            Put_Line ("Quit move South");
          elsif Command = ":Qe" then
            Put_Line ("Quit move East");
          elsif Command = ":Qw" then
            Put_Line ("Quit move West");
          elsif Command = ":RG" then
            Put_Line ("Set slew rate to Guiding");
          elsif Command = ":RC" then
            Put_Line ("Set slew rate to Centering");
          elsif Command = ":RM" then
            Put_Line ("Set slew rate to Finding");
          elsif Command = ":RS" then
            Put_Line ("Set slew rate to maximum");
          elsif Command = ":Rc" then
            Put_Line ("Set centering rate to " & Image(Image'first .. Image'last - 1) & "x");
          else
            Put_Line ("Unknown Command " & Data);
          end if;
        end;
      elsif Data = "#" then
        Flush_Input;
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
      Put_Line (Mount_Type'image & " HPS connected. Ip Address " & Network.Image_Of (Client_Address));
      begin
        loop
          declare
            Command : constant String := Network.Tcp.Raw_String_From (The_Client_Socket, Terminator => Terminator);
            The_Character : Character;
            Is_Available  : Boolean;
          begin
            exit Main when Command = "";
            if Command in ":Gstat#" | ":TLESCK#"
              | ":GD#" | ":GR#" | ":GaXa#" | ":GaXb#" | ":GRPRS#" | ":GRTMP#" | ":GJD1#" | ":pS#"
            then
              if not Hiding and not Log_All then
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
            Ada.Text_IO.Get_Immediate (The_Character, Is_Available);
            if Is_Available and The_Character in '1' | '9' then
              delay Duration(Character'pos(The_Character) - Character'pos('0'));
              Put_Line (" Delay " & The_Character & 's');
              Log_All := True;
            end if;
          end;
        end loop;
      exception
      when Network.Tcp.No_Client =>
        End_Hiding;
        Put_Line ("GM HPS has disconnected");
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
    if Nr_Of_Arguments > 1 then
      Put_Line ("Incorrect number of parameters");
      return;
    elsif Nr_Of_Arguments = 1 then
      begin
        Mount_Type := Mount'value ("GM_" & Ada.Command_Line.Argument(1));
      exception
      when others =>
        Put_Line ("Expected parameter: 1000 or 4000");
        return;
      end;
    end if;
    Put_Line (Mount_Type'image & " HPS Simulator.");
    Server;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Test;
