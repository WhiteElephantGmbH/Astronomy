-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Astro;
with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;
with Angle;
with Earth;
with Exceptions;
with Log;
with Lx200;
with Network.Tcp;
with Objects;
with Space;
with Site;
with Text;
with Time;

package body Simulator is

  package Real renames Ada.Real_Time;

  Start_Hiding : Boolean := False;
  Hiding       : Boolean := False;

  use type Time.JD;

  type Mount is (GM_1000, GM_4000);

  Mount_Type : Mount := GM_1000;
  Firmware   : Float := 0.0;

  Error : exception;


  procedure Put_Line (Item : String) is
  begin
    pragma Assert (Item'length > 0);
    declare
      Image : constant String := (if Item'length = 1 then Item(Item'first)'image else Item);
    begin
      Log.Write (Image);
      if not Hiding then
        Ada.Text_IO.Put_Line (Image);
      end if;
    end;
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
      Put_Line (".>");
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

    Ra_Park_Position  : constant Angle.Degrees := 0.0;
    Dec_Park_Position : constant Angle.Degrees := 90.0;

    The_Ra  : Angle.Degrees := Ra_Park_Position;
    The_Dec : Angle.Degrees := Dec_Park_Position;

    RA_Park_Axis_Image  : constant String := "+090.0000#";
    Dec_Park_Axis_Image : constant String := "+000.0000#";

    RA_Axis_Image  : String := RA_Park_Axis_Image;
    Dec_Axis_Image : String := Dec_Park_Axis_Image;

    The_Speed_Correction : Character := '0';

    Air_Pressure_Image : String := "1013.0#";
    Temperature_Image  : String := "+010.0#";

    Alignment_Information_2 : constant String := "000.0002,+47.6668,00.0002,322.82,E,+00.00,+00.00,E,E#";
    Alignment_Information_3 : constant String := "000.0002,+47.6668,00.0002,322.82,-00.0001,+00.00,+00.00,E,E#";
    Alignment_Information   : constant String := "000.0002,+47.6668,00.0002,322.82,-00.0001,+00.00,+00.00,02,00000.5#";

    The_Points_Count   : Natural := 0;
    Last_Points_Count : Natural := 0;

    Loaded_TLE_Data      : Text.String;
    Tle_Is_Precalculated : Boolean := False;
    Jd_Start             : Time.JD := 0.0;
    Jd_End               : Time.JD := 0.0;

    type Satellite_State is (Undefined, Preparing, Waiting, Catching, Tracking, Ended);

    The_Satellite_State  : Satellite_State := Undefined;

    type Offset is delta 0.1 range -9999.0 .. 9999.0 with Small => 0.1;

    The_Time_Offset  : Offset := 0.0;

    type Arc_Seconds is new Duration;

    Lunar_Tracking_Increment : constant Arc_Seconds := 0.356; -- per second
    Solar_Tracking_Increment : constant Arc_Seconds := 0.041; -- per second

    The_Dec_Increment : Arc_Seconds := 0.0;
    The_Ra_Increment  : Arc_Seconds := 0.0;

    Undefined_Time : constant Duration := Duration'first;

    The_Dec_Start_Time : Real.Time;
    The_Dec_Time_Delta : Duration := Undefined_Time;
    The_Ra_Start_Time  : Real.Time;
    The_Ra_Time_Delta  : Duration := Undefined_Time;


    function Dec_Value_Of (Image : String) return Angle.Degrees is
      use type Angle.Value;
    begin
      return +Lx200.Signed_Degrees_Of (Image(Image'first .. Image'last - 1));
    end Dec_Value_Of;


    function Ra_Value_Of (Image : String) return Angle.Degrees is
      use type Angle.Value;
    begin
      return +Lx200.Hours_Of (Image(Image'first .. Image'last - 1));
    end Ra_Value_Of;


    function Ra_Image return String is
      use type Angle.Value;
    begin
      return Lx200.Hours_Of (Angle.Value'(+The_Ra));
    end Ra_Image;


    function Dec_Image return String is
      use type Angle.Value;
    begin
      return Lx200.Signed_Degrees_Of (Angle.Value'(+The_Dec));
    end Dec_Image;


    function Sideral_Time return String is
    begin
      return Lx200.Hours_Of (Time.Lmst);
    end Sideral_Time;


    function Traget_Direction return Earth.Direction is
    begin
      return Objects.Direction_Of (Space.Direction_Of (Ra => The_Ra, Dec => The_Dec), Time.Lmst);
    end Traget_Direction;


    function Az_Image return String is
    begin
      return Lx200.Hours_Of (Earth.Az_Of (Traget_Direction));
    end Az_Image;


    function Alt_Image return String is
    begin
      return Lx200.Signed_Degrees_Of (Earth.Alt_Of (Traget_Direction));
    end Alt_Image;


    procedure Increment_Dec is
      The_End_Time  : Real.Time;
      The_Increment : Arc_Seconds;
      use type Angle.Degrees;
      use type Real.Time;
    begin
      if The_Dec_Time_Delta = Undefined_Time then
        The_Dec_Start_Time := Real.Clock;
        The_Dec_Time_Delta := 0.0;
      else
        The_End_Time := Real.Clock;
        The_Dec_Time_Delta := Real.To_Duration (The_End_Time - The_Dec_Start_Time);
        The_Dec_Start_Time := The_End_Time;
        The_Increment := Arc_Seconds(The_Dec_Time_Delta * The_Dec_Increment);
        The_Dec := @ + Angle.Degrees(The_Increment / 3600);
      end if;
    exception
    when Event : others =>
      Ada.Text_IO.Put_Line ("EXCEPTION: " & Exceptions.Information_Of (Event));
    end Increment_Dec;


    procedure Increment_Ra is
      The_End_Time  : Real.Time;
      The_Increment : Arc_Seconds;
      use type Angle.Degrees;
      use type Real.Time;
    begin
      if The_Ra_Time_Delta = Undefined_Time then
        The_Ra_Start_Time := Real.Clock;
        The_Ra_Time_Delta := 0.0;
      else
        The_End_Time := Real.Clock;
        The_Ra_Time_Delta := Real.To_Duration (The_End_Time - The_Ra_Start_Time);
        The_Ra_Start_Time := The_End_Time;
        The_Increment := Arc_Seconds(The_Ra_Time_Delta * The_Ra_Increment);
        The_Ra := @ + Angle.Degrees(The_Increment / 3600);
      end if;
    exception
    when Event : others =>
      Ada.Text_IO.Put_Line ("EXCEPTION: " & Exceptions.Information_Of (Event));
    end Increment_Ra;


    function Increment_Of (Factor_Image : String) return Arc_Seconds is
      The_Factor    : Lx200.Rate_Factor;
      The_Increment : Arc_Seconds;
    begin
      The_Factor := Lx200.Factor_Of (Factor_Image(Factor_Image'first .. Factor_Image'last - 1));
      The_Increment := Arc_Seconds(The_Factor) * Astro.Sideral_Rate;
      return The_Increment;
    exception
    when Event : others =>
      Ada.Text_IO.Put_Line ("EXCEPTION: " & Exceptions.Information_Of (Event));
      return 0.0;
    end Increment_Of;


    function Julian_Date_Image return String is
    begin
      return Text.Trimmed (Time.Julian_Date'image);
    end Julian_Date_Image;


    function Points_Image return String is
      Image : constant String := "00" & Text.Trimmed (The_Points_Count'image);
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
          Put_Line ("->" & Item'image);
        end if;
        Network.Tcp.Send (Item, The_Client_Socket);
      end Send;


      The_Offset : Offset;

      procedure Get_Offset (Image   : String;
                            Maximum : Offset) is

        type Id is range 1 .. 4;

        The_Id : Id;

        Items : constant Text.Strings := Text.Strings_Of (Image, ',', Symbols => "#");

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


      function Actual_State return String is
      begin
        case The_State is
        when Parked =>
          return "5";
        when Stopped =>
          return "1";
        when Slewing =>
          Send_Delay := Send_Delay - 1;
          if Send_Delay = 0 then
            The_State := Tracking;
          end if;
          return "6";
        when Positioning =>
          Send_Delay := Send_Delay - 1;
          if Send_Delay = 0 then
            The_State := Positioned;
          end if;
          return "6";
        when Positioned =>
          return "7";
        when Tracking =>
          return "0";
        when Following =>
          if The_Satellite_State = Ended then
            The_State := Stopped;
          end if;
          return "10";
        end case;
      end Actual_State;


      function Side_Image return String is
        use type Angle.Degrees;
      begin
        if The_Ra > 180.0 then
          return "West";
        else
          return "East";
        end if;
      end Side_Image;


      function Side_Character return Character is
        Image : constant String := Side_Image;
      begin
        return Image(Image'first);
      end Side_Character;

    begin -- Message_Handler
      if Data = ":Gg#" then
        Put_Line ("Get Longitude");
        Send ("-008*37:32.0#");
      elsif Data = ":Gt#" then
        Put_Line ("Get Latitude");
        Send ("+47*42:22.0#");
      elsif Data = ":Gev#" then
        Put_Line ("Get Site Elevation");
        Send ("+0428.0#");
      elsif Data = ":GS#" then
        Put_Line ("Get Sideral Time");
        Send (Sideral_Time & "#");
      elsif Data = ":GA#" then
        Put_Line ("Get Telescope Altitude");
        Send (Alt_Image & "#");
      elsif Data = ":GD#" then
        Put_Line ("Get Telescope DEC");
        case The_State is
        when Parked =>
          The_Dec := Dec_Park_Position;
        when others =>
          Increment_Dec;
        end case;
        Send (Dec_Image & "#");
      elsif Data = ":GR#" then
        Put_Line ("Get Telescope RA");
        case The_State is
        when Parked =>
          The_Ra := Ra_Park_Position;
        when others =>
          Increment_Ra;
        end case;
        Send (Ra_Image & "#");
      elsif Data = ":GZ#" then
        Put_Line ("Get Telescope Azimuth");
        Send (Az_Image & "#");
      elsif Data = ":pS#" then
        Put_Line ("Get_Pointing_State");
        Send (Side_Image & "#");
      elsif Data = ":GVP#" then
        Put_Line ("Get Product Name");
        case Mount_Type is
        when GM_1000 =>
          Send ("10micron GM1000HPS#");
        when GM_4000 =>
          Send ("10micron GM4000HPS#");
        end case;
      elsif Data = ":GVN#" then
        Put_Line ("Get Firmware Number");
        case Mount_Type is
        when GM_1000 =>
          Send ("3.4#");
          Firmware := 3.4;
        when GM_4000 =>
          Send ("2.15.1#");
          Firmware := 2.15;
        end case;
      elsif Data = ":GCFG#" then
        Put_Line ("Get Mount Information");
        if Minimum_Version (3.4) then
          Send ("E,G,N,h#");
        end if;
      elsif Data = ":GETID#" then
        Put_Line ("Get Mount Information");
        if Minimum_Version (3.0) then
          Send ("07206073609258177599#");
        end if;
      elsif Data = ":Ginfo#" then
        Put_Line ("Get Multiple Information");
        Send (Ra_Image & "," & Dec_Image & "," & Side_Character & "," & Az_Image & "," & Alt_Image & "," &
              Julian_Date_Image & "," & Actual_State & ",0#");
      elsif Data = ":Gstat#" then
        Put_Line ("Get Status");
        Send (Actual_State & "#");
      elsif Data = ":D#" then
        Put_Line ("Get Slewing State");
        if The_State = Slewing then
          Send (Ascii.Del & "#");
        else
          Send ("#");
        end if;
      elsif Data = ":Gpgc#" then
        Put_Line ("Get Guiding Status");
        Send ("0#");
      elsif Data = ":h?#" then
        Put_Line ("Query Home Status");
        Send ("0");
      elsif Data = ":GSC#" then
        Put_Line ("Get Speed Correction Flag");
        Send ([The_Speed_Correction]);
      elsif Data = ":SSC0#" then
        Put_Line ("Deactivate Speed Correction");
        The_Speed_Correction := '0';
        Send ("1");
      elsif Data = ":SSC1#" then
        Put_Line ("Activate Speed Correction");
        The_Speed_Correction := '1';
        Send ("1");
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
        Send (Julian_Date_Image & "#");
      elsif Data = ":GJD2#" then
        Put_Line ("Get Julian Date [LeapSecond]");
        Send (Julian_Date_Image & "#");
      elsif Data = ":Ggui#" then
        Put_Line ("Get Guide Rate");
        Send ("7.50#");
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
      elsif Data = ":GMs#" then
        Put_Line ("Get Current Slewing Rate");
        Send ("08#");
      elsif Data = ":GMsb#" then
        Put_Line ("Get Maximum Slewing Rate");
        Send ("15#");
      elsif Data = ":MaX#" then
        The_State := Positioning;
        Put_Line ("Slew to Axis Position");
        Send ("0"); -- OK
        Send_Delay := Delay_Counter;
      elsif Data = ":TLEG#" then
        if Text.Is_Null (Loaded_TLE_Data) then
          Put_Line ("TLE not loaded");
          Send ("E#");
        else
          Put_Line ("Get loaded TLE");
          Send (Loaded_TLE_Data.S & "#");
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
      elsif Data = ":PsX#" then
        Put_Line ("Slew to saved Parking Position (cannot be performed)");
        Send ("3#");
      elsif Data = ":PyX#" then
        Put_Line ("Save Parking Position");
        Send ("0#");
      elsif Data = ":hP#" then
        Put_Line ("Slew to Park Position");
        The_Satellite_State := Undefined;
        The_State := Parked;
        The_Time_Offset := 0.0;
      elsif Data = [Ascii.Ack] then
        Put_Line ("Query Alignment Mounting Mode");
        if The_State = Tracking then
          Send ("P");
        else
          Send ("L");
        end if;
      elsif Data = "#" then
        Put_Line ("Flush Input");
      elsif Data'length > 3  and then Data(Data'last) = Terminator then
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
          if Command_4 = ":SaXa" then
            Put_Line ("Set RA Axis Angle");
            RA_Axis_Image := Image_4;
            Send ("1");
          elsif Command_4 = ":SaXb" then
            Put_Line ("Set Dec Axis Angle");
            Dec_Axis_Image := Image_4;
            Send ("1");
          elsif Command_4 = ":Suaf" then
            if Image_4 = "0#" then
              Put_Line ("Disables Unattented Flip");
            elsif Image_4 = "1#" then
              Put_Line ("Enables Unattented Flip");
            else
              Put_Line ("Invalid Command " & Data);
            end if;
          elsif Command_4 = ":TLEP" then
            Tle_Is_Precalculated := False;
            if Text.Is_Null (Loaded_TLE_Data) then
              Put_Line ("TLE not loaded");
              Send ("E#");
            else
              declare
                Items     : constant Text.Strings := Text.Strings_Of (Image_4, ',', Symbols => "#");
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
                  Send (Text.Trimmed (Jd_Start'image) & ',' & Text.Trimmed (Jd_End'image)  & ",F#");
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
          elsif Command = ":AP" then
            Put_Line ("Start Tracking");
          elsif Command = ":AL" then
            Put_Line ("Stop Tracking");
          elsif Command = ":Sr" then
            Put_Line ("Set Object RA " & Image);
            The_Ra := Ra_Value_Of (Image);
            Send ("1");
          elsif Command = ":Sd" then
            Put_Line ("Set Object DEC " & Image);
            The_Dec := Dec_Value_Of (Image);
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
          elsif Command = ":RT" then
            case Data(Data'first + 3) is
            when '0' => -- RT0
              Put_Line ("Set lunar tracking rate");
              The_Ra_Increment := Lunar_Tracking_Increment;
            when '1' => -- RT1
              Put_Line ("Set solar tracking rate");
              The_Ra_Increment := Solar_Tracking_Increment;
            when '2' => -- RT2
              The_Ra_Increment := 0.0;
              Put_Line ("Set sideral tracking rate");
            when others =>
              Put_Line ("Not supported tracking rate " & Data);
            end case;
          elsif Command = ":RR" then
            The_Ra_Increment := Increment_Of (Image);
            Put_Line ("Set Tracking Rate RA Factor " & Image);
            Send ("1");
          elsif Command = ":RA" then
            Put_Line ("Set Slew Rate Factor " & Image);
          elsif Command = ":RD" then
            The_Dec_Increment := Increment_Of (Image);
            Put_Line ("Set Tracking Rate Dec Factor " & Image);
            Send ("1");
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
      Put_Line (Mount_Type'image & " HPS connected. Ip Address " & Network.Image_Of (Client_Address));
      begin
        loop
          declare
            Command : constant String := Network.Tcp.Raw_String_From (Used_Socket      => The_Client_Socket,
                                                                      Single_Character => Ascii.Ack,
                                                                      Terminator       => Terminator);
            The_Character : Character;
            Is_Available  : Boolean;
          begin
            exit Main when Command = "";
            if Command in ":Gstat#"   | ":TLESCK#" | ":Ginfo#" | ":GS#" | ":Ggui#" | ":h?#"  | ":Gt#"   | ":Gg#"
              | ":Gev#"   | ":GA#"    | ":GZ#"     | ":GD#"    | ":D#"  | ":GR#"   | ":Gpgc#"| ":GJD2#" | ":GaXa#"
              | ":GaXb#"  | ":GRPRS#" | ":GRTMP#"  | ":GJD1#"  | ":pS#" | [Ascii.Ack]
            then
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
            Ada.Text_IO.Get_Immediate (The_Character, Is_Available);
            if Is_Available and The_Character in '1' .. '9' then
              End_Hiding;
              Put_Line ("@ delay " & The_Character & 's');
              Time.Wait (Duration(Character'pos(The_Character) - Character'pos('0')));
            end if;
          end;
        end loop;
      exception
      when Network.Tcp.Aborted =>
        End_Hiding;
        Put_Line ("### GM HPS aborted");
      when Network.Tcp.No_Client =>
        End_Hiding;
        Put_Line ("### GM HPS has disconnected");
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
        Put_Line ("### Network Error: " & Network_Error'image);
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

begin
  Lx200.Set_Ultra_Precision;
  Site.Define ((Location  => Site.Home,
                Latitude  => Angle.Value_Of ("47°42'22"""),
                Longitude => Angle.Value_Of ("8°37'32"""),
                Elevation => 428)); -- in meters
end Simulator;
