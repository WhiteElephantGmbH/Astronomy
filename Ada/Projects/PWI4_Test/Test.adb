-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with PWI4.Mount;
with PWI4.M3;
with PWI4.Focuser;
with PWI4.Rotator;
with Serial_Io.Usb;
with Strings;

package body Test is

  procedure Put (Image : String) is
  begin
    Log.Write (Image);
    Ada.Text_IO.Put_Line (Image);
  end Put;


  procedure Error (Message : String) is
  begin
    Put ("*** " & Message & " ***");
  end Error;


  procedure Input_Error (Message : String) is
  begin
    Error (Message);
    Put ("allowed:");
    Put ("  Input [{Com1 .. Com99}]");
    Put ("  Move1={0..1000000}");
    Put ("  Move2={0..1000000}");
    Put ("  Connect");
    Put ("  Enable");
    Put ("  Home");
    Put ("  SetModel");
    Put ("  ConnectRotator1");
    Put ("  ConnectRotator2");
    Put ("  DisconnectRotator1");
    Put ("  DisconnectRotator2");
    Put ("  HomeRotator1");
    Put ("  HomeRotator2");
    Put ("  StartRotator");
    Put ("  TurnTo1");
    Put ("  TurnTo2");
    Put ("  Startup");
    Put ("  MoveM13");
    Put ("  MoveM110");
    Put ("  Stop");
    Put ("  Shutdown");
  end Input_Error;


  procedure Serial_Input (Port_Image : String) is

    Incorrect_Port : exception;
    Port_Not_Found : exception;

    function Detected_Port return Serial_Io.Port is
      Vendor_Id  : constant Serial_Io.Usb.Vendor_Id := 3368;
      Product_Id : constant Serial_Io.Usb.Product_Id := 516;
    begin
      if Port_Image = "" then
        declare
          Ports : constant Serial_Io.Usb.Ports := Serial_Io.Usb.Ports_For (Vid => Vendor_Id, Pid => Product_Id);
        begin
          if Ports'length = 1 then
            return Ports(Ports'first);
          else
            for The_Port of Ports loop
              Put ("found port " & The_Port'img);
            end loop;
            raise Port_Not_Found;
          end if;
        end;
      else
        begin
          return Serial_Io.Port'value(Port_Image);
        exception
        when others =>
          raise Incorrect_Port;
        end;
      end if;
    end Detected_Port;

    task type Reader is
      entry Start;
    end Reader;

    The_Reader : access Reader;
    The_Port   : Serial_Io.Port;

    task body Reader is
      Channel       : Serial_Io.Channel(The_Port);
      The_Character : Character;
      The_Version   : String := "x.xx";
    begin
      accept Start;
      begin
        Serial_Io.Set (The_Baudrate => 19200,
                       On           => Channel);
        Serial_Io.Set_For_Read (The_Timeout => 1.0,
                                On          => Channel);
        Serial_Io.Send (The_Item => 'v',
                        To       => Channel);
        Serial_Io.Receive (The_Item => The_Version,
                           From     => Channel);
        Put ("Reader started for handbox version " & The_Version & " (Esc to abort)");
      exception
      when Serial_Io.Timeout =>
        Put ("Reader started for handbox with no version (Esc to abort)");
      end;
      Serial_Io.Set_For_Read (The_Timeout => Serial_Io.Infinite,
                              On          => Channel);
      loop
        The_Character := Serial_Io.Character_Of (Channel);
        Put ("- from Serial: " & The_Character);
      end loop;
    exception
    when Serial_Io.Aborted =>
      Put ("<aborted>");
    end Reader;

    The_Character : Character;

  begin -- Serial_Input
    Put ("Serial Input");
    The_Port := Detected_Port;
    if Serial_Io.Is_Available (The_Port) then
      Put ("Handbox detected on Port " & The_Port'img);
      The_Reader := new Reader;
      The_Reader.Start;
      loop
        Ada.Text_IO.Get_Immediate (The_Character);
        exit when The_Character = Ascii.Esc;
      end loop;
      Serial_Io.Free (The_Port);
    else
      Put ("Port " & The_Port'img & " not available");
    end if;
  exception
  when Incorrect_Port =>
    Input_Error ("incorrect port " & Port_Image);
  when Port_Not_Found =>
    Input_Error ("port not found");
  end Serial_Input;


  procedure Execute (Command : String) is

    Mount_Not_Connected : exception;

    procedure Connect_Mount is

      use type PWI4.Mount.State;

      procedure Wait_For_Mount_Connected is
        Connect_Timeout : constant := 5; -- seconds
      begin
        for Unused_Count in 1 .. Connect_Timeout loop
          delay 1.0;
          PWI4.Get_System;
          if PWI4.Mount.Info.Status = PWI4.Mount.Connected then
            return;
          end if;
        end loop;
        raise Mount_Not_Connected;
      end Wait_For_Mount_Connected;

    begin -- Connect_Mount
      PWI4.Get_System;
      if PWI4.Mount.Info.Status = PWI4.Mount.Disconnected then
        PWI4.Mount.Connect;
        Put ("connecting mount");
        Wait_For_Mount_Connected;
        Put ("mount connected");
      end if;
    end Connect_Mount;


    procedure Enable_Focuser is
    begin
      PWI4.Focuser.Enable;
    end Enable_Focuser;


    procedure Disable_Focuser is
    begin
      PWI4.Focuser.Disable;
    end Disable_Focuser;


    procedure Enable_Rotator is
    begin
      PWI4.Rotator.Enable;
    end Enable_Rotator;


    procedure Disable_Rotator is
    begin
      PWI4.Rotator.Disable;
    end Disable_Rotator;


    Mount_Not_Enabled       : exception;
    Mount_Must_Be_Connected : exception;

    procedure Enable_Mount is

      use type PWI4.Mount.State;

      procedure Wait_For_Mount_Enabled is
        Enable_Timeout : constant := 45; -- seconds
      begin
        for Unused_Count in 1 .. Enable_Timeout loop
          delay 1.0;
          PWI4.Get_System;
          if PWI4.Mount.Info.Status /= PWI4.Mount.Connected then
            return;
          end if;
        end loop;
        raise Mount_Not_Enabled;
      end Wait_For_Mount_Enabled;

    begin -- Enable_Mount
      PWI4.Get_System;
      if PWI4.Mount.Info.Status < PWI4.Mount.Connected then
        raise Mount_Must_Be_Connected;
      end if;
      PWI4.Get_System;
      if PWI4.Mount.Info.Status = PWI4.Mount.Connected then
        PWI4.Mount.Enable;
        Put ("enabling mount motors");
        Wait_For_Mount_Enabled;
        Put ("motors energized");
      end if;
    end Enable_Mount;


    Mount_Not_At_Home     : exception;
    Mount_Must_Be_Enabled : exception;

    procedure Home_Mount is

      use type PWI4.Mount.State;

      procedure Wait_For_Mount_At_Home is
      begin
        loop
          delay 1.0;
          PWI4.Get_System;
          case PWI4.Mount.Info.Status is
          when PWI4.Mount.Enabled =>
            null;
          when PWI4.Mount.Stopped =>
            exit;
          when others =>
            raise Mount_Not_At_Home;
          end case;
        end loop;
      end Wait_For_Mount_At_Home;

    begin -- Home_Mount
      PWI4.Get_System;
      if PWI4.Mount.Info.Status < PWI4.Mount.Enabled then
        raise Mount_Must_Be_Enabled;
      elsif PWI4.Mount.Info.Status = PWI4.Mount.Enabled then
        PWI4.Mount.Find_Home;
        Put ("homing mount");
        Wait_For_Mount_At_Home;
        Put ("homing mount completed succsessfully");
      end if;
    end Home_Mount;


    procedure Turn_M3 (To : PWI4.Port) is
    begin
      PWI4.M3.Turn (To);
    end Turn_M3;


    procedure Startup is
    begin
      Connect_Mount;
      Enable_Mount;
      Home_Mount;
    end Startup;


    procedure Move (Ra  : PWI4.Hours;
                    Dec : PWI4.Degrees) is
      use type PWI4.Mount.State;
    begin
      Startup;
      PWI4.Mount.Goto_Ra_Dec (Ra         => Ra,
                              Dec        => Dec,
                              From_J2000 => True);
      loop
        delay 1.0;
        PWI4.Get_System;
        declare
          Info : constant PWI4.Mount.Information := PWI4.Mount.Info;
        begin
          Put ("Mount State     : " & Strings.Legible_Of (Info.Status'img));
          Put ("      Ra        : " & Strings.Legible_Of (Info.Ra'img));
          Put ("      Dec       : " & Strings.Legible_Of (Info.Dec'img));
          Put ("      Ra J2000  : " & Strings.Legible_Of (Info.Ra_J2000'img));
          Put ("      Dec j2000 : " & Strings.Legible_Of (Info.Dec_J2000'img));
          if Info.Status = PWI4.Mount.Tracking then
            exit;
          end if;
        end;
      end loop;
    end Move;


    procedure Stop is
    begin
      loop
        PWI4.Get_System;
        case PWI4.Mount.Info.Status is
        when PWI4.Mount.Approaching | PWI4.Mount.Tracking =>
          PWI4.Mount.Stop;
        when others =>
          exit;
        end case;
        delay 1.0;
      end loop;
    end Stop;


    procedure Shutdown is
    begin
      Stop;
      PWI4.Mount.Disable;
      PWI4.Mount.Disconnect;
    end Shutdown;

    Id : constant String := Strings.Lowercase_Of (Command);

    use type PWI4.Degrees;
    use type PWI4.Hours;

  begin -- execute
    if Id = "connect" then
      Connect_Mount;
    elsif Id = "enable" then
      Enable_Mount;
    elsif Id = "home" then
      Home_Mount;
    elsif Id = "enablefocuser" then
      Enable_Focuser;
     elsif Id = "disablefocuser" then
      Disable_Focuser;
    elsif Id = "enablerotator" then
      Enable_Rotator;
     elsif Id = "disablerotator" then
      Disable_Rotator;
    elsif Id = "input" then
      Serial_Input ("");
    elsif Id = "turnto1" then
      Turn_M3 (To => PWI4.Port_1);
    elsif Id = "turnto2" then
      Turn_M3 (To => PWI4.Port_2);
    elsif Id = "startup" then
      Startup;
    elsif Id = "movem13" then
      Move (Ra  => 16.69486111,
            Dec => 36.4608333);
    elsif Id = "movem110" then
      Move (Ra  => (40.0 * 60.0 + 22.1) / 3600.0,
            Dec => 41.0 + ((41.0 * 60.0 + 7.0) / 3600.0));
    elsif Id = "stop" then
      Stop;
    elsif Id = "shutdown" then
      Shutdown;
    else
      Input_Error ("unknown command");
    end if;
  exception
  when Mount_Must_Be_Connected =>
    Error ("mount must be connected");
  when Mount_Must_Be_Enabled =>
    Error ("mount must be enabled");
  when Mount_Not_Connected =>
    Error ("failed to connect to mount");
  when Mount_Not_Enabled =>
    Error ("failed to energize the motors");
  when Mount_Not_At_Home =>
    Error ("failed to home the mount");
  end Execute;


  procedure Work is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    Put ("PWI_TEST");
    case Nr_Of_Arguments is
    when 0 =>
      Input_Error ("command missing");
    when 1 =>
      Execute (Ada.Command_Line.Argument(1));
    when 2 =>
      Serial_Input (Ada.Command_Line.Argument(2));
    when others =>
      Input_Error ("incorrect number of parameters");
    end case;
  exception
  when Occurrence: others =>
    Put (Exceptions.Information_Of (Occurrence));
  end Work;

end Test;
