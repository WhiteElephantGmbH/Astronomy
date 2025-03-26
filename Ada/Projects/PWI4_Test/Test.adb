-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with PWI4.Focuser;
with PWI4.Rotator;
with PWI4.Mount;
with PWI4.M3;
with Serial_Io.Usb;
with Text;

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
    Put ("  Connect");
    Put ("  Enable");
    Put ("  Home");
    Put ("  Disable");
    Put ("  Disconnect");
    Put ("  ConnectFocuser0");
    Put ("  ConnectFocuser1");
    Put ("  DisconnectFocuser0");
    Put ("  DisconnectFocuser1");
    Put ("  HomeFocuser0");
    Put ("  HomeFocuser1");
    Put ("  Focuser0Goto5000");
    Put ("  Focuser1Goto5000");
    Put ("  ConnectRotator0");
    Put ("  ConnectRotator1");
    Put ("  DisconnectRotator0");
    Put ("  DisconnectRotator1");
    Put ("  HomeRotator0");
    Put ("  HomeRotator1");
    Put ("  Rotator0Offset0");
    Put ("  Rotator1Offset0");
    Put ("  Rotator0Goto180");
    Put ("  Rotator1Goto180");
    Put ("  TurnTo1");
    Put ("  TurnTo2");
    Put ("  Startup");
    Put ("  MoveM13");
    Put ("  MoveM110");
    Put ("  Stop");
    Put ("  Shutdown");
    Put ("  Crash");
    Put ("  Error");
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
          Put ("State: " & PWI4.Mount.Info.Status'image);
          if PWI4.Mount.Info.Status >= PWI4.Mount.Connected then
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
      declare
        State : constant PWI4.Mount.State := PWI4.Mount.Info.Status;
      begin
        if State < PWI4.Mount.Enabled then
          Put ("PWI4.Mount.Info.Status: " & State'image);
          raise Mount_Must_Be_Enabled;
        elsif PWI4.Mount.Info.Status = PWI4.Mount.Enabled then
          PWI4.Mount.Find_Home;
          Put ("homing mount");
          Wait_For_Mount_At_Home;
          Put ("homing mount completed succsessfully");
        end if;
      end;
    end Home_Mount;


    procedure Disable_Mount is

    begin
      PWI4.Mount.Disable;
    end Disable_Mount;


    procedure Disconnect_Mount is

    begin
      PWI4.Mount.Disconnect;
    end Disconnect_Mount;


    procedure Connect_Focuser_0 is

    begin
      PWI4.Focuser.Connect (0);
    end Connect_Focuser_0;


    procedure Connect_Focuser_1 is

    begin
      PWI4.Focuser.Connect (1);
    end Connect_Focuser_1;


    procedure Connect_Rotator_0 is

    begin
      PWI4.Rotator.Connect (0);
    end Connect_Rotator_0;


    procedure Connect_Rotator_1 is

    begin
      PWI4.Rotator.Connect (1);
    end Connect_Rotator_1;


    procedure Disconnect_Focuser_0 is

    begin
      PWI4.Focuser.Disconnect (0);
    end Disconnect_Focuser_0;


    procedure Disconnect_Focuser_1 is

    begin
      PWI4.Focuser.Disconnect (1);
    end Disconnect_Focuser_1;


    procedure Disconnect_Rotator_0 is

    begin
      PWI4.Rotator.Disconnect (0);
    end Disconnect_Rotator_0;


    procedure Disconnect_Rotator_1 is

    begin
      PWI4.Rotator.Disconnect (1);
    end Disconnect_Rotator_1;


    procedure Home_Focuser_0 is
    begin
      PWI4.Focuser.Find_Home (0);
    end Home_Focuser_0;


    procedure Home_Focuser_1 is
    begin
      PWI4.Focuser.Find_Home (1);
    end Home_Focuser_1;


    procedure Focuser_0_Goto_5000 is
    begin
      PWI4.Focuser.Go_To (5000.0, 0);
    end Focuser_0_Goto_5000;


    procedure Focuser_1_Goto_5000 is
    begin
      PWI4.Focuser.Go_To (5000.0, 1);
    end Focuser_1_Goto_5000;


    procedure Home_Rotator_0 is
    begin
      PWI4.Rotator.Find_Home (0);
    end Home_Rotator_0;


    procedure Home_Rotator_1 is
    begin
      PWI4.Rotator.Find_Home (1);
    end Home_Rotator_1;


    procedure Rotator_0_Offset_0 is
    begin
      PWI4.Rotator.Goto_Offset (0.0, 0);
    end Rotator_0_Offset_0;


    procedure Rotator_1_Offset_0 is
    begin
      PWI4.Rotator.Goto_Offset (0.0, 1);
    end Rotator_1_Offset_0;


    procedure Rotator_0_Goto_180 is
    begin
      PWI4.Rotator.Goto_Mech (180.0, 0);
    end Rotator_0_Goto_180;


    procedure Rotator_1_Goto_180 is
    begin
      PWI4.Rotator.Goto_Mech (180.0, 1);
    end Rotator_1_Goto_180;


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
      PWI4.Mount.Goto_Ra_Dec (With_Ra    => Ra,
                              With_Dec   => Dec,
                              From_J2000 => True);
      loop
        delay 1.0;
        PWI4.Get_System;
        declare
          Info : constant PWI4.Mount.Information := PWI4.Mount.Info;
        begin
          Put ("Mount State     : " & Text.Legible_Of (Info.Status'img));
          Put ("      Ra        : " & Text.Legible_Of (Info.Ra'img));
          Put ("      Dec       : " & Text.Legible_Of (Info.Dec'img));
          Put ("      Ra J2000  : " & Text.Legible_Of (Info.Ra_J2000'img));
          Put ("      Dec j2000 : " & Text.Legible_Of (Info.Dec_J2000'img));
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

    procedure Test_Crash is
    begin
      PWI4.Test_Crash;
      Put ("State: " & PWI4.Mount.Info.Status'image);
      Put ("Error: " & PWI4.Error_Info);
    end Test_Crash;


    procedure Test_Error is
    begin
      PWI4.Test_Error;
      Put ("State: " & PWI4.Mount.Info.Status'image);
      Put ("Error: " & PWI4.Error_Info);
    end Test_Error;


    Id : constant String := Text.Lowercase_Of (Command);

    use type PWI4.Degrees;
    use type PWI4.Hours;

  begin -- execute
    if Id = "connect" then
      Connect_Mount;
    elsif Id = "enable" then
      Enable_Mount;
    elsif Id = "home" then
      Home_Mount;
    elsif Id = "disable" then
      Disable_Mount;
    elsif Id = "disconnect" then
      Disconnect_Mount;
    elsif Id = "connectfocuser0" then
      Connect_Focuser_0;
    elsif Id = "connectfocuser1" then
      Connect_Focuser_1;
    elsif Id = "connectrotator0" then
      Connect_Rotator_0;
    elsif Id = "connectrotator1" then
      Connect_Rotator_1;
    elsif Id = "disconnectfocuser0" then
      Disconnect_Focuser_0;
    elsif Id = "disconnectfocuser1" then
      Disconnect_Focuser_1;
    elsif Id = "disconnectrotator0" then
      Disconnect_Rotator_0;
    elsif Id = "disconnectrotator1" then
      Disconnect_Rotator_1;
    elsif Id = "homefocuser0" then
      Home_Focuser_0;
    elsif Id = "homefocuser1" then
      Home_Focuser_1;
    elsif Id = "homerotator0" then
      Home_Rotator_0;
    elsif Id = "homerotator1" then
      Home_Rotator_1;
    elsif Id = "focuser0goto5000" then
      Focuser_0_Goto_5000;
    elsif Id = "focuser1goto5000" then
      Focuser_1_Goto_5000;
    elsif Id = "rotator0offset0" then
      Rotator_0_Offset_0;
    elsif Id = "rotator1offset0" then
      Rotator_1_Offset_0;
    elsif Id = "rotator0goto180" then
      Rotator_0_Goto_180;
    elsif Id = "rotator1goto180" then
      Rotator_1_Goto_180;
    elsif Id = "enable" then
      Enable_Mount;
    elsif Id = "home" then
      Home_Focuser_0;
      Home_Rotator_0;
      Home_Mount;
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
    elsif Id = "crash" then
      Test_Crash;
    elsif Id = "error" then
      Test_Error;
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
