-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with PWI.Mount;
with PWI.M3;
with PWI.Focuser;
with PWI.Rotator;
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

      use type PWI.Mount.State;

      procedure Wait_For_Mount_Connected is
        Connect_Timeout : constant := 5; -- seconds
      begin
        for Unused_Count in 1 .. Connect_Timeout loop
          delay 1.0;
          PWI.Get_System;
          if PWI.Mount.Status = PWI.Mount.Connected then
            return;
          end if;
        end loop;
        raise Mount_Not_Connected;
      end Wait_For_Mount_Connected;

    begin -- Connect_Mount
      PWI.Get_System;
      if PWI.Mount.Status = PWI.Mount.Disconnected then
        PWI.Mount.Connect;
        Put ("connecting mount");
        Wait_For_Mount_Connected;
        Put ("mount connected");
      end if;
    end Connect_Mount;


    Rotator_Not_Connected : exception;

    procedure Connect_Rotator (The_Port : PWI.Port) is

      use type PWI.Rotator.State;

      procedure Wait_For_Rotator_Connected is
        Connect_Timeout : constant := 5; -- seconds
      begin
        for Unused_Count in 1 .. Connect_Timeout loop
          delay 1.0;
          PWI.Get_System;
          if PWI.Rotator.Status /= PWI.Rotator.Disconnected then
            return;
          end if;
        end loop;
        raise Rotator_Not_Connected;
      end Wait_For_Rotator_Connected;

    begin -- Connect_Rotator
      PWI.Get_System;
      if PWI.Rotator.Status = PWI.Rotator.Disconnected then
        PWI.Focuser.Connect (The_Port);
        Put ("connecting rotator/focuser");
        Wait_For_Rotator_Connected;
        Put ("rotator connected/focuser");
      end if;
    end Connect_Rotator;


    Rotator_Not_Disconnected : exception;

    procedure Disconnect_Rotator (The_Port : PWI.Port) is

      use type PWI.Rotator.State;

      procedure Wait_For_Rotator_Disconnected is
        Disconnect_Timeout : constant := 5; -- seconds
      begin
        for Unused_Count in 1 .. Disconnect_Timeout loop
          delay 1.0;
          PWI.Get_System;
          if PWI.Rotator.Status = PWI.Rotator.Disconnected then
            return;
          end if;
        end loop;
        raise Rotator_Not_Disconnected;
      end Wait_For_Rotator_Disconnected;

    begin -- Disconnect_Rotator
      PWI.Get_System;
      if PWI.Rotator.Status = PWI.Rotator.Connected then
        PWI.Focuser.Disconnect (The_Port);
        Put ("disconnecting rotator/focuser");
        Wait_For_Rotator_Disconnected;
        Put ("rotator/focuser disconnected");
      end if;
    end Disconnect_Rotator;


    Mount_Not_Enabled       : exception;
    Mount_Must_Be_Connected : exception;

    procedure Enable_Mount is

      use type PWI.Mount.State;

      procedure Wait_For_Mount_Enabled is
        Enable_Timeout : constant := 45; -- seconds
      begin
        for Unused_Count in 1 .. Enable_Timeout loop
          delay 1.0;
          PWI.Get_System;
          if PWI.Mount.Status /= PWI.Mount.Connected then
            return;
          end if;
        end loop;
        raise Mount_Not_Enabled;
      end Wait_For_Mount_Enabled;

    begin -- Enable_Mount
      PWI.Get_System;
      if PWI.Mount.Status < PWI.Mount.Connected then
        raise Mount_Must_Be_Connected;
      end if;
      PWI.Get_System;
      if PWI.Mount.Status = PWI.Mount.Connected then
        PWI.Mount.Enable;
        Put ("enabling mount motors");
        Wait_For_Mount_Enabled;
        Put ("motors energized");
      end if;
    end Enable_Mount;


    Mount_Not_At_Home     : exception;
    Mount_Must_Be_Enabled : exception;

    procedure Home_Mount is

      use type PWI.Mount.State;

      procedure Wait_For_Mount_At_Home is
      begin
        loop
          delay 1.0;
          PWI.Get_System;
          case PWI.Mount.Status is
          when PWI.Mount.Homing =>
            null;
          when PWI.Mount.Synchronised | PWI.Mount.Stopped =>
            exit;
          when others =>
            raise Mount_Not_At_Home;
          end case;
        end loop;
      end Wait_For_Mount_At_Home;

    begin -- Home_Mount
      PWI.Get_System;
      if PWI.Mount.Status < PWI.Mount.Enabled then
        raise Mount_Must_Be_Enabled;
      elsif PWI.Mount.Status = PWI.Mount.Enabled then
        PWI.Mount.Find_Home;
        Put ("homing mount");
        Wait_For_Mount_At_Home;
        Put ("homing mount completed succsessfully");
      end if;
    end Home_Mount;


    Rotator_Must_Be_Connected : exception;

    procedure Home_Rotator (The_Port : PWI.Port) is

      use type PWI.Rotator.State;

      procedure Wait_For_Rotator_At_Home is
      begin
        loop
          delay 1.0;
          PWI.Get_System;
          case PWI.Rotator.Status is
          when PWI.Rotator.Homing =>
            null;
          when others =>
            exit;
          end case;
        end loop;
      end Wait_For_Rotator_At_Home;

    begin -- Home_Rotator
      PWI.Get_System;
      if PWI.Rotator.Status < PWI.Rotator.Connected then
        raise Rotator_Must_Be_Connected;
      elsif PWI.Rotator.Status = PWI.Rotator.Connected then
        PWI.Rotator.Find_Home (The_Port);
        Put ("homing rotator");
        Wait_For_Rotator_At_Home;
        Put ("homing rotator completed succsessfully");
      end if;
    end Home_Rotator;


    procedure Start_Rotator is
      use type PWI.Rotator.State;
    begin
      PWI.Get_System;
      if PWI.Rotator.Status < PWI.Rotator.Connected then
        raise Rotator_Must_Be_Connected;
      elsif PWI.Rotator.Status = PWI.Rotator.Connected then
        PWI.Rotator.Start;
        Put ("Start rotator");
      end if;
    end Start_Rotator;


    Focuser_Incorrect_Position : exception;
    Focuser_Not_Connected      : exception;

    procedure Move_Focuser (On          : PWI.Port;
                            To_Position : String) is
    begin
      declare
        Position : constant PWI.Microns := PWI.Microns'value(To_Position);
      begin
        Put ("move focuser to " & To_Position);
        PWI.Focuser.Move (On, To_Position => Position);
      end;
    exception
    when PWI.No_Server =>
      raise Focuser_Not_Connected;
    when others =>
      raise Focuser_Incorrect_Position;
    end Move_Focuser;


    procedure Turn_M3 (To : PWI.Port) is
    begin
      PWI.M3.Turn (To);
    end Turn_M3;


    Pointing_Model_Not_Set     : exception;
    Mount_Must_Be_Synchronised : exception;

    procedure Set_Pointing_Model is

      use type PWI.Mount.State;

      procedure Wait_For_Ponting_Model_Set is
      begin
        delay 1.0;
        PWI.Get_System;
        if PWI.Mount.Status /= PWI.Mount.Stopped then
          raise Pointing_Model_Not_Set;
        end if;
      end Wait_For_Ponting_Model_Set;

    begin -- Set_Pointing_Model
      PWI.Get_System;
      if PWI.Mount.Status < PWI.Mount.Synchronised then
        raise Mount_Must_Be_Synchronised;
      end if;
      PWI.Mount.Set_Pointing_Model;
      Put ("set pointing model");
      Wait_For_Ponting_Model_Set;
      Put ("pointing model set");
    end Set_Pointing_Model;


    procedure Startup is
    begin
      Connect_Mount;
      Enable_Mount;
      Home_Mount;
      Set_Pointing_Model;
    end Startup;


    procedure Move (Ra  : PWI.Mount.Hours;
                    Dec : PWI.Mount.Degrees) is
      use type PWI.Mount.State;
    begin
      Startup;
      PWI.Mount.Move (Ra         => Ra,
                      Dec        => Dec,
                      From_J2000 => True);
      loop
        delay 1.0;
        PWI.Get_System;
        declare
          Info : constant PWI.Mount.Information := PWI.Mount.Info;
        begin
          Put ("Mount State    : " & Strings.Legible_Of (Info.Status'img));
          Put ("      Ra       : " & Strings.Legible_Of (Info.Ra'img));
          Put ("      Dec      : " & Strings.Legible_Of (Info.Dec'img));
          Put ("      Ra 2000  : " & Strings.Legible_Of (Info.Ra_2000'img));
          Put ("      Dec 2000 : " & Strings.Legible_Of (Info.Dec_2000'img));
          if Info.Status = PWI.Mount.Tracking then
            exit;
          end if;
        end;
      end loop;
    end Move;


    procedure Stop is
    begin
      loop
        PWI.Get_System;
        case PWI.Mount.Status is
        when PWI.Mount.Homing | PWI.Mount.Approaching | PWI.Mount.Tracking =>
          PWI.Mount.Stop;
        when others =>
          exit;
        end case;
        delay 1.0;
      end loop;
    end Stop;


    procedure Shutdown is
    begin
      Stop;
      PWI.Mount.Disable;
      PWI.Mount.Disconnect;
    end Shutdown;

    Id : constant String := Strings.Lowercase_Of (Command);

    use type PWI.Mount.Degrees;
    use type PWI.Mount.Hours;

  begin -- execute
    PWI.Mount.Define_Pointing_Model ("First.pxp");
    if Id = "connect" then
      Connect_Mount;
    elsif Id = "enable" then
      Enable_Mount;
    elsif Id = "home" then
      Home_Mount;
    elsif Id = "setmodel" then
      Set_Pointing_Model;
    elsif Id = "connectrotator1" then
      Connect_Rotator (PWI.Port_1);
    elsif Id = "connectrotator2" then
      Connect_Rotator (PWI.Port_2);
    elsif Id = "disconnectrotator1" then
      Disconnect_Rotator (PWI.Port_1);
    elsif Id = "disconnectrotator2" then
      Disconnect_Rotator (PWI.Port_2);
    elsif Id = "homerotator1" then
      Home_Rotator (PWI.Port_1);
    elsif Id = "homerotator2" then
      Home_Rotator (PWI.Port_2);
    elsif Id = "input" then
      Serial_Input ("");
    elsif Id = "startrotator" then
      Start_Rotator;
    elsif Id = "turnto1" then
      Turn_M3 (To => PWI.Port_1);
    elsif Id = "turnto2" then
      Turn_M3 (To => PWI.Port_2);
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
    elsif Id'length > 6 and then Id(Id'first..Id'first+5) = "move1=" then
      Move_Focuser (PWI.Port_1, Id(Id'first+6..Id'last));
    elsif Id'length > 6 and then Id(Id'first..Id'first+5) = "move2=" then
      Move_Focuser (PWI.Port_2, Id(Id'first+6..Id'last));
    else
      Input_Error ("unknown command");
    end if;
  exception
  when Mount_Must_Be_Connected =>
    Error ("mount must be connected");
  when Mount_Must_Be_Enabled =>
    Error ("mount must be enabled");
  when Mount_Must_Be_Synchronised =>
    Error ("mount must be synchronised");
  when Mount_Not_Connected =>
    Error ("failed to connect to mount");
  when Mount_Not_Enabled =>
    Error ("failed to energize the motors");
  when Mount_Not_At_Home =>
    Error ("failed to home the mount");
  when Pointing_Model_Not_Set =>
    Error ("failed to set pointing model");
  when Rotator_Not_Connected =>
    Error ("failed to connect to rotator");
  when Rotator_Not_Disconnected =>
    Error ("failed to disconnect the rotator");
  when Rotator_Must_Be_Connected =>
    Error ("rotator must be connected");
  when Focuser_Not_Connected =>
    Error ("focuser not connected");
  when Focuser_Incorrect_Position =>
    Error ("focuser position incorrect");
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
  when PWI.No_Server =>
    Error ("server not available");
  when Occurrence: others =>
    Put (Exceptions.Information_Of (Occurrence));
  end Work;

end Test;
