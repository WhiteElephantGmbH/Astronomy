-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Interfaces.C;
with PWI.Mount;
with PWI.M3;
with PWI.Focuser;
with PWI.Rotator;
with Strings;
with System;
with Win32.Bluetooth;
with Win32.Winbase;
with Win32.Winerror;
with Win32.Winnt;

with Log;

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
    Put ("  Connect");
    Put ("  Enable");
    Put ("  Home");
    Put ("  SetModel");
    Put ("  ConnectRotator");
    Put ("  HomeRotator");
    Put ("  StartRotator");
    Put ("  TurnTo1");
    Put ("  TurnTo2");
    Put ("  Startup");
    Put ("  MoveM13");
    Put ("  MoveM110");
    Put ("  Stop");
    Put ("  Shutdown");
  end Input_Error;


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

    procedure Connect_Rotator is

      use type PWI.Rotator.State;

      procedure Wait_For_Rotator_Connected is
        Connect_Timeout : constant := 5; -- seconds
      begin
        for Unused_Count in 1 .. Connect_Timeout loop
          delay 1.0;
          PWI.Get_System;
          if PWI.Rotator.Status = PWI.Rotator.Connected then
            return;
          end if;
        end loop;
        raise Rotator_Not_Connected;
      end Wait_For_Rotator_Connected;

    begin -- Connect_Rotator
      PWI.Get_System;
      if PWI.Rotator.Status = PWI.Rotator.Disconnected then
        PWI.Focuser.Connect;
        Put ("connecting rotator/focuser");
        Wait_For_Rotator_Connected;
        Put ("rotator connected/focuser");
      end if;
    end Connect_Rotator;


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

    procedure Home_Rotator is

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
        PWI.Rotator.Find_Home;
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


    procedure Turn_M3 (To : PWI.M3.Port) is
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
    elsif Id = "connectrotator" then
      Connect_Rotator;
    elsif Id = "homerotator" then
      Home_Rotator;
    elsif Id = "startrotator" then
      Start_Rotator;
    elsif Id = "turnto1" then
      Turn_M3 (To => PWI.M3.Port_1);
    elsif Id = "turnto2" then
      Turn_M3 (To => PWI.M3.Port_2);
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
  when Rotator_Must_Be_Connected =>
    Error ("rotator must be connected");
  end Execute;


  procedure Work is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    Put ("PWI_TEST");
    if Nr_Of_Arguments = 0 then
      declare
        use Win32;
        Ok     : BOOL := Bluetooth.Is_Discoverable (System.Null_Address);
        Result : DWORD;
      begin
        Put ("Is_Discoverable Result:" & Result'img);
        declare
          Parameters        : aliased Bluetooth.Find_Radio_Params;
          Radio_Handle      : aliased Bluetooth.Radio_Handle;
          Find_Radio_Handle : aliased Bluetooth.Radio_Find_Handle;
          Radio_Info        : aliased Bluetooth.Radio_Info;
          use type Bluetooth.Radio_Find_Handle;
        begin
          Put ("Find_First_Radio");
          Find_Radio_Handle := Bluetooth.Find_First_Radio (Arg1 => Parameters'access,
                                                           Arg2 => Radio_Handle'access);
          if Find_Radio_Handle /= Bluetooth.No_Radio_Found then
            loop
              Result := Bluetooth.Get_Radio_Info (Arg1 => Radio_Handle,
                                                  Arg2 => Radio_Info'access);
              case Result is
              when Win32.Winerror.NO_ERROR =>
                Put ("Get_Radio_Info Name:" &
                  Ada.Characters.Handling.To_String (Interfaces.C.To_Ada (Interfaces.C.wchar_array(Radio_Info.Name))));
                declare
                  Parameters         : aliased Bluetooth.Device_Search_Params;
                  Find_Device_Handle : aliased Bluetooth.Device_Find_Handle;
                  Device_Info        : aliased Bluetooth.Device_Info;
                  use type Bluetooth.Device_Find_Handle;
                begin
                  Put ("Lookup Devices");
                  Parameters.Radio := Radio_Handle;
                  Find_Device_Handle := Bluetooth.Find_First_Device (Arg1 => Parameters'access,
                                                                     Arg2 => Device_Info'access);
                  if Find_Device_Handle /= Bluetooth.No_Device_Found then
                    loop
                      Put ("Found_Device:" &
                        Ada.Characters.Handling.To_String (Interfaces.C.To_Ada (Interfaces.C.wchar_array(
                          Device_Info.Name))));

                      Put ("  Class_Of_Device:" & Device_Info.Class_Of_Device'img);
                      Put ("  Address        :" & Device_Info.Address.Anon2422.Ull_Long'img);
                      Put ("  Connected      :" & Device_Info.Connected'img);
                      Put ("  Remembered     :" & Device_Info.Remembered'img);
                      Put ("  Authenticated  :" & Device_Info.Authenticated'img);

                      Ok := Bluetooth.Find_Next_Device (Arg1 => Find_Device_Handle,
                                                        Arg2 => Device_Info'access);
                      Put ("Find_Next_Next Ok:" & Ok'img);
                      exit when Ok = Win32.FALSE;
                    end loop;
                    Ok := Bluetooth.Find_Device_Close (Find_Device_Handle);
                    Put ("Find_Device_Close Ok:" & Ok'img);
                  end if;
                end;
              when others =>
                Put ("Get_Radio_Info Result:" & Result'img);
              end case;
              Ok := Winbase.CloseHandle(Win32.Winnt.HANDLE(Radio_Handle));
              Put ("Close Radio_Handle Ok:" & Ok'img);

              Ok := Bluetooth.Find_Next_Radio (Arg1 => Find_Radio_Handle,
                                               Arg2 => Radio_Handle'access);
              Put ("Find_Next_Radio Ok:" & Ok'img);
              exit when Ok = Win32.FALSE;
            end loop;
            Ok := Bluetooth.Find_Radio_Close (Find_Radio_Handle);
            Put ("Find_Radio_Close Ok:" & Ok'img);
          end if;
        end;
      end;
    elsif Nr_Of_Arguments > 1 then
      Input_Error ("incorrect number of parameters");
    else
      Execute (Ada.Command_Line.Argument(1));
    end if;
  exception
  when PWI.No_Server =>
    Error ("server not available");
  when Occurrence: others =>
    Put (Exceptions.Information_Of (Occurrence));
  end Work;

end Test;
