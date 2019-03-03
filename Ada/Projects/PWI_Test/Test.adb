-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with PWI.Mount;
with Strings;

package body Test is

  procedure Put (Image : String) is
  begin
    Ada.Text_IO.Put_Line (Image);
  end Put;


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
      Put ("*** unknown command ***");
    end if;
  exception
  when Mount_Must_Be_Connected =>
    Put ("*** mount must be connected ***");
  when Mount_Must_Be_Enabled =>
    Put ("*** mount must be enabled ***");
  when Mount_Must_Be_Synchronised =>
    Put ("*** mount must be synchronised ***");
  when Mount_Not_Connected =>
    Put ("*** failed to connect to mount ***");
  when Mount_Not_Enabled =>
    Put ("*** failed to energize the motors ***");
  when Mount_Not_At_Home =>
    Put ("*** failed to home the mount ***");
  when Pointing_Model_Not_Set =>
    Put ("*** failed to set pointing model ***");
  end Execute;


  procedure Work is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Put ("*** command missing ***");
    elsif Nr_Of_Arguments > 1 then
      Put ("*** incorrect number of parameters ***");
    else
      Execute (Ada.Command_Line.Argument(1));
    end if;
  exception
  when PWI.No_Server =>
    Put ("*** server not available ***");
  when Occurrence: others =>
    Put (Exceptions.Information_Of (Occurrence));
  end Work;

end Test;
