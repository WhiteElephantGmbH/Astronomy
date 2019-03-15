-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Parameter;
with PWI.Fans;
with PWI.Mount;
with Traces;

package body Device is

  package Log is new Traces ("Device");

  type Fan_Action is (No_Action, Turn_On, Turn_Off);

  type Mount_Action is (No_Action,
                        Stop,
                        Connect,
                        Disconnect,
                        Enable,
                        Disable,
                        Find_Home,
                        Set_Pointing_Model,
                        Goto_Target,
                        Goto_Mark);

  type Parameters is record
    Ra         : PWI.Mount.Hours   := 0.0;
    Dec        : PWI.Mount.Degrees := 0.0;
    From_J2000 : Boolean           := False;
    Alt        : PWI.Mount.Degrees := 0.0;
    Azm        : PWI.Mount.Degrees := 0.0;
  end record;


  protected Action is

    procedure Put (Item : Fan_Action);

    procedure Put (Item : Mount_Action);

    procedure Move (Ra         : PWI.Mount.Hours;
                    Dec        : PWI.Mount.Degrees;
                    From_J2000 : Boolean := False);

    procedure Move (Alt : PWI.Mount.Degrees;
                    Azm : PWI.Mount.Degrees);

    procedure Finish;

    entry Get (New_Fan_Action   : out Fan_Action;
               New_Mount_Action : out Mount_Action;
               Is_Termination   : out Boolean;
               New_Parameter    : out Parameters);

  private
    The_Fan_Action   : Fan_Action   := No_Action;
    The_Mount_Action : Mount_Action := No_Action;
    Is_Finishing     : Boolean      := False;
    Is_Pending       : Boolean      := False;
    The_Parameter    : Parameters;
  end Action;


  protected body Action is

    procedure Put (Item : Fan_Action) is
    begin
      The_Fan_Action := Item;
      Is_Pending := True;
    end Put;


    procedure Put (Item : Mount_Action) is
    begin
      The_Mount_Action := Item;
      Is_Pending := True;
    end Put;


    procedure Move (Ra         : PWI.Mount.Hours;
                    Dec        : PWI.Mount.Degrees;
                    From_J2000 : Boolean := False) is
    begin
      The_Mount_Action := Goto_Target;
      The_Parameter.Ra := Ra;
      The_Parameter.Dec := Dec;
      The_Parameter.From_J2000 := From_J2000;
      Is_Pending := True;
    end Move;


    procedure Move (Alt : PWI.Mount.Degrees;
                    Azm : PWI.Mount.Degrees) is
    begin
      The_Mount_Action := Goto_Mark;
      The_Parameter.Alt := Alt;
      The_Parameter.Azm := Azm;
      Is_Pending := True;
    end Move;


    procedure Finish is
    begin
      Is_Finishing := True;
      Is_Pending := True;
    end Finish;


    entry Get (New_Fan_Action   : out Fan_Action;
               New_Mount_Action : out Mount_Action;
               Is_Termination   : out Boolean;
               New_Parameter    : out Parameters) when Is_Pending is
    begin
      New_Fan_Action := The_Fan_Action;
      New_Mount_Action := The_Mount_Action;
      The_Fan_Action := No_Action;
      The_Mount_Action := No_Action;
      Is_Termination  := Is_Finishing;
      New_Parameter := The_Parameter;
      Is_Pending := False;
    end Get;

  end Action;


  task type Control is

    entry Start (Item : Mount.State_Handler_Access);

  end Control;

  The_Control : access Control;


  task body Control is

    The_Mount_State_Handler : Mount.State_Handler_Access;
    Is_Simulating           : constant Boolean := Parameter.Is_Simulation_Mode;

    Is_Finishing     : Boolean := False;
    The_Fan_Action   : Fan_Action := No_Action;
    The_Mount_Action : Mount_Action := No_Action;
    The_Mount_State  : Mount.State := Mount.Unknown;
    The_Parameter    : Parameters;

    use type Mount.State;

    use type PWI.Mount.State;

  begin
    accept Start (Item : Mount.State_Handler_Access)
    do
      The_Mount_State_Handler := Item;
    end Start;
    Log.Write ("Control started" & (if Is_Simulating then " Simulation" else ""));
    loop
      begin
        select
          Action.Get (New_Fan_Action   => The_Fan_Action,
                      New_Mount_Action => The_Mount_Action,
                      Is_Termination   => Is_Finishing,
                      New_Parameter    => The_Parameter);
          Log.Write ("Handle - Fan " & The_Fan_Action'img & " - Mount " & The_Mount_Action'img);

          begin
            case The_Fan_Action is
            when No_Action =>
              null;
            when Turn_On =>
              if not Is_Simulating then
                PWI.Fans.Turn_On;
              end if;
            when Turn_Off =>
              if not Is_Simulating then
                PWI.Fans.Turn_Off;
              end if;
            end case;
          exception
          when others =>
            null; -- PWI server could have been terminated
          end;

          if Is_Finishing then
            begin
              PWI.Mount.Stop;
            exception
            when others =>
              null; -- PWI server could have been terminated
            end;
            exit;
          end if;

          case The_Mount_Action is
          when No_Action =>
            null;
          when Stop =>
            PWI.Mount.Stop;
            if Is_Simulating then
              The_Mount_State := Mount.Stopped;
            end if;
          when Connect =>
            if Is_Simulating then
              The_Mount_State := Mount.Connected;
            else
              PWI.Mount.Connect;
            end if;
          when Disconnect =>
            if Is_Simulating then
              The_Mount_State := Mount.Disconnected;
            else
              PWI.Mount.Disconnect;
            end if;
          when Enable =>
            if Is_Simulating then
              The_Mount_State := Mount.Enabled;
            else
              PWI.Mount.Enable;
            end if;
          when Disable =>
            if Is_Simulating then
              The_Mount_State := Mount.Connected;
            else
              PWI.Mount.Disable;
            end if;
          when Find_Home =>
            if Is_Simulating then
              The_Mount_State := Mount.Synchronised;
            else
              PWI.Mount.Find_Home;
            end if;
          when Set_Pointing_Model =>
            if Is_Simulating then
              The_Mount_State := Mount.Stopped;
            else
              PWI.Mount.Set_Pointing_Model;
            end if;
          when Goto_Target =>
            PWI.Mount.Move (Ra         => The_Parameter.Ra,
                            Dec        => The_Parameter.Dec,
                            From_J2000 => The_Parameter.From_J2000);
          when Goto_Mark =>
            PWI.Mount.Move (Alt => The_Parameter.Alt,
                            Azm => The_Parameter.Azm);
          end case;
        or
          delay 1.0;
          PWI.Get_System;
        end select;
        if Is_Simulating then
          case PWI.Mount.Status is
          when PWI.Mount.Approaching =>
            The_Mount_State := Mount.Approaching;
          when PWI.Mount.Tracking =>
            The_Mount_State := Mount.Tracking;
          when others =>
            if The_Mount_State = Mount.Unknown then
              The_Mount_State := Mount.Disconnected;
            end if;
          end case;
        else
          case PWI.Mount.Status is
          when PWI.Mount.Disconnected =>
            The_Mount_State := Mount.Disconnected;
          when PWI.Mount.Connected =>
            The_Mount_State := Mount.Connected;
          when PWI.Mount.Enabled =>
            The_Mount_State := Mount.Enabled;
          when PWI.Mount.Homing =>
            The_Mount_State := Mount.Homing;
          when PWI.Mount.Synchronised =>
            The_Mount_State := Mount.Synchronised;
          when PWI.Mount.Stopped =>
            The_Mount_State := Mount.Stopped;
          when PWI.Mount.Approaching =>
            The_Mount_State := Mount.Approaching;
          when PWI.Mount.Tracking =>
            The_Mount_State := Mount.Tracking;
          end case;
        end if;
      exception
      when PWI.No_Server =>
        The_Mount_State := Mount.Unknown;
      end;
      The_Mount_State_Handler (The_Mount_State);
    end loop;
    Log.Write ("Control end");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Control;


  procedure Start (Mount_State_Handler  : Mount.State_Handler_Access;
                   Pointing_Model       : String) is
  begin
    PWI.Mount.Define_Pointing_Model (Pointing_Model);
    The_Control := new Control;
    The_Control.Start (Mount_State_Handler);
  end Start;


  procedure Finalize is
  begin
    Fans.Turn_On_Or_Off;
    Action.Finish;
  end Finalize;


  package body Fans is

    procedure Turn_On_Or_Off is
    begin
      Log.Write ("Fans.Turn_On_Or_Off");
      if Parameter.Turn_Fans_On then
        if not PWI.Fans.Turned_On then
          Action.Put (Turn_On);
        end if;
      else
        if PWI.Fans.Turned_On then
          Action.Put (Turn_Off);
        end if;
      end if;
    end Turn_On_Or_Off;

  end Fans;


  package body Mount is

    function Image_Of (The_Direction : Space.Direction) return String is
    begin
      if Space.Direction_Is_Known (The_Direction) then
        return "RA: " & Space.Ra_Image_Of (The_Direction) & " - DEC: " & Space.Dec_Image_Of (The_Direction);
      end if;
      return "Unknown";
    end Image_Of;


    function Image_Of (The_Direction : Earth.Direction) return String is
    begin
      if Earth.Direction_Is_Known (The_Direction) then
        return "AZ: " & Earth.Az_Image_Of (The_Direction) & " - ALT: " & Earth.Alt_Image_Of (The_Direction);
      end if;
      return "Unknown";
    end Image_Of;


    function Actual_Info return Information is
      Mount_Info : constant PWI.Mount.Information := PWI.Mount.Info;
      use type Angle.Value;
    begin
      case Mount_Info.Status is
      when PWI.Mount.Stopped | PWI.Mount.Approaching | PWI.Mount.Tracking =>
        return (J2000_Direction  => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra_2000)),
                                                        Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec_2000))),
                Actual_Direction => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra)),
                                                        Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec))),
                Local_Direction  => Earth.Direction_Of (Az  => Angle.Value'(+Angle.Degrees(Mount_Info.Azm)),
                                                        Alt => Angle.Value'(+Angle.Degrees(Mount_Info.Alt))));
      when others =>
        return (J2000_Direction  => Space.Unknown_Direction,
                Actual_Direction => Space.Unknown_Direction,
                Local_Direction  => Earth.Unknown_Direction);
      end case;
    end Actual_Info;


    procedure Connect is
    begin
      Log.Write ("Mount.Connect");
      Action.Put (Mount_Action'(Connect));
    end Connect;


    procedure Disconnect is
    begin
      Log.Write ("Mount.Disconnect");
      Action.Put (Mount_Action'(Disconnect));
    end Disconnect;


    procedure Enable is
    begin
      Log.Write ("Mount.Enable");
      Action.Put (Mount_Action'(Enable));
    end Enable;


    procedure Disable is
    begin
      Log.Write ("Mount.Disable");
      Action.Put (Mount_Action'(Disable));
    end Disable;


    procedure Find_Home (Completion_Time : out Time.Ut) is
    begin
      Log.Write ("Mount.Find_Home");
      Action.Put (Mount_Action'(Find_Home));
      Completion_Time := Time.Universal + 125.0;
    end Find_Home;


    procedure Set_Pointing_Model is
    begin
      Log.Write ("Mount.Set_Pointing_Model");
      Action.Put (Mount_Action'(Set_Pointing_Model));
    end Set_Pointing_Model;


    procedure Goto_Target (Direction       :     Space.Direction;
                           Completion_Time : out Time.Ut) is
      use type Angle.Signed;
      use type Angle.Value;
    begin
      Log.Write ("Mount.Goto_Target " & Image_Of (Direction));
      Action.Move (Ra         => PWI.Mount.Hours(Angle.Hours'(+Space.Ra_Of (Direction))),
                   Dec        => PWI.Mount.Degrees(Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (Direction)))),
                   From_J2000 => False);
      Completion_Time := Time.Universal + 22.0;
    end Goto_Target;


    procedure Goto_Mark (Direction       :     Earth.Direction;
                         Completion_Time : out Time.Ut) is
      use type Angle.Value;
    begin
      Log.Write ("Mount.Goto_Mark " & Image_Of (Direction));
      pragma Assert (Earth.Direction_Is_Known (Direction));
      Action.Move (Alt => PWI.Mount.Degrees(Angle.Degrees'(+Earth.Alt_Of (Direction))),
                   Azm => PWI.Mount.Degrees(Angle.Degrees'(+Earth.Az_Of (Direction))));
      Completion_Time := Time.Universal + 25.0;
    end Goto_Mark;


    procedure Direct (The_Drive  : Drive;
                      With_Speed : Angle.Signed) is
      pragma Unreferenced (With_Speed);
    begin
      Log.Write ("Mount.Direct " & The_Drive'img);
    end Direct;


    procedure Adjust (The_Drive  : Drive;
                      With_Speed : Angle.Signed) is
      pragma Unreferenced (With_Speed);
    begin
      Log.Write ("Mount.Adjust " & The_Drive'img);
    end Adjust;


    procedure Stop is
    begin
      Log.Write ("Mount.Stop");
      Action.Put (Mount_Action'(Stop));
    end Stop;


    function Actual_Direction return Space.Direction is
    begin
      Log.Write ("Mount.Actual_Direction Unknown");
      return Space.Unknown_Direction;
    end Actual_Direction;

  end Mount;

end Device;
