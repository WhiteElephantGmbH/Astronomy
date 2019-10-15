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
with PWI.Focuser;
with PWI.Mount;
with PWI.M3;
with PWI.Rotator;
with PWI.Settings;
with Traces;
with System;

package body Device is

  package Log is new Traces ("Device");

  type Fan_Action is (No_Action,
                      Turn_On,
                      Turn_Off);

  type Mount_Action is (No_Action,
                        Stop,
                        Connect,
                        Disconnect,
                        Enable,
                        Disable,
                        Find_Home,
                        Set_Pointing_Model,
                        Goto_Target,
                        Update_Target,
                        Goto_Mark,
                        Jog);

  type M3_Action is (No_Action,
                     Turn_To_Camera,
                     Turn_To_Ocular);

  type Focuser_Action is (No_Action,
                          Connect,
                          Disconnect,
                          Move);

  type Rotator_Action is (No_Action,
                          Find_Home,
                          Start,
                          Stop);

  type Parameters is record
    Ra         : PWI.Mount.Hours   := 0.0;
    Dec        : PWI.Mount.Degrees := 0.0;
    Ra_Rate    : PWI.Mount.Speed   := 0.0;
    Dec_Rate   : PWI.Mount.Speed   := 0.0;
    From_J2000 : Boolean           := False;
    Alt        : PWI.Mount.Degrees := 0.0;
    Azm        : PWI.Mount.Degrees := 0.0;
    Focuser    : Microns           := 0;
  end record;

  type Mode is (Undefined, Normal, Simulation);


  protected Action is

    procedure Set_Mode;

    function Is_Simulating return Boolean;

    procedure Put (Item : Fan_Action);

    procedure Put (Item : Mount_Action);

    procedure Move (Ra         : PWI.Mount.Hours;
                    Dec        : PWI.Mount.Degrees;
                    From_J2000 : Boolean := False);

    procedure Move (Ra         : PWI.Mount.Hours;
                    Dec        : PWI.Mount.Degrees;
                    Ra_Rate    : PWI.Mount.Speed;
                    Dec_Rate   : PWI.Mount.Speed;
                    From_J2000 : Boolean := False);

    procedure Move (Alt : PWI.Mount.Degrees;
                    Azm : PWI.Mount.Degrees);

    procedure Jog (Alt_Rate : PWI.Mount.Axis_Rate;
                   Azm_Rate : PWI.Mount.Axis_Rate);

    procedure Put (Item : M3_Action);

    procedure Set (Item : Microns);

    procedure Put (Item : Focuser_Action);

    procedure Put (Item : Rotator_Action);

    procedure Finish;

    entry Get (New_Fan_Action     : out Fan_Action;
               New_Mount_Action   : out Mount_Action;
               New_M3_Action      : out M3_Action;
               New_Focuser_Action : out Focuser_Action;
               New_Rotator_Action : out Rotator_Action;
               Is_Termination     : out Boolean;
               New_Parameter      : out Parameters);

  private
    The_Mode           : Mode           := Undefined;
    The_Fan_Action     : Fan_Action     := No_Action;
    The_Mount_Action   : Mount_Action   := No_Action;
    The_M3_Action      : M3_Action      := No_Action;
    The_Focuser_Action : Focuser_Action := No_Action;
    The_Rotator_Action : Rotator_Action := No_Action;
    Is_Finishing       : Boolean        := False;
    Is_Pending         : Boolean        := False;
    The_Parameter      : Parameters;
  end Action;


  protected body Action is

    procedure Set_Mode is
    begin
      if Parameter.Is_Simulation_Mode then
        The_Mode := Simulation;
      else
        The_Mode := Normal;
      end if;
    end Set_Mode;


    function Is_Simulating return Boolean is
    begin
      case The_Mode is
      when Normal =>
        return False;
      when Simulation =>
        return True;
      when Undefined =>
        raise Program_Error;
      end case;
    end Is_Simulating;


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


    procedure Move (Ra         : PWI.Mount.Hours;
                    Dec        : PWI.Mount.Degrees;
                    Ra_Rate    : PWI.Mount.Speed;
                    Dec_Rate   : PWI.Mount.Speed;
                    From_J2000 : Boolean := False) is
    begin
      The_Mount_Action := Update_Target;
      The_Parameter.Ra := Ra;
      The_Parameter.Dec := Dec;
      The_Parameter.Ra_Rate := Ra_Rate;
      The_Parameter.Dec_Rate := Dec_Rate;
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


    procedure Jog (Alt_Rate : PWI.Mount.Axis_Rate;
                   Azm_Rate : PWI.Mount.Axis_Rate) is
    begin
      The_Mount_Action := Jog;
      The_Parameter.Alt := Alt_Rate;
      The_Parameter.Azm := Azm_Rate;
      Is_Pending := True;
    end Jog;


    procedure Put (Item : M3_Action) is
    begin
      The_M3_Action := Item;
      Is_Pending := True;
    end Put;


    procedure Set (Item : Microns) is
    begin
      The_Parameter.Focuser := Item;
    end Set;


    procedure Put (Item : Focuser_Action) is
    begin
      The_Focuser_Action := Item;
      Is_Pending := True;
    end Put;


    procedure Put (Item : Rotator_Action) is
    begin
      The_Rotator_Action := Item;
      Is_Pending := True;
    end Put;


    procedure Finish is
    begin
      Is_Finishing := True;
      Is_Pending := True;
    end Finish;


    entry Get (New_Fan_Action     : out Fan_Action;
               New_Mount_Action   : out Mount_Action;
               New_M3_Action      : out M3_Action;
               New_Focuser_Action : out Focuser_Action;
               New_Rotator_Action : out Rotator_Action;
               Is_Termination     : out Boolean;
               New_Parameter      : out Parameters) when Is_Pending is
    begin
      New_Fan_Action := The_Fan_Action;
      New_Mount_Action := The_Mount_Action;
      New_M3_Action := The_M3_Action;
      New_Focuser_Action := The_Focuser_Action;
      New_Rotator_Action := The_Rotator_Action;
      The_Fan_Action := No_Action;
      The_Mount_Action := No_Action;
      The_M3_Action := No_Action;
      The_Focuser_Action := No_Action;
      The_Rotator_Action := No_Action;
      Is_Termination  := Is_Finishing;
      New_Parameter := The_Parameter;
      Is_Pending := False;
    end Get;

  end Action;


  task type Control with Priority => System.Max_Priority is

    entry Start (Fans_State_Handler  : Fans.State_Handler_Access;
                 Mount_State_Handler : Mount.State_Handler_Access;
                 M3_Position_Handler : M3.Position_Handler_Access);

  end Control;

  The_Control : access Control;

  task body Control is

    The_Fans_State_Handler  : Fans.State_Handler_Access;
    The_Mount_State_Handler : Mount.State_Handler_Access;
    The_M3_Position_Handler : M3.Position_Handler_Access;

    Is_Finishing       : Boolean := False;
    The_Fan_Action     : Fan_Action := No_Action;
    The_Mount_Action   : Mount_Action := No_Action;
    The_M3_Action      : M3_Action := No_Action;
    The_Focuser_Action : Focuser_Action := No_Action;
    The_Rotator_Action : Rotator_Action := No_Action;
    The_Fans_State     : Fans.State := Fans.Initial_State;
    Last_Fans_State    : Fans.State := Fans.Initial_State;
    The_Mount_State    : Mount.State := Mount.Unknown;
    Last_Mount_State   : Mount.State := Mount.Unknown;
    The_M3_Position    : M3.Position := M3.Unknown;
    Last_M3_Position   : M3.Position := M3.Unknown;
    The_Parameter      : Parameters;

    use type Fans.State;
    use type Mount.State;
    use type M3.Position;
    use type PWI.M3_Port;

  begin
    accept Start (Fans_State_Handler  : Fans.State_Handler_Access;
                  Mount_State_Handler : Mount.State_Handler_Access;
                  M3_Position_Handler : M3.Position_Handler_Access)
    do
      The_Fans_State_Handler := Fans_State_Handler;
      The_Mount_State_Handler := Mount_State_Handler;
      The_M3_Position_Handler := M3_Position_Handler;
    end Start;
    Log.Write ("Control started" & (if Action.Is_Simulating then " Simulation" else ""));
    if Action.Is_Simulating then
      PWI.Mount.Set_Simulation_Mode;
    end if;
    loop
      begin
        select
          Action.Get (New_Fan_Action     => The_Fan_Action,
                      New_Mount_Action   => The_Mount_Action,
                      New_M3_Action      => The_M3_Action,
                      New_Focuser_Action => The_Focuser_Action,
                      New_Rotator_Action => The_Rotator_Action,
                      Is_Termination     => Is_Finishing,
                      New_Parameter      => The_Parameter);
          Log.Write ("Handle - Fan " & The_Fan_Action'img
                         & " - Mount " & The_Mount_Action'img
                         & " - M3 " & The_M3_Action'img
                         & " - Focuser " & The_Focuser_Action'img
                         & " - Rotator " & The_Rotator_Action'img);
          if Is_Finishing then
            exit;
          end if;

          case The_Fan_Action is
          when No_Action =>
            null;
          when Turn_On =>
            if Action.Is_Simulating then
              The_Fans_State := Fans.On;
            else
              PWI.Fans.Turn_On;
            end if;
          when Turn_Off =>
            if Action.Is_Simulating then
              The_Fans_State := Fans.Off;
            else
              PWI.Fans.Turn_Off;
            end if;
          end case;

          case The_Mount_Action is
          when No_Action =>
            null;
          when Stop =>
            PWI.Mount.Stop;
            if Action.Is_Simulating then
              The_Mount_State := Mount.Stopped;
            end if;
          when Connect =>
            if Action.Is_Simulating then
              The_Mount_State := Mount.Connected;
            else
              PWI.Mount.Connect;
            end if;
          when Disconnect =>
            if Action.Is_Simulating then
              The_Mount_State := Mount.Disconnected;
            else
              PWI.Mount.Disconnect;
            end if;
          when Enable =>
            if Action.Is_Simulating then
              The_Mount_State := Mount.Enabled;
            else
              PWI.Mount.Enable;
            end if;
          when Disable =>
            if Action.Is_Simulating then
              The_Mount_State := Mount.Connected;
            else
              PWI.Mount.Disable;
            end if;
          when Find_Home =>
            if Action.Is_Simulating then
              The_Mount_State := Mount.Synchronised;
            else
              PWI.Mount.Find_Home;
            end if;
          when Set_Pointing_Model =>
            if Action.Is_Simulating then
              The_Mount_State := Mount.Stopped;
            else
              PWI.Mount.Set_Pointing_Model;
            end if;
          when Goto_Target =>
            PWI.Mount.Move (Ra         => The_Parameter.Ra,
                            Dec        => The_Parameter.Dec,
                            From_J2000 => The_Parameter.From_J2000);
            The_Mount_State := Mount.Approaching;
          when Update_Target =>
            PWI.Mount.Move (Ra         => The_Parameter.Ra,
                            Dec        => The_Parameter.Dec,
                            Ra_Rate    => The_Parameter.Ra_Rate,
                            Dec_Rate   => The_Parameter.Dec_Rate,
                            From_J2000 => The_Parameter.From_J2000);
          when Goto_Mark =>
            PWI.Mount.Move (Alt => The_Parameter.Alt,
                            Azm => The_Parameter.Azm);
          when Jog =>
            PWI.Mount.Jog (Alt_Rate => The_Parameter.Alt,
                           Azm_Rate => The_Parameter.Azm);
          end case;

          case The_M3_Action is
          when No_Action =>
            null;
          when Turn_To_Ocular =>
            if Action.Is_Simulating then
              The_M3_Position := M3.Ocular;
            else
              PWI.M3.Turn (To => Parameter.M3_Ocular_Port);
            end if;
          when Turn_To_Camera =>
            if Action.Is_Simulating then
              The_M3_Position := M3.Camera;
            else
              PWI.M3.Turn (To => Parameter.M3_Camera_Port);
            end if;
          end case;

          case The_Focuser_Action is
          when No_Action =>
            null;
          when Connect =>
            PWI.Focuser.Connect (Parameter.M3_Camera_Port);
          when Disconnect =>
            PWI.Focuser.Disconnect (Parameter.M3_Camera_Port);
          when Move =>
            PWI.Focuser.Move (Parameter.M3_Camera_Port, The_Parameter.Focuser);
          end case;

          case The_Rotator_Action is
          when No_Action =>
            null;
          when Find_Home =>
            PWI.Rotator.Find_Home (Parameter.M3_Camera_Port);
          when Start =>
            PWI.Rotator.Start;
          when Stop =>
            PWI.Rotator.Stop;
          end case;
        or
          delay 1.0;
          PWI.Get_System;
        end select;
        if Action.Is_Simulating then
          case PWI.Mount.Status is
          when PWI.Mount.Approaching =>
            The_Mount_State := Mount.Approaching;
          when PWI.Mount.Tracking =>
            The_Mount_State := Mount.Tracking;
          when PWI.Mount.Stopped =>
            if The_Mount_State in Mount.Approaching | Mount.Tracking  then
              The_Mount_State := Mount.Stopped;
            end if;
          when others =>
            if The_Mount_State = Mount.Unknown then
              The_Mount_State := Mount.Disconnected;
            end if;
          end case;
          if The_M3_Position = M3.Unknown then
            The_M3_Position := Parameter.M3_Default_Place;
          end if;
        else
          The_Fans_State := Fans.State'val(Boolean'pos(PWI.Fans.Turned_On));
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
          case PWI.M3.Actual_Port is
          when PWI.Unknown =>
            The_M3_Position := M3.Unknown;
          when PWI.Between =>
            The_M3_Position := M3.Between;
          when others =>
            if PWI.M3.Actual_Port = Parameter.M3_Camera_Port then
              The_M3_Position := M3.Camera;
            else
              The_M3_Position := M3.Ocular;
            end if;
          end case;
        end if;
      exception
      when PWI.No_Server =>
        The_Mount_State := Mount.Unknown;
        The_M3_Position := M3.Unknown;
      end;
      if The_Fans_State /= Last_Fans_State then
        The_Fans_State_Handler (The_Fans_State);
        Last_Fans_State := The_Fans_State;
      end if;
      if The_Mount_State /= Last_Mount_State then
        The_Mount_State_Handler (The_Mount_State);
        Last_Mount_State := The_Mount_State;
      end if;
      if The_M3_Position /= Last_M3_Position then
        The_M3_Position_Handler (The_M3_Position);
        Last_M3_Position := The_M3_Position;
      end if;
    end loop;
    Log.Write ("Control end");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Control;


  procedure Start (Fans_State_Handler  : Fans.State_Handler_Access;
                   Mount_State_Handler : Mount.State_Handler_Access;
                   M3_Position_Handler : M3.Position_Handler_Access;
                   Pointing_Model      : String) is
  begin
    Action.Set_Mode;
    PWI.Mount.Define_Pointing_Model (Pointing_Model);
    The_Control := new Control;
    The_Control.Start (Fans_State_Handler  => Fans_State_Handler,
                       Mount_State_Handler => Mount_State_Handler,
                       M3_Position_Handler => M3_Position_Handler);
  end Start;


  procedure Finalize is
  begin
    Action.Finish;
  end Finalize;


  function Image_Of (Item : Encoder_Degrees) return String is
    Image : String := Item'img;
  begin
    if Image(Image'first) = ' ' then
      Image(Image'first) := '+';
    end if;
    return Image & "°";
  end Image_Of;


  function Limits return Encoder_Limits is
  begin
    return (Azm_Lower_Goto => PWI.Settings.Lower_Azm_Goto_Limit,
            Azm_Upper_Goto => PWI.Settings.Upper_Azm_Goto_Limit,
            Alt_Lower_Goto => PWI.Settings.Lower_Alt_Goto_Limit,
            Alt_Upper_Goto => PWI.Settings.Upper_Alt_Goto_Limit);
  end Limits;


  package body Fans is

    procedure Turn (To : State) is
    begin
      Log.Write ("Fans.Turn " & To'img);
      case To is
      when Off =>
        Action.Put (Turn_Off);
      when On =>
        Action.Put (Turn_On);
      end case;
    end Turn;

    procedure Turn_On_Or_Off is
    begin
      Log.Write ("Fans.Turn_On_Or_Off");
      if Parameter.Turn_Fans_On then
        Action.Put (Turn_On);
      else
        Action.Put (Turn_Off);
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


    function Image_Of (The_Speed : Speed) return String is
    begin
      return " - RA_Rate:" & The_Speed(D1)'img & " - DEC_Rate: " & The_Speed(D2)'img;
    end Image_Of;


    function Actual_Encoder return Encoder_Data is
      Mount_Encoder : constant PWI.Encoder_Data := PWI.Mount.Encoder;
    begin
      return (Azm => Mount_Encoder.Azm,
              Alt => Mount_Encoder.Alt);
    end Actual_Encoder;


    function Actual_Info return Information is
      Mount_Info : constant PWI.Mount.Information := PWI.Mount.Info;
      use type Angle.Value;
    begin
      return (J2000_Direction  => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra_2000)),
                                                      Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec_2000))),
              Actual_Direction => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra)),
                                                      Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec))),
              Local_Direction  => Earth.Direction_Of (Az  => Angle.Value'(+Angle.Degrees(Mount_Info.Azm)),
                                                      Alt => Angle.Value'(+Angle.Degrees(Mount_Info.Alt))),
              Encoder          => (Azm => Mount_Info.Encoder.Azm,
                                   Alt => Mount_Info.Encoder.Alt));
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
      Completion_Time := Time.Universal + 140.0;
    end Find_Home;


    procedure Set_Pointing_Model is
    begin
      Log.Write ("Mount.Set_Pointing_Model");
      Action.Put (Mount_Action'(Set_Pointing_Model));
    end Set_Pointing_Model;


    procedure Goto_Target (Direction       :     Space.Direction;
                           With_Speed      :     Speed;
                           Completion_Time : out Time.Ut) is
      use type Angle.Degrees;
      use type Angle.Signed;
      use type Angle.Value;
    begin
      if With_Speed = (0, 0) then
        Log.Write ("Mount.Goto_Target " & Image_Of (Direction));
        Action.Move (Ra         => PWI.Mount.Hours(Angle.Hours'(+Space.Ra_Of (Direction))),
                     Dec        => PWI.Mount.Degrees(Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (Direction)))),
                     From_J2000 => False);
      else
        Log.Write ("Mount.Goto_Target " & Image_Of (Direction) & " " & Image_Of (With_Speed));
        Action.Move (Ra         => PWI.Mount.Hours(Angle.Hours'(+Space.Ra_Of (Direction))),
                     Dec        => PWI.Mount.Degrees(Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (Direction)))),
                     Ra_Rate    => PWI.Mount.Speed(Angle.Degrees'(+With_Speed(D1)) * 3600.0),
                     Dec_Rate   => PWI.Mount.Speed(Angle.Degrees'(+With_Speed(D2)) * 3600.0),
                     From_J2000 => False);
      end if;
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


    procedure Jog (Rate : Speed) is
      use type Angle.Signed;
      Alt_Rate : constant PWI.Mount.Axis_Rate := PWI.Mount.Axis_Rate(Angle.Degrees'(+Rate(D2)));
      Azm_Rate : constant PWI.Mount.Axis_Rate := PWI.Mount.Axis_Rate(Angle.Degrees'(+Rate(D1)));
    begin
      Log.Write ("Mount.Jog - Alt_Rate:" & Alt_Rate'img & " - Azm_Rate:" & Azm_Rate'img);
      Action.Jog (Alt_Rate => Alt_Rate,
                  Azm_Rate => Azm_Rate);
    end Jog;


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


  package body M3 is

    procedure Turn (To : Place) is
    begin
      case To is
      when Ocular =>
        Log.Write ("M3.Turn_To_Ocular");
        Action.Put (M3_Action'(Turn_To_Ocular));
      when Camera =>
        Log.Write ("M3.Turn_To_Camera");
        Action.Put (M3_Action'(Turn_To_Camera));
      end case;
    end Turn;

  end M3;


  package body Focuser is

    Position_Is_Set : Boolean := False;

    The_Simulated_Goto_Position   : Microns;
    The_Simulated_Actual_Position : Microns := 0;

    procedure Connect is
    begin
      Log.Write ("Focuser.Connect");
      Action.Put (Focuser_Action'(Connect));
    end Connect;


    procedure Disconnect is
    begin
      Log.Write ("Focuser.Disconnect");
      Action.Put (Focuser_Action'(Disconnect));
    end Disconnect;


    procedure Set (The_Position : Microns) is
    begin
      Log.Write ("Focuser.Set" & The_Position'img);
      Position_Is_Set := True;
      if Action.Is_Simulating then
        The_Simulated_Goto_Position := The_Position;
      else
        Action.Set (The_Position);
      end if;
    end Set;


    procedure Move is
    begin
      Log.Write ("Focuser.Move");
      if Position_Is_Set then
        if Action.Is_Simulating then
          The_Simulated_Actual_Position := The_Simulated_Goto_Position;
        else
          Action.Put (Focuser_Action'(Move));
        end if;
      else
        Log.Warning ("Focuser position undefined");
      end if;
    end Move;


    function Position return Microns is
    begin
      if Action.Is_Simulating then
        return The_Simulated_Actual_Position;
      else
        return PWI.Focuser.Position (On => Parameter.M3_Camera_Port);
      end if;
    end Position;

  end Focuser;


  package body Rotator is

    procedure Find_Home is
    begin
      Log.Write ("Rotator.Find_Home");
      if not Action.Is_Simulating then
        Action.Put (Rotator_Action'(Find_Home));
      end if;
    end Find_Home;


    procedure Start is
    begin
      Log.Write ("Rotator.Start");
      if not Action.Is_Simulating then
        Action.Put (Rotator_Action'(Start));
      end if;
    end Start;


    procedure Stop is
    begin
      Log.Write ("Rotator.Stop");
      if not Action.Is_Simulating then
        Action.Put (Rotator_Action'(Stop));
      end if;
    end Stop;

  end Rotator;


end Device;
