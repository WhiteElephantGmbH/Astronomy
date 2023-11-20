-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;
with Parameter;
with PWI4.Fans;
with PWI4.Focuser;
with PWI4.Mount;
with PWI4.M3;
with PWI4.Rotator;
with Satellite;
with System;
with Traces;

package body Device is

  package Log is new Traces ("Device");

  Completion_Duration : constant Duration := 22.0;
  Homing_Duration     : constant Duration := 140.0;

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
                        Follow_Tle,
                        Goto_Target,
                        Goto_Mark,
                        Set_Gradual_Offsets,
                        Set_Offset,
                        Stop_Rates);

  type M3_Action is (No_Action,
                     Turn_To_Camera,
                     Turn_To_Ocular);

  type Focuser_Action is (No_Action,
                          Enable,
                          Disable);

  type Rotator_Action is (No_Action,
                          Enable,
                          Disable);

  type Parameters is record
    From_J2000     : Boolean         := False;
    Alt            : PWI4.Degrees    := 0.0;
    Azm            : PWI4.Degrees    := 0.0;
    Ra             : PWI4.Hours      := 0.0;
    Dec            : PWI4.Degrees    := 0.0;
    Arc_Seconds    : PWI4.Arc_Second := 0.0;
    Delta_Ra       : PWI4.Arc_Second := 0.0;
    Delta_Dec      : PWI4.Arc_Second := 0.0;
    Offset_Axis    : PWI4.Mount.Offset_Axis;
    Offset_Command : PWI4.Mount.Offset_Command;
    Name_Id        : Name.Id;
  end record;


  protected Action is

    procedure Put (Item : Fan_Action);

    procedure Put (Item : Mount_Action);

    procedure Slew_To (Ra         : PWI4.Hours;
                       Dec        : PWI4.Degrees;
                       From_J2000 : Boolean := False);

    procedure Set_Offset (Axis : PWI4.Mount.Offset_Axis;
                          Cmd  : PWI4.Mount.Offset_Command;
                          Item : PWI4.Arc_Second);

    procedure Stop_Rates;

    procedure Set_Gradual_Offsets (Delta_Ra  : PWI4.Arc_Second;
                                   Delta_Dec : PWI4.Arc_Second);

    procedure Position_To (Alt : PWI4.Degrees;
                           Azm : PWI4.Degrees);

    procedure Follow_Tle (Id : Name.Id);

    procedure Put (Item : M3_Action);

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


    procedure Slew_To (Ra         : PWI4.Hours;
                       Dec        : PWI4.Degrees;
                       From_J2000 : Boolean := False) is
    begin
      The_Mount_Action := Goto_Target;
      The_Parameter.Ra := Ra;
      The_Parameter.Dec := Dec;
      The_Parameter.From_J2000 := From_J2000;
      Is_Pending := True;
    end Slew_To;


    procedure Set_Offset (Axis : PWI4.Mount.Offset_Axis;
                          Cmd  : PWI4.Mount.Offset_Command;
                          Item : PWI4.Arc_Second) is
    begin
      The_Mount_Action := Set_Offset;
      The_Parameter.Offset_Axis := Axis;
      The_Parameter.Offset_Command := Cmd;
      The_Parameter.Arc_Seconds := Item;
      Is_Pending := True;
    end Set_Offset;


    procedure Stop_Rates is
    begin
      The_Mount_Action := Stop_Rates;
      Is_Pending := True;
    end Stop_Rates;

    procedure Set_Gradual_Offsets (Delta_Ra  : PWI4.Arc_Second;
                                   Delta_Dec : PWI4.Arc_Second) is
    begin
      if The_Mount_Action = No_Action then
        The_Mount_Action := Set_Gradual_Offsets;
        The_Parameter.Delta_Ra := Delta_Ra;
        The_Parameter.Delta_Dec := Delta_Dec;
        Is_Pending := True;
      end if;
    end Set_Gradual_Offsets;


    procedure Position_To (Alt : PWI4.Degrees;
                           Azm : PWI4.Degrees) is
    begin
      The_Mount_Action := Goto_Mark;
      The_Parameter.Alt := Alt;
      The_Parameter.Azm := Azm;
      Is_Pending := True;
    end Position_To;

    procedure Follow_Tle (Id : Name.Id) is
    begin
      The_Mount_Action := Follow_Tle;
      The_Parameter.Name_Id := Id;
      Is_Pending := True;
    end Follow_Tle;

    procedure Put (Item : M3_Action) is
    begin
      The_M3_Action := Item;
      Is_Pending := True;
    end Put;


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

    procedure Follow_Tle (Tle_Name : String) is
      Tle : constant Satellite.Tle := Satellite.Tle_Of (Tle_Name);
    begin
      PWI4.Mount.Follow_Tle (Line_1 => Tle_Name,
                             Line_2 => Tle(1),
                             Line_3 => Tle(2));
    end Follow_Tle;

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
    use type PWI4.M3_Port;

  begin
    accept Start (Fans_State_Handler  : Fans.State_Handler_Access;
                  Mount_State_Handler : Mount.State_Handler_Access;
                  M3_Position_Handler : M3.Position_Handler_Access)
    do
      The_Fans_State_Handler := Fans_State_Handler;
      The_Mount_State_Handler := Mount_State_Handler;
      The_M3_Position_Handler := M3_Position_Handler;
    end Start;
    Log.Write ("Control started");
    loop
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
          PWI4.Fans.Turn_On;
        when Turn_Off =>
          PWI4.Fans.Turn_Off;
        end case;

        case The_Mount_Action is
        when No_Action =>
          null;
        when Stop =>
          PWI4.Mount.Stop;
        when Connect =>
          PWI4.Mount.Connect;
        when Disconnect =>
          PWI4.Mount.Disconnect;
        when Enable =>
          PWI4.Mount.Enable;
        when Disable =>
          PWI4.Mount.Disable;
        when Find_Home =>
          PWI4.Mount.Find_Home;
        when Follow_Tle =>
          Follow_Tle (Name.Image_Of (The_Parameter.Name_Id));
        when Goto_Target =>
          PWI4.Mount.Goto_Ra_Dec (With_Ra    => The_Parameter.Ra,
                                  With_Dec   => The_Parameter.Dec,
                                  From_J2000 => The_Parameter.From_J2000);
          The_Mount_State := Mount.Approaching;
        when Goto_Mark =>
          PWI4.Mount.Goto_Alt_Az (Alt => The_Parameter.Alt,
                                  Az  => The_Parameter.Azm);
        when Set_Offset =>
          PWI4.Mount.Set_Offset (Axis    => The_Parameter.Offset_Axis,
                                 Command => The_Parameter.Offset_Command,
                                 Item    => The_Parameter.Arc_Seconds);
        when Stop_Rates =>
          PWI4.Mount.Stop_Rates;
        when Set_Gradual_Offsets =>
          PWI4.Mount.Set_Gradual_Offsets (Delta_Ra  => The_Parameter.Delta_Ra,
                                          Delta_Dec => The_Parameter.Delta_Dec);
        end case;

        case The_M3_Action is
        when No_Action =>
          null;
        when Turn_To_Ocular =>
          PWI4.M3.Turn (To => Parameter.M3_Ocular_Port);
        when Turn_To_Camera =>
          PWI4.M3.Turn (To => Parameter.M3_Camera_Port);
        end case;

        case The_Focuser_Action is
        when No_Action =>
          null;
        when Enable =>
          PWI4.Focuser.Enable;
        when Disable =>
          PWI4.Focuser.Disable;
        end case;

        case The_Rotator_Action is
        when No_Action =>
          null;
        when Enable =>
          PWI4.Rotator.Enable;
        when Disable =>
          PWI4.Rotator.Disable;
        end case;
      or
        delay 1.0;
        PWI4.Get_System;
      end select;
      The_Fans_State := Fans.State'val(Boolean'pos(PWI4.Fans.Turned_On));
      case PWI4.Mount.Info.Status is
      when PWI4.Mount.Disconnected =>
        The_Mount_State := Mount.Disconnected;
      when PWI4.Mount.Connected =>
        The_Mount_State := Mount.Connected;
      when PWI4.Mount.Enabled =>
        The_Mount_State := Mount.Enabled;
      when PWI4.Mount.Stopped =>
        The_Mount_State := Mount.Stopped;
      when PWI4.Mount.Approaching =>
        The_Mount_State := Mount.Approaching;
      when PWI4.Mount.Tracking =>
        The_Mount_State := Mount.Tracking;
      when PWI4.Mount.Error =>
        The_Mount_State := Mount.Error;
      end case;
      case PWI4.M3.Actual_Port is
      when PWI4.Unknown =>
        The_M3_Position := M3.Unknown;
      when PWI4.Between =>
        The_M3_Position := M3.Between;
      when others =>
        if PWI4.M3.Actual_Port = Parameter.M3_Camera_Port then
          The_M3_Position := M3.Camera;
        else
          The_M3_Position := M3.Ocular;
        end if;
      end case;
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
                   M3_Position_Handler : M3.Position_Handler_Access) is
  begin
    The_Control := new Control;
    The_Control.Start (Fans_State_Handler  => Fans_State_Handler,
                       Mount_State_Handler => Mount_State_Handler,
                       M3_Position_Handler => M3_Position_Handler);
  end Start;


  procedure Finalize is
  begin
    Action.Finish;
  end Finalize;


  package body Mount is

    The_Last_Ra_Offset  : PWI4.Arc_Second;
    The_Last_Dec_Offset : PWI4.Arc_Second;


    function Arc_Second_Of (Item : Angle.Value) return PWI4.Arc_Second is
      use type Angle.Signed;
      use type Angle.Degrees;
    begin
      return PWI4.Arc_Second(Angle.Degrees'(+Angle.Signed'(+Item)) * 3600.0);
    end Arc_Second_Of;


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
      Mount_Info : constant PWI4.Mount.Information := PWI4.Mount.Info;
      use type Angle.Value;
    begin
      return (J2000_Direction  => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra_J2000)),
                                                      Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec_J2000))),
              Actual_Direction => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra)),
                                                      Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec))),
              Local_Direction  => Earth.Direction_Of (Az  => Angle.Value'(+Angle.Degrees(Mount_Info.Az)),
                                                      Alt => Angle.Value'(+Angle.Degrees(Mount_Info.Alt))),
              Az_Axis          => Mount_Info.Az_Axis,
              Alt_Axis         => Mount_Info.Alt_Axis);
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
      use type Time.Ut;
    begin
      Log.Write ("Mount.Find_Home");
      Action.Put (Mount_Action'(Find_Home));
      Completion_Time := Time.Universal + Homing_Duration;
    end Find_Home;


    function At_Home return Boolean is
    begin
      return True;
    end At_Home;


    procedure Goto_Target (Direction       :     Space.Direction;
                           Completion_Time : out Time.Ut) is
      use type Angle.Degrees;
      use type Angle.Signed;
      use type Angle.Value;
      use type Time.Ut;
    begin
      Log.Write ("Mount.Goto_Target " & Image_Of (Direction));
      The_Last_Ra_Offset := 0.0;
      The_Last_Dec_Offset := 0.0;
      Action.Slew_To (Ra         => PWI4.Hours(Angle.Hours'(+Space.Ra_Of (Direction))),
                      Dec        => PWI4.Degrees(Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (Direction)))),
                      From_J2000 => False);
      Completion_Time := Time.Universal + Completion_Duration;
    end Goto_Target;


    procedure Update_Target (Offset : Space.Direction) is
      The_Offset : PWI4.Arc_Second;
      Delta_Ra   : PWI4.Arc_Second;
      Delta_Dec  : PWI4.Arc_Second;
      use type PWI4.Arc_Second;
    begin
      The_Offset := Arc_Second_Of (Space.Ra_Of (Offset));
      Delta_Ra := The_Offset - The_Last_Ra_Offset;
      The_Last_Ra_Offset := The_Offset;
      The_Offset := Arc_Second_Of (Space.Dec_Of (Offset));
      Delta_Dec := The_Offset - The_Last_Dec_Offset;
      The_Last_Dec_Offset := The_Offset;
      if Delta_Ra /= 0.0 or Delta_Dec /= 0.0 then
        Log.Write ("Mount.Update_Target - Delta_Ra: " & Image_Of (Delta_Ra) & " - Delta_Dec: " & Image_Of (Delta_Dec));
        Action.Set_Gradual_Offsets (Delta_Ra  => Delta_Ra,
                                    Delta_Dec => Delta_Dec);
      end if;
    end Update_Target;


    procedure Set_Rate_Dec (Item : Speed) is
    begin
      Log.Write ("Mount.Set_Rate_Dec " & Image_Of (Item));
      Action.Set_Offset (PWI4.Mount.Dec, PWI4.Mount.Set_Rate_Arcsec_Per_Sec, Item);
    end Set_Rate_Dec;


    procedure Set_Rate_Ra (Item : Speed) is
    begin
      Log.Write ("Mount.Set_Rate_Ra " & Image_Of (Item));
      Action.Set_Offset (PWI4.Mount.Ra, PWI4.Mount.Set_Rate_Arcsec_Per_Sec, Item);
    end Set_Rate_Ra;


    procedure Set_Rate_Path (Item : Speed) is
    begin
      Log.Write ("Mount.Set_Rate_Path " & Image_Of (Item));
      Action.Set_Offset (PWI4.Mount.Path, PWI4.Mount.Set_Rate_Arcsec_Per_Sec, Item);
    end Set_Rate_Path;


    procedure Set_Rate_Transverse (Item : Speed) is
    begin
      Log.Write ("Mount.Set_Rate_Transverse " & Image_Of (Item));
      Action.Set_Offset (PWI4.Mount.Transverse, PWI4.Mount.Set_Rate_Arcsec_Per_Sec, Item);
    end Set_Rate_Transverse;


    procedure Stop_Rate is
    begin
      Log.Write ("Mount.Stop_Rate");
      Action.Stop_Rates;
    end Stop_Rate;


    procedure Goto_Mark (Direction       :     Earth.Direction;
                         Completion_Time : out Time.Ut) is
      use type Angle.Value;
      use type Time.Ut;
    begin
      Log.Write ("Mount.Goto_Mark " & Image_Of (Direction));
      pragma Assert (Earth.Direction_Is_Known (Direction));
      Action.Position_To (Alt => PWI4.Degrees(Angle.Degrees'(+Earth.Alt_Of (Direction))),
                          Azm => PWI4.Degrees(Angle.Degrees'(+Earth.Az_Of (Direction))));
      Completion_Time := Time.Universal + Completion_Duration;
    end Goto_Mark;


    procedure Confirm_Goto is
    begin
      PWI4.Mount.Confirm_Goto;
    end Confirm_Goto;


    procedure Follow_Tle (Id : Name.Id) is
    begin
      Log.Write ("Mount.Follow_Tle " & Name.Image_Of (Id));
      Action.Follow_Tle (Id);
    end Follow_Tle;


    procedure Stop is
    begin
      Log.Write ("Mount.Stop");
      Action.Put (Mount_Action'(Stop));
    end Stop;

  end Mount;


  package body Focuser is

    function Exists return Boolean renames PWI4.Focuser.Exists;

    procedure Enable is
    begin
      if Exists then
        Log.Write ("Focuser.Enable");
        Action.Put (Focuser_Action'(Enable));
      else
        Log.Warning ("Focuser does not exist");
      end if;
    end Enable;


    procedure Disable is
    begin
      if Exists then
        Log.Write ("Focuser.Disable");
        Action.Put (Focuser_Action'(Disable));
      else
        Log.Warning ("Focuser does not exist");
      end if;
    end Disable;

  end Focuser;


  package body Rotator is

    function Exists return Boolean renames PWI4.Rotator.Exists;

    procedure Enable is
    begin
      if Exists then
        Log.Write ("Rotator.Enable");
        Action.Put (Rotator_Action'(Enable));
      else
        Log.Warning ("Rotator does not exist");
      end if;
    end Enable;


    procedure Disable is
    begin
      if Exists then
        Log.Write ("Rotator.Disable");
        Action.Put (Rotator_Action'(Disable));
      else
        Log.Warning ("Rotator does not exist");
      end if;
    end Disable;

  end Rotator;


  package body M3 is

    function Exists return Boolean renames PWI4.M3.Exists;

    procedure Turn (To : Place) is
    begin
      if Exists then
        case To is
        when Ocular =>
          Log.Write ("M3.Turn_To_Ocular");
          Action.Put (M3_Action'(Turn_To_Ocular));
        when Camera =>
          Log.Write ("M3.Turn_To_Camera");
          Action.Put (M3_Action'(Turn_To_Camera));
        end case;
      else
        Log.Warning ("M3 does not exist");
      end if;
    end Turn;

  end M3;


  package body Fans is

    function Exists return Boolean renames PWI4.Fans.Exists;

    procedure Turn (To : State) is
    begin
      if Exists then
        Log.Write ("Fans.Turn " & To'img);
        case To is
        when Off =>
          Action.Put (Turn_Off);
        when On =>
          Action.Put (Turn_On);
        end case;
      else
        Log.Warning ("Fans do not exist");
      end if;
    end Turn;

    procedure Turn_On_Or_Off is
    begin
      if Exists then
        Log.Write ("Fans.Turn_On_Or_Off");
        if Parameter.Turn_Fans_On then
          Action.Put (Turn_On);
        else
          Action.Put (Turn_Off);
        end if;
      else
        Log.Warning ("Fans do not exist");
      end if;
    end Turn_On_Or_Off;

  end Fans;

end Device;
