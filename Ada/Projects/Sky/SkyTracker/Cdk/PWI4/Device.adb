-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Cdk_700;
with Parameter;
with PWI4.Fans;
with PWI4.Focuser;
with PWI4.Mount;
with PWI4.M3;
with PWI4.Rotator;
with PWI4.Site;
with Satellite;
with System;
with Traces;

package body Device is

  package Log is new Traces ("Device");

  Completion_Duration : constant Duration := 22.0;
  Homing_Duration     : constant Duration := 45.0;

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
                        Stop_Rates,
                        Set_Moving,
                        Spiral_Offset_Center,
                        Spiral_Offset_Next,
                        Spiral_Offset_Previous,
                        Reset_Moving_Target);

  type Mount_Setup is (No_Setup,
                       Set_Az_Axis_Wrap,
                       Add_To_Model);

  type M3_Action is (No_Action,
                     Toggle);

  type Focuser_Action is (No_Action,
                          Connect,
                          Disconnect,
                          Find_Home,
                          Go_To,
                          Stop);

  type Rotator_Action is (No_Action,
                          Connect,
                          Disconnect,
                          Find_Home,
                          Goto_Field,
                          Goto_Mech,
                          Goto_Offset,
                          Stop);

  type Parameters is record
    From_J2000       : Boolean         := False;
    Alt              : PWI4.Degrees    := 0.0;
    Azm              : PWI4.Degrees    := 0.0;
    Ra               : PWI4.Hours      := 0.0;
    Dec              : PWI4.Degrees    := 0.0;
    Ra_J2000         : PWI4.Hours      := 0.0;
    Dec_J2000        : PWI4.Degrees    := 0.0;
    Arc_Seconds      : PWI4.Arc_Second := 0.0;
    Delta_Ra         : PWI4.Arc_Second := 0.0;
    Delta_Dec        : PWI4.Arc_Second := 0.0;
    Offset_Axis      : PWI4.Mount.Offset_Axis;
    Offset_Command   : PWI4.Mount.Offset_Command;
    Alt_Speed        : Speed := 0.0;
    Az_Speed         : Speed := 0.0;
    Wrap_Position    : PWI4.Degrees := 0.0;
    Focuser_Position : Microns      := 0.0;
    Rotator_Value    : PWI4.Degrees := 0.0;
    Name_Id          : Name.Id;
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

    procedure Set_Moving (Alt_Speed : Speed;
                          Az_Speed  : Speed);

    procedure Spiral_Offset_Center;

    procedure Spiral_Offset_Next;

    procedure Spiral_Offset_Previous;

    procedure Reset_Moving_Target;

    procedure Set_Gradual_Offsets (Delta_Ra  : PWI4.Arc_Second;
                                   Delta_Dec : PWI4.Arc_Second);

    procedure Position_To (Alt : PWI4.Degrees;
                           Azm : PWI4.Degrees);

    procedure Follow_Tle (Id : Name.Id);

    procedure Set_Az_Axis_Wrap (Range_Min : Degrees);

    procedure Add_To_Model (Ra_J2000  : PWI4.Hours;
                            Dec_J2000 : PWI4.Degrees);

    procedure Put (Item : M3_Action);

    procedure Put (Item : Focuser_Action);

    procedure Go_To (Position : Microns);

    procedure Put (Item : Rotator_Action);

    procedure Goto_Field (Position : Degrees);

    procedure Goto_Mech (Position : Degrees);

    procedure Goto_Offset (Distance : Degrees);

    procedure Finish;

    entry Get (New_Fan_Action     : out Fan_Action;
               New_Mount_Action   : out Mount_Action;
               New_Mount_Setup    : out Mount_Setup;
               New_M3_Action      : out M3_Action;
               New_Focuser_Action : out Focuser_Action;
               New_Rotator_Action : out Rotator_Action;
               Is_Termination     : out Boolean;
               New_Parameter      : out Parameters);

  private
    The_Fan_Action     : Fan_Action     := No_Action;
    The_Mount_Action   : Mount_Action   := No_Action;
    The_Mount_Setup    : Mount_Setup    := No_Setup;
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


    procedure Set_Moving (Alt_Speed : Speed;
                          Az_Speed  : Speed) is
    begin
      The_Mount_Action := Set_Moving;
      The_Parameter.Alt_Speed := Alt_Speed;
      The_Parameter.Az_Speed := Az_Speed;
      Is_Pending := True;
    end Set_Moving;


    procedure Spiral_Offset_Center is
    begin
      The_Mount_Action := Spiral_Offset_Center;
      Is_Pending := True;
    end Spiral_Offset_Center;


    procedure Spiral_Offset_Next is
    begin
      The_Mount_Action := Spiral_Offset_Next;
      Is_Pending := True;
    end Spiral_Offset_Next;


    procedure Spiral_Offset_Previous is
    begin
      The_Mount_Action := Spiral_Offset_Previous;
      Is_Pending := True;
    end Spiral_Offset_Previous;


    procedure Reset_Moving_Target is
    begin
      The_Mount_Action := Reset_Moving_Target;
      Is_Pending := True;
    end Reset_Moving_Target;


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


    procedure Set_Az_Axis_Wrap (Range_Min : Degrees) is
    begin
      The_Mount_Setup := Set_Az_Axis_Wrap;
      The_Parameter.Wrap_Position := Range_Min;
      Is_Pending := True;
    end Set_Az_Axis_Wrap;


    procedure Add_To_Model (Ra_J2000  : PWI4.Hours;
                            Dec_J2000 : PWI4.Degrees) is
    begin
      The_Mount_Setup := Add_To_Model;
      The_Parameter.Ra_J2000 := Ra_J2000;
      The_Parameter.Dec_J2000 := Dec_J2000;
      Is_Pending := True;
    end Add_To_Model;


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


    procedure Go_To (Position : Microns) is
    begin
      The_Focuser_Action := Go_To;
      The_Parameter.Focuser_Position := Position;
      Is_Pending := True;
    end Go_To;


    procedure Put (Item : Rotator_Action) is
    begin
      The_Rotator_Action := Item;
      Is_Pending := True;
    end Put;


    procedure Goto_Field (Position : Degrees) is
    begin
      The_Rotator_Action := Goto_Field;
      The_Parameter.Rotator_Value := Position;
      Is_Pending := True;
    end Goto_Field;


    procedure Goto_Mech (Position : Degrees) is
    begin
      The_Rotator_Action := Goto_Mech;
      The_Parameter.Rotator_Value := Position;
      Is_Pending := True;
    end Goto_Mech;


    procedure Goto_Offset (Distance : Degrees) is
    begin
      The_Rotator_Action := Goto_Offset;
      The_Parameter.Rotator_Value := Distance;
      Is_Pending := True;
    end Goto_Offset;


    procedure Finish is
    begin
      Is_Finishing := True;
      Is_Pending := True;
    end Finish;


    entry Get (New_Fan_Action     : out Fan_Action;
               New_Mount_Action   : out Mount_Action;
               New_Mount_Setup    : out Mount_Setup;
               New_M3_Action      : out M3_Action;
               New_Focuser_Action : out Focuser_Action;
               New_Rotator_Action : out Rotator_Action;
               Is_Termination     : out Boolean;
               New_Parameter      : out Parameters) when Is_Pending is
    begin
      New_Fan_Action := The_Fan_Action;
      New_Mount_Action := The_Mount_Action;
      New_Mount_Setup := The_Mount_Setup;
      New_M3_Action := The_M3_Action;
      New_Focuser_Action := The_Focuser_Action;
      New_Rotator_Action := The_Rotator_Action;
      The_Fan_Action := No_Action;
      The_Mount_Action := No_Action;
      The_Mount_Setup := No_Setup;
      The_M3_Action := No_Action;
      The_Focuser_Action := No_Action;
      The_Rotator_Action := No_Action;
      Is_Termination  := Is_Finishing;
      New_Parameter := The_Parameter;
      Is_Pending := False;
    end Get;

  end Action;


  protected Focuser_State is

    procedure Set (State : Focuser.State);

    function Actual return Focuser.State;

  private
    The_Focuser_State  : Focuser.State := Focuser.Unknown;
  end Focuser_State;


  protected body Focuser_State is

    procedure Set (State : Focuser.State) is
    begin
      The_Focuser_State := State;
    end Set;

    function Actual return Focuser.State is (The_Focuser_State);

  end Focuser_State;


  task type Control with Priority => System.Max_Priority is

    entry Start (Mount_State_Handler   : Mount.State_Handler_Access;
                 Focuser_State_Handler : Focuser.State_Handler_Access;
                 M3_Position_Handler   : M3.Position_Handler_Access);

  end Control;


  The_Control : access Control;

  Is_Simulation : Boolean := False;

  The_Simulated_Focuser_Position : Microns;

  type Rotator_Simulation_State is (Powerup, Homing, Stopped, Following, Offsetting, Moving, Positioning);

  Simulator_Rotator_Homing_Delta : constant Degrees := 18.0 / Degrees(PWI4.Request_Rate);

  The_Simulated_Rotator_State         : Rotator_Simulation_State;
  The_Simulated_Rotator_Mech_Position : Degrees := 180.0;
  The_Simulated_Rotator_Field_Angle   : Degrees := 150.0;

  task body Control is

    use type Mount.State;
    use type Focuser.State;
    use type M3.Position;
    use type PWI4.M3_Port;
    use type Microns;
    use type Degrees;

    procedure Follow_Tle (Tle_Name : String) is
      Tle : constant Satellite.Tle := Satellite.Tle_Of (Tle_Name);
    begin
      PWI4.Mount.Follow_Tle (Line_1 => Tle_Name,
                             Line_2 => Tle(1),
                             Line_3 => Tle(2));
    end Follow_Tle;

    The_Mount_State_Handler   : Mount.State_Handler_Access;
    The_Focuser_State_Handler : Focuser.State_Handler_Access;
    The_M3_Position_Handler   : M3.Position_Handler_Access;

    Is_Finishing       : Boolean := False;
    The_Fan_Action     : Fan_Action := No_Action;
    The_Mount_Action   : Mount_Action := No_Action;
    The_Mount_Setup    : Mount_Setup := No_Setup;
    The_M3_Action      : M3_Action := No_Action;
    The_Focuser_Action : Focuser_Action := No_Action;
    The_Rotator_Action : Rotator_Action := No_Action;
    The_Mount_State    : Mount.State := Mount.Unknown;
    Last_Mount_State   : Mount.State := Mount.Unknown;
    Last_Az_Position   : Degrees := 0.0;
    Last_Focuser_State : Focuser.State := Focuser.Unknown;
    The_M3_Position    : M3.Position := M3.Unknown;
    Last_M3_Position   : M3.Position := M3.Unknown;
    The_Parameter      : Parameters;

    Simulated_Mount_Connected : Boolean := False;

    The_Simulated_Focuser_Goto_Position : Microns;

    The_Simulated_Rotator_Goto_Position : Degrees := The_Simulated_Rotator_Mech_Position;

    function Simulate_Rotator_Positioning return Boolean is
    begin
      if abs(The_Simulated_Rotator_Mech_Position - The_Simulated_Rotator_Goto_Position) > 3.0 then
        The_Simulated_Rotator_Mech_Position := @ + (The_Simulated_Rotator_Goto_Position - @) / 2;
        return True;
      else
        The_Simulated_Rotator_Mech_Position := The_Simulated_Rotator_Goto_Position;
        return False;
      end if;
    end Simulate_Rotator_Positioning;

    The_Simulated_Rotator_Goto_Angle : Degrees := The_Simulated_Rotator_Field_Angle;

    function Simulate_Rotator_Moving return Boolean is
    begin
      if abs(The_Simulated_Rotator_Field_Angle - The_Simulated_Rotator_Goto_Angle) > 3.0 then
        The_Simulated_Rotator_Field_Angle := @ + (The_Simulated_Rotator_Goto_Angle - @) / 2;
        return True;
      else
        The_Simulated_Rotator_Field_Angle := The_Simulated_Rotator_Goto_Angle;
        return False;
      end if;
    end Simulate_Rotator_Moving;

    Simulated_M3_Position : M3.Position := M3.Ocular;

    Default_Focuser_Position : constant Microns := Microns(6500);

  begin -- Control
    if Focuser.Persistent_Position.Storage_Is_Empty then
      Focuser.Stored_Position := Default_Focuser_Position;
    end if;
    accept Start (Mount_State_Handler   : Mount.State_Handler_Access;
                  Focuser_State_Handler : Focuser.State_Handler_Access;
                  M3_Position_Handler   : M3.Position_Handler_Access)
    do
      The_Mount_State_Handler := Mount_State_Handler;
      The_Focuser_State_Handler := Focuser_State_Handler;
      The_M3_Position_Handler := M3_Position_Handler;
    end Start;
    Log.Write ("Control started");
    Is_Simulation := Cdk_700.Is_Simulated;
    if Is_Simulation then
      The_Simulated_Focuser_Position := Focuser.Stored_Position;
      The_Simulated_Focuser_Goto_Position := The_Simulated_Focuser_Position;
      if Cdk_700.Had_Powerup then
        Log.Write ("Simulated powerup");
        The_Simulated_Rotator_State := Powerup;
      else
        The_Simulated_Rotator_State := Stopped;
      end if;
    end if;
    loop
      select
        Action.Get (New_Fan_Action     => The_Fan_Action,
                    New_Mount_Action   => The_Mount_Action,
                    New_Mount_Setup    => The_Mount_Setup,
                    New_M3_Action      => The_M3_Action,
                    New_Focuser_Action => The_Focuser_Action,
                    New_Rotator_Action => The_Rotator_Action,
                    Is_Termination     => Is_Finishing,
                    New_Parameter      => The_Parameter);
        Log.Write ("Handle - Fan " & The_Fan_Action'img
                       & " - Mount " & The_Mount_Action'img
                       & " - Setup " & The_Mount_Setup'img
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
          if Is_Simulation then
            Log.Write ("Simulated fans on");
          else
            PWI4.Fans.Turn_On;
          end if;
        when Turn_Off =>
          if Is_Simulation then
            Log.Write ("Simulated fans off");
          else
            PWI4.Fans.Turn_Off;
          end if;
        end case;

        case The_Mount_Action is
        when No_Action =>
          null;
        when Stop =>
          The_Simulated_Rotator_State := Stopped;
          PWI4.Mount.Stop;
        when Connect =>
          Simulated_Mount_Connected := True;
          PWI4.Mount.Connect;
        when Disconnect =>
          Simulated_Mount_Connected := False;
          PWI4.Mount.Disconnect;
        when Enable =>
          PWI4.Mount.Enable;
        when Disable =>
          PWI4.Mount.Disable;
        when Find_Home =>
          PWI4.Mount.Find_Home;
          The_Mount_State := Mount.Homing;
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
        when Set_Moving =>
          PWI4.Mount.Set_Moving (Alt_Speed => The_Parameter.Alt_Speed,
                                 Az_Speed  => The_Parameter.Az_Speed);
        when Spiral_Offset_Center =>
          PWI4.Mount.Spiral_Offset_Center;
        when Spiral_Offset_Next =>
          PWI4.Mount.Spiral_Offset_Next;
        when Spiral_Offset_Previous =>
          PWI4.Mount.Spiral_Offset_Previous;
        when Reset_Moving_Target =>
          PWI4.Mount.Reset_Moving_Target;
        when Set_Gradual_Offsets =>
          PWI4.Mount.Set_Gradual_Offsets (Delta_Ra  => The_Parameter.Delta_Ra,
                                          Delta_Dec => The_Parameter.Delta_Dec);
        end case;

        case The_Mount_Setup is
        when No_Setup =>
          null;
        when Set_Az_Axis_Wrap =>
          PWI4.Mount.Set_Axis0_Wrap (Range_Min => The_Parameter.Wrap_Position);
        when Add_To_Model =>
          PWI4.Mount.Add_Point (Ra_J2000  => The_Parameter.Ra_J2000,
                                Dec_J2000 => The_Parameter.Dec_J2000);
        end case;

        case The_M3_Action is
        when No_Action =>
          null;
        when Toggle =>
          case The_M3_Position is
          when M3.Camera =>
            if Is_Simulation then
              Simulated_M3_Position := M3.Ocular;
            else
              PWI4.M3.Turn (To => Parameter.M3_Ocular_Port);
            end if;
          when M3.Ocular =>
            if Is_Simulation then
              Simulated_M3_Position := M3.Camera;
            else
              PWI4.M3.Turn (To => Parameter.M3_Camera_Port);
            end if;
          when others =>
            null;
          end case;
        end case;

        if Is_Simulation then
          case The_Focuser_Action is
          when No_Action =>
            null;
          when Connect =>
            Focuser_State.Set (Focuser.Connected);
            Log.Write ("Simulated focuser connect");
          when Disconnect =>
            Focuser_State.Set (Focuser.Disconnected);
            The_Simulated_Rotator_State := Stopped;
            Log.Write ("Simulated focuser disconnect");
          when Find_Home =>
            Log.Write ("Simulated focuser find_home");
            The_Simulated_Focuser_Position := 1000.0;
            The_Simulated_Focuser_Goto_Position := 1000.0;
          when Go_To =>
            The_Simulated_Focuser_Goto_Position := The_Parameter.Focuser_Position;
            Focuser_State.Set (Focuser.Moving);
            Log.Write ("Simulated focuser goto :" & The_Simulated_Focuser_Goto_Position'image);
          when Stop =>
            Focuser_State.Set (Focuser.Connected);
            Log.Write ("Simulated focuser stop");
          end case;
        else
          case The_Focuser_Action is
          when No_Action =>
            null;
          when Connect =>
            PWI4.Focuser.Connect;
          when Disconnect =>
            PWI4.Focuser.Disconnect;
          when Find_Home =>
            PWI4.Focuser.Find_Home;
          when Go_To =>
            PWI4.Focuser.Go_To (The_Parameter.Focuser_Position);
          when Stop =>
            PWI4.Focuser.Stop;
          end case;
        end if;

        if Is_Simulation then
          case The_Rotator_Action is
          when No_Action =>
            null;
          when Connect =>
            Log.Write ("Simulated rotator connect");
          when Disconnect =>
            The_Simulated_Rotator_State := Stopped;
            Log.Write ("Simulated rotator disconnect");
          when Find_Home =>
            Log.Write ("Simulated rotator find_home");
            The_Simulated_Rotator_Mech_Position := 180.0;
            The_Simulated_Rotator_Goto_Position := 0.0;
            The_Simulated_Rotator_State := Homing;
          when Goto_Mech =>
            The_Simulated_Rotator_Goto_Position := The_Parameter.Rotator_Value;
            The_Simulated_Rotator_State := Positioning;
            Log.Write ("Simulated Rotator goto_mech :" & The_Simulated_Rotator_Goto_Position'image);
          when Goto_Field =>
            The_Simulated_Rotator_Goto_Angle := The_Parameter.Rotator_Value;
            The_Simulated_Rotator_State := Moving;
            Log.Write ("Simulated Rotator goto_field :" & The_Simulated_Rotator_Goto_Angle'image);
          when Goto_Offset =>
            The_Simulated_Rotator_Goto_Angle := @ + The_Parameter.Rotator_Value;
            The_Simulated_Rotator_Goto_Position := @ + The_Parameter.Rotator_Value;
            The_Simulated_Rotator_State := Offsetting;
            Log.Write ("Simulated rotator goto_offset");
          when Stop =>
            Log.Write ("Simulated rotator stop");
            The_Simulated_Rotator_State := Stopped;
          end case;
        else
          case The_Rotator_Action is
          when No_Action =>
            null;
          when Connect =>
            PWI4.Rotator.Connect;
          when Disconnect =>
            PWI4.Rotator.Disconnect;
          when Find_Home =>
            PWI4.Rotator.Find_Home;
          when Goto_Mech =>
            PWI4.Rotator.Goto_Mech (The_Parameter.Rotator_Value);
          when Goto_Field =>
            PWI4.Rotator.Goto_Field (The_Parameter.Rotator_Value);
          when Goto_Offset =>
            PWI4.Rotator.Goto_Offset (The_Parameter.Rotator_Value);
          when Stop =>
            PWI4.Rotator.Stop;
          end case;
        end if;
      or
        delay 1.0 / PWI4.Request_Rate;
        if The_Mount_State /= Mount.Error then
          PWI4.Get_System;
          if Is_Simulation then
            if Focuser.Actual_State = Focuser.Moving then
              if abs(The_Simulated_Focuser_Position - The_Simulated_Focuser_Goto_Position) > 4.0 then
                The_Simulated_Focuser_Position := @ + (The_Simulated_Focuser_Goto_Position - @) / 2;
              elsif The_Simulated_Focuser_Position > The_Simulated_Focuser_Goto_Position then
                The_Simulated_Focuser_Position := @ - 1.0;
                if The_Simulated_Focuser_Position < The_Simulated_Focuser_Goto_Position then
                  The_Simulated_Focuser_Position := The_Simulated_Focuser_Goto_Position;
                end if;
              elsif The_Simulated_Focuser_Position < The_Simulated_Focuser_Goto_Position then
                The_Simulated_Focuser_Position := @ + 1.0;
                if The_Simulated_Focuser_Position > The_Simulated_Focuser_Goto_Position then
                  The_Simulated_Focuser_Position := The_Simulated_Focuser_Goto_Position;
                end if;
              else
                Focuser_State.Set (Focuser.Connected);
              end if;
            end if;
            case The_Simulated_Rotator_State is
            when Powerup =>
              The_Simulated_Rotator_Mech_Position := 180.0;
            when Homing =>
              The_Simulated_Rotator_Mech_Position := @ - Simulator_Rotator_Homing_Delta;
              if The_Simulated_Rotator_Mech_Position < Simulator_Rotator_Homing_Delta then
                The_Simulated_Rotator_Mech_Position := 0.0;
                The_Simulated_Rotator_State := Stopped;
              end if;
            when Positioning =>
              if not Simulate_Rotator_Positioning then
                The_Simulated_Rotator_State := Stopped;
              end if;
            when Moving =>
              if not Simulate_Rotator_Moving then
                The_Simulated_Rotator_State := Following;
              end if;
            when Offsetting =>
              if not (Simulate_Rotator_Positioning or Simulate_Rotator_Moving) then
                The_Simulated_Rotator_State := Following;
              end if;
            when Following =>
              case The_Mount_State is
              when Mount.Tracking =>
                The_Simulated_Rotator_Mech_Position := @ + 0.01;
                The_Simulated_Rotator_Field_Angle := @ + 0.01;
              when others =>
                null;
              end case;
            when Stopped =>
              null;
            end case;
          end if;
        end if;
      end select;

      declare
        Mount_Info : constant PWI4.Mount.Information := PWI4.Mount.Info;
      begin
        case Mount_Info.Status is
        when PWI4.Mount.Disconnected =>
          The_Mount_State := Mount.Disconnected;
        when PWI4.Mount.Connected =>
          The_Mount_State := Mount.Connected;
        when PWI4.Mount.Enabled =>
          if Is_Simulation then
            if Simulated_Mount_Connected then
              The_Mount_State := Mount.Connected;
              Simulated_Mount_Connected := False;
            else
              if Rotator.Is_Homed then
                The_Mount_State := Mount.Enabled;
              end if;
            end if;
          else
            if Rotator.Is_Homed then
              The_Mount_State := Mount.Enabled;
            end if;
          end if;
        when PWI4.Mount.Stopped =>
          if The_Mount_State /= Mount.Homing  or else not Rotator.Moving then
            The_Mount_State := Mount.Stopped;
          end if;
        when PWI4.Mount.Approaching =>
          The_Mount_State := Mount.Approaching;
        when PWI4.Mount.Tracking =>
          The_Mount_State := Mount.Tracking;
        when PWI4.Mount.Error =>
          The_Mount_State := Mount.Error;
        end case;
        if Last_Az_Position /= Mount_Info.Az_Axis.Position then
          Log.Write ("Alt Position: " & Image_Of (Mount_Info.Alt_Axis.Position));
          Log.Write ("Az Position : " & Image_Of (Mount_Info.Az_Axis.Position));
          Log.Write ("Wrap Minimum: " & Image_Of (Mount_Info.Wrap_Min));
          Last_Az_Position := Mount_Info.Az_Axis.Position;
        end if;
      end;

      if not Is_Simulation then
        if PWI4.Focuser.Exists then
          if PWI4.Focuser.Connected then
            if PWI4.Focuser.Moving then
              Focuser_State.Set (Focuser.Moving);
            else
              Focuser_State.Set (Focuser.Connected);
            end if;
          else
            Focuser_State.Set (Focuser.Disconnected);
          end if;
        else
          Focuser_State.Set (Focuser.Unknown);
        end if;
      end if;

      case PWI4.M3.Actual_Port is
      when PWI4.Unknown =>
        if Is_Simulation and then The_Mount_State > Mount.Enabled then
          The_M3_Position := Simulated_M3_Position;
        else
          The_M3_Position := M3.Unknown;
        end if;
      when PWI4.Between =>
        The_M3_Position := M3.Between;
      when others =>
        if PWI4.M3.Actual_Port = Parameter.M3_Camera_Port then
          The_M3_Position := M3.Camera;
        else
          The_M3_Position := M3.Ocular;
        end if;
      end case;

      if The_Mount_State /= Last_Mount_State then
        The_Mount_State_Handler (The_Mount_State);
        Last_Mount_State := The_Mount_State;
      end if;
      if Focuser_State.Actual /= Last_Focuser_State then
        Last_Focuser_State := Focuser_State.Actual;
        The_Focuser_State_Handler (Last_Focuser_State);
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


  procedure Start (Mount_State_Handler   : Mount.State_Handler_Access;
                   Focuser_State_Handler : Focuser.State_Handler_Access;
                   M3_Position_Handler   : M3.Position_Handler_Access) is
  begin
    if Cdk_700.Had_Powerup then
      PWI4.Mount.Set_Powerup (Enable_Delay => (if Cdk_700.Is_Simulated then 5.0 else 60.0)); -- seconds
    end if;
    The_Control := new Control;
    The_Control.Start (Mount_State_Handler   => Mount_State_Handler,
                       Focuser_State_Handler => Focuser_State_Handler,
                       M3_Position_Handler   => M3_Position_Handler);
  end Start;


  procedure Finalize is
  begin
    Action.Finish;
  end Finalize;


  function Site_Info return Standard.Site.Data is
    Data : constant PWI4.Site_Info := PWI4.Site.Info;
    use type Angle.Value;
  begin
    return (Latitude  => +Angle.Degrees(Data.Latitude),
            Longitude => +Angle.Degrees(Data.Longitude),
            Elevation => Integer(Data.Height));
  end Site_Info;


  function Site_Lmst return Time.Value is
    Data : constant PWI4.Site_Info := PWI4.Site.Info;
    use type Time.Value;
  begin
    return +Angle.Degrees(Data.Lmst * 15.0);
  end Site_Lmst;


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


    The_Az_Axis_Minimum : Degrees := PWI4.Undefined_Degrees;

    function Actual_Info return Information is
      Mount_Info : constant PWI4.Mount.Information := PWI4.Mount.Info;
      use type Angle.Value;
    begin
      The_Az_Axis_Minimum := Mount_Info.Az_Axis.Min_Position;
      return (J2000_Direction  => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra_J2000)),
                                                      Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec_J2000))),
              Actual_Direction => Space.Direction_Of (Ra  => Angle.Value'(+Angle.Hours(Mount_Info.Ra)),
                                                      Dec => Angle.Value'(+Angle.Degrees(Mount_Info.Dec))),
              Local_Direction  => Earth.Direction_Of (Az  => Angle.Value'(+Angle.Degrees(Mount_Info.Az)),
                                                      Alt => Angle.Value'(+Angle.Degrees(Mount_Info.Alt))),
              Az_Axis          => Mount_Info.Az_Axis,
              Alt_Axis         => Mount_Info.Alt_Axis,
              Model            => Mount_Info.Model);
    end Actual_Info;


    function Az_Axis_Minimum return Degrees is
      use type Degrees;
    begin
      if The_Az_Axis_Minimum = PWI4.Undefined_Degrees then
        raise Program_Error;
      end if;
      return The_Az_Axis_Minimum;
    end Az_Axis_Minimum;


    function Is_Inactive return Boolean is
    begin
      return not PWI4.Mount.Is_Updating;
    end Is_Inactive;


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


    procedure Set_Rate_Axis0 (Item : Speed) is
      use type Speed;
    begin
      Log.Write ("Mount.Set_Rate_Axis0 " & Image_Of (Item));
      Action.Set_Offset (PWI4.Mount.Axis0, PWI4.Mount.Set_Rate_Arcsec_Per_Sec, -Item); -- reverse direction
    end Set_Rate_Axis0;


    procedure Set_Rate_Axis1 (Item : Speed) is
    begin
      Log.Write ("Mount.Set_Rate_Axis1 " & Image_Of (Item));
      Action.Set_Offset (PWI4.Mount.Axis1, PWI4.Mount.Set_Rate_Arcsec_Per_Sec, Item);
    end Set_Rate_Axis1;


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


    procedure Set_Moving (Alt_Speed : Speed;
                          Az_Speed  : Speed) is
    begin
      Log.Write ("Mount.Set_Moving");
      Action.Set_Moving (Alt_Speed, Az_Speed);
    end Set_Moving;


    procedure Spiral_Offset_Center is
    begin
      Log.Write ("Mount.Spiral_Offset_Center");
      Action.Spiral_Offset_Center;
    end Spiral_Offset_Center;


    procedure Spiral_Offset_Next is
    begin
      Log.Write ("Mount.Spiral_Offset_Next");
      Action.Spiral_Offset_Next;
    end Spiral_Offset_Next;


    procedure Spiral_Offset_Previous is
    begin
      Log.Write ("Mount.Spiral_Offset_Previous");
      Action.Spiral_Offset_Previous;
    end Spiral_Offset_Previous;


    procedure Reset_Moving_Target is
    begin
      Log.Write ("Mount.Reset_Moving_Target");
      Action.Reset_Moving_Target;
    end Reset_Moving_Target;


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


    procedure Set_Az_Axis_Wrap (Range_Min : Degrees) is
    begin
      Log.Write ("Mount.Set_Az_Axis_Wrap " & Image_Of (Range_Min));
      Action.Set_Az_Axis_Wrap (Range_Min);
    end Set_Az_Axis_Wrap;


    procedure Add_To_Model (Direction : Space.Direction) is
      use type Angle.Degrees;
      use type Angle.Signed;
      use type Angle.Value;
    begin
      Log.Write ("Mount.Add_To_Model " & Image_Of (Direction));
      Action.Add_To_Model (Ra_J2000  => PWI4.Hours(Angle.Hours'(+Space.Ra_Of (Direction))),
                           Dec_J2000 => PWI4.Degrees(Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (Direction)))));
    end Add_To_Model;


    procedure Stop is
    begin
      Log.Write ("Mount.Stop");
      Action.Put (Mount_Action'(Stop));
    end Stop;

  end Mount;


  package body Focuser is

    function Exists return Boolean renames PWI4.Focuser.Exists;

    function Actual_State return State is (Focuser_State.Actual);

    function Actual_Position return Microns is
    begin
      if Is_Simulation then
        return The_Simulated_Focuser_Position;
      else
        return PWI4.Focuser.Actual_Position;
      end if;
    end Actual_Position;


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


    procedure Find_Home is
    begin
      Log.Write ("Focuser.Find_Home");
      Action.Put (Focuser_Action'(Find_Home));
    end Find_Home;


    procedure Go_To (The_Position : Microns) is
    begin
      Log.Write ("Focuser.Goto" & The_Position'image);
      Action.Go_To (The_Position);
    end Go_To;


    procedure Stop is
    begin
      Log.Write ("Focuser.Stop");
      Action.Put (Focuser_Action'(Stop));
    end Stop;

  end Focuser;


  package body Rotator is

    function Is_Homed return Boolean is
    begin
      if Is_Simulation then
        return The_Simulated_Rotator_State in Stopped;
      else
        return PWI4.Rotator.Is_Homed;
      end if;
    end Is_Homed;


    function Moving return Boolean is
    begin
      if Is_Simulation then
        return The_Simulated_Rotator_State not in Stopped;
      else
        return PWI4.Rotator.Moving;
      end if;
    end Moving;


    function Slewing return Boolean is
    begin
      if Is_Simulation then
        return The_Simulated_Rotator_State in Moving | Positioning | Offsetting;
      else
        return PWI4.Rotator.Slewing;
      end if;
    end Slewing;


    function Field_Angle return Degrees is
    begin
      if Is_Simulation then
        return The_Simulated_Rotator_Field_Angle;
      else
        return PWI4.Rotator.Field_Angle;
      end if;
    end Field_Angle;


    function Mech_Position return Degrees is
    begin
      if Is_Simulation then
        return The_Simulated_Rotator_Mech_Position;
      else
        return PWI4.Rotator.Mech_Position;
      end if;
    end Mech_Position;


    procedure Connect is
    begin
      Log.Write ("Rotator.Connect");
      Action.Put (Rotator_Action'(Connect));
    end Connect;


    procedure Disconnect is
    begin
      Log.Write ("Rotator.Disconnect");
      Action.Put (Rotator_Action'(Disconnect));
    end Disconnect;


    procedure Find_Home is
    begin
      Log.Write ("Rotator.Find_Home");
      Action.Put (Rotator_Action'(Find_Home));
    end Find_Home;


    procedure Goto_Field (The_Angle : Degrees) is
    begin
      Log.Write ("Rotator.Goto_Field_Angle" & The_Angle'image);
      Action.Goto_Field (The_Angle);
    end Goto_Field;


    procedure Goto_Mech (The_Position : Degrees) is
    begin
      Log.Write ("Rotator.Goto_Mech_Position" & The_Position'image);
      Action.Goto_Mech (The_Position);
    end Goto_Mech;


    procedure Go_To (The_Offset : Degrees) is
    begin
      Log.Write ("Rotator.Goto_Offset" & The_Offset'image);
      Action.Goto_Offset (The_Offset);
    end Go_To;


    procedure Stop is
    begin
      Log.Write ("Rotator.Stop");
      Action.Put (Rotator_Action'(Stop));
    end Stop;

  end Rotator;


  package body M3 is

    function Exists return Boolean renames PWI4.M3.Exists;

    procedure Rotate is
    begin
      if not Exists then
        Log.Warning ("M3 does not exist");
      end if;
      Action.Put (M3_Action'(Toggle));
    end Rotate;

    procedure Turn_To_Occular is
      use type PWI4.Port;
    begin
      if Exists and then PWI4.M3.Actual_Port = Parameter.M3_Camera_Port then
        Rotate;
      end if;
    end Turn_To_Occular;

  end M3;


  package body Fans is

    procedure Turn (To : State) is
    begin
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

end Device;
