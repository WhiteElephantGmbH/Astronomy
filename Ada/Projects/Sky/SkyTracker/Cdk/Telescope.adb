-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Real_Time;
with Cwe;
with Objects;
with Parameter;
with Remote;
with System;
with Traces;

package body Telescope is

  package Log is new Traces ("Telescope");

  package Mount renames Device.Mount;

  task type Control_Task with Priority => System.Max_Priority is

    entry Start;

    entry Execute (The_Command : Command);

    entry Set (The_Orientation : Orientation);

    entry Halt;

    entry Startup;

    entry Shutdown;

    entry Follow (Tracking_Period : Time.Period);

    entry Back;

    entry Position_To (Landmark : Name.Id);

    entry New_Fans_State (New_State : Fans.State);

    entry New_Mount_State (New_State : Mount.State);

    entry New_M3_Position (New_Position : M3.Position);

    entry Get (The_Data : out Data);

    entry Close;

  end Control_Task;

  type Control_Access is access Control_Task;

  Control : Control_Access;


  procedure Fans_State_Handler (New_State : Fans.State) is
  begin
    if not Control'terminated then
      Control.New_Fans_State (New_State);
    end if;
  end Fans_State_Handler;


  procedure Mount_State_Handler (New_State : Mount.State) is
  begin
    if not Control'terminated then
      Control.New_Mount_State (New_State);
    end if;
  end Mount_State_Handler;


  procedure M3_Position_Handler (New_Position : M3.Position) is
  begin
    if not Control'terminated then
      Control.New_M3_Position (New_Position);
    end if;
  end M3_Position_Handler;


  Signal_Information_Update : Information_Update_Handler;

  procedure Start (Update_Handler : Information_Update_Handler) is
  begin
    Signal_Information_Update := Update_Handler;
    Control := new Control_Task;
    Control.Start;
  end Start;


  procedure Execute (The_Command : Command) is
  begin
    Control.Execute (The_Command);
  end Execute;


  procedure Set (The_Orientation  : Orientation) is
  begin
    Control.Set (The_Orientation);
  end Set;


  procedure Startup is
  begin
    Control.Startup;
  end Startup;


  procedure Shutdown is
  begin
    Control.Shutdown;
  end Shutdown;


  procedure Halt is
  begin
    Control.Halt;
  end Halt;


  procedure Back is
  begin
    Control.Back;
  end Back;


  procedure Follow (Tracking_Period : Time.Period) is
  begin
    Control.Follow (Tracking_Period);
  end Follow;


  procedure Position_To (Landmark : Name.Id) is
  begin
    Control.Position_To (Landmark);
  end Position_To;


  Next_Id            : Name.Id;
  Next_Get_Direction : Get_Space_Access;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id) is
  begin
    Next_Id := The_Id;
    Next_Get_Direction := Get_Direction;
  end Define_Space_Access;


  function Information return Data is
    The_Data : Data;
  begin
    Control.Get (The_Data);
    return The_Data;
  end Information;


  procedure Close is
  begin
    Control.Close;
  end Close;


  task body Control_Task is

    use type Device.Encoder_Degrees;

    Azm_Goto_Limit : constant Device.Encoder_Degrees := 10.0;
    Azm_Wrap_Limit : constant Device.Encoder_Degrees := Azm_Goto_Limit + 10.0;
    Homing_Limit   : constant Device.Encoder_Degrees := 180.0 + Azm_Goto_Limit;
    Homing_Time    : constant Duration := 18.0;

    The_Next_Tracking_Period : Time.Period := Time.Undefined;
    The_Completion_Time      : Time.Ut := Time.In_The_Past;
    The_Start_Time           : Time.Ut := Time.In_The_Past;
    The_Arrival_Time         : Time.Ut := Time.In_The_Future;
    The_Home_Direction       : Space.Direction;
    The_Waiting_Position     : Earth.Direction;
    Is_Fast_Tracking         : Boolean := False;

    The_Landmark : Name.Id;

    type Adjusting_Kind is (First_Adjusting, Second_Adjusting, Time_Adjusting);

    The_Adjusting_Kind        : Adjusting_Kind;
    The_Adjusting_Start_Time  : Time.Ut;
    The_Adjusting_End_Time    : Time.Ut;
    The_Time_Adjusting_Factor : Duration;
    The_Time_Adjustment       : Duration;
    First_Adjust_Factor       : Angle.Signed := 1;
    The_First_Moving_Speed    : Angle.Degrees := 0.0;
    Second_Adjust_Factor      : Angle.Signed := 1;
    The_Second_Moving_Speed   : Angle.Degrees := 0.0;
    The_Adjusted_Offset       : Earth.Direction;

    type Event is (No_Event,
                   Startup,
                   Shutdown,
                   Halt,
                   Follow,
                   Back,
                   Position,
                   User_Adjust,
                   User_Setup,
                   Mount_Unknown,
                   Mount_Disconnected,
                   Mount_Connected,
                   Mount_Enabled,
                   Mount_Homing,
                   Mount_Synchronised,
                   Mount_Stopped,
                   Mount_Approaching,
                   Mount_Tracking);

    subtype Mount_Startup is Event range Mount_Disconnected .. Mount_Synchronised;

    The_State  : State := Disconnected;
    The_Event  : Event := No_Event;

    Mount_Is_Stopped : Boolean := True;

    The_Fans_State : Fans.State := Fans.Initial_State;

    The_M3_Position : M3.Position := M3.Unknown;

    The_User_Adjust : Adjust;
    The_User_Setup  : Setup;

    Id            : Name.Id;
    Get_Direction : Get_Space_Access;

    The_Azm_Offset : Device.Encoder_Degrees;

    Target_Lost    : exception;
    Target_Is_Lost : Boolean := False;


    procedure Calculate_Azm_Offset is
      Info : constant Mount.Information := Mount.Actual_Info;
      use type Angle.Value;
      Azm  : constant Angle.Degrees := +Earth.Az_Of (Info.Local_Direction);
    begin
      The_Azm_Offset := Device.Encoder_Degrees(Azm) - Info.Encoder.Azm;
      while The_Azm_Offset > 180.0 loop
        The_Azm_Offset := The_Azm_Offset - 360.0;
      end loop;
      while The_Azm_Offset < -180.0 loop
        The_Azm_Offset := The_Azm_Offset + 360.0;
      end loop;
    end Calculate_Azm_Offset;


    function Target_Direction (At_Time : Time.Ut := Time.Universal) return Space.Direction is
      Adjusted_Time : constant Time.Ut := At_Time + The_Time_Adjustment;
      Direction     : constant Space.Direction := Get_Direction (Id, Adjusted_Time);
      The_Direction : Earth.Direction;
    begin
      if not Space.Direction_Is_Known (Direction) then
        Target_Is_Lost := True;
        raise Target_Lost;
      end if;
      if Earth.Direction_Is_Known (The_Adjusted_Offset) then
        The_Direction := Objects.Direction_Of (Direction, Time.Lmst_Of (Adjusted_Time));
        Earth.Add_To (The_Direction, The_Adjusted_Offset);
        return Objects.Direction_Of (The_Direction, Adjusted_Time);
      else
        return Direction;
      end if;
    end Target_Direction;


    function Target_Speed (At_Time : Time.Ut := Time.Universal) return Mount.Speed is
      Direction_Before : constant Space.Direction := Get_Direction (Id, At_Time - 0.5);
      Direction_After  : constant Space.Direction := Get_Direction (Id, At_Time + 0.5);
      Direction_Delta  : Space.Direction;
      Ra_Speed         : Angle.Signed;
      Dec_Speed        : Angle.Signed;
      use type Space.Direction;
      use type Angle.Signed;
    begin
      if Space.Direction_Is_Known (Direction_Before) and Space.Direction_Is_Known (Direction_After) then
        Direction_Delta := Direction_After - Direction_Before;
        Ra_Speed := +Space.Ra_Of (Direction_Delta);
        Dec_Speed := +Space.Dec_Of (Direction_Delta);
        return (Mount.D1 => Ra_Speed, Mount.D2 => Dec_Speed);
      else
        Target_Is_Lost := True;
        raise Target_Lost;
      end if;
    end Target_Speed;


    function Encoder_Within_Goto_Limits return Boolean is
      Encoder : constant Mount.Encoder_Data := Mount.Actual_Encoder;
      Limits  : constant Device.Encoder_Limits := Device.Limits;
    begin
     if Encoder.Azm > (Limits.Azm_Upper_Goto - Azm_Goto_Limit) or else
        Encoder.Azm < (Limits.Azm_Lower_Goto + Azm_Goto_Limit) or else
        Encoder.Alt > Limits.Alt_Upper_Goto or else
        Encoder.Alt < Limits.Alt_Lower_Goto
     then
       return False;
     end if;
     return True;
    end Encoder_Within_Goto_Limits;


    function Azm_Encoder_Position_Of (Item : Earth.Direction) return Device.Encoder_Degrees is
      use type Angle.Value;
    begin
      return Device.Encoder_Degrees(Angle.Degrees'(+Earth.Az_Of (Item))) - The_Azm_Offset;
    end Azm_Encoder_Position_Of;


    procedure Goto_Target is

      Encoder : constant Mount.Encoder_Data := Mount.Actual_Encoder;
      Limits  : constant Device.Encoder_Limits := Device.Limits;

      function Azm_Encoder_Goto_Position return Device.Encoder_Degrees is
        The_Direction : constant Earth.Direction := Objects.Direction_Of (Target_Direction (The_Start_Time),
                                                                          Time.Lmst_Of (The_Start_Time));
        Azm_Goto_Position : Device.Encoder_Degrees := Azm_Encoder_Position_Of (The_Direction);
        The_Delta         : Device.Encoder_Degrees;
      begin
        The_Delta := Encoder.Azm - Azm_Goto_Position;
        if The_Delta > 180.0 then
          Azm_Goto_Position := Azm_Goto_Position + 360.0;
          if Azm_Goto_Position > Limits.Azm_Upper_Goto then
            Azm_Goto_Position := Azm_Goto_Position - 360.0;
          end if;
        elsif The_Delta < -180.0 then
          Azm_Goto_Position := Azm_Goto_Position - 360.0;
          if Azm_Goto_Position < Limits.Azm_Lower_Goto then
            Azm_Goto_Position := Azm_Goto_Position + 360.0;
          end if;
        end if;
        return Azm_Goto_Position;
      end Azm_Encoder_Goto_Position;

      function Home_Direction return Space.Direction is
        The_Direction : Earth.Direction := Objects.Direction_Of (Target_Direction (The_Start_Time),
                                                                 Time.Lmst_Of (The_Start_Time));
        use type Angle.Value;
      begin
        The_Direction := Earth.Direction_Of (Az  => Angle.Semi_Circle + Angle.Degrees(The_Azm_Offset),
                                             Alt => Earth.Alt_Of (The_Direction));
        return Objects.Direction_Of (The_Direction, The_Start_Time);
      end Home_Direction;

    begin -- Goto_Target
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      The_Start_Time := Time.Universal;
      if Encoder.Azm > (Limits.Azm_Upper_Goto - Homing_Limit) or else
         Encoder.Azm < (Limits.Azm_Lower_Goto + Homing_Limit)
      then
        declare
          Encoder_Goto_Position : constant Device.Encoder_Degrees := Azm_Encoder_Goto_Position;
        begin
          if Encoder_Goto_Position > (Limits.Azm_Upper_Goto - Azm_Wrap_Limit) or else
             Encoder_Goto_Position < (Limits.Azm_Lower_Goto + Azm_Wrap_Limit) or else
             Encoder.Azm > (Limits.Azm_Upper_Goto - Azm_Wrap_Limit) or else
             Encoder.Azm < (Limits.Azm_Lower_Goto + Azm_Wrap_Limit)
          then
            The_Home_Direction := Home_Direction;
            Mount.Goto_Target (The_Home_Direction, (0, 0), The_Completion_Time);
            The_Completion_Time := The_Completion_Time + Homing_Time;
            The_State := Approaching;
            return;
          end if;
        end;
      end if;
      Mount.Goto_Target (Direction       => Target_Direction (At_Time => The_Start_Time),
                         With_Speed      => Target_Speed (At_Time => The_Start_Time),
                         Completion_Time => The_Completion_Time);
      The_State := Approaching;
    exception
    when Target_Lost =>
      null;
    end Goto_Target;


    procedure Reset_Adjustments is
    begin
      The_Adjusted_Offset := Cwe.Adjustment;
      The_Time_Adjustment := Parameter.Time_Adjustment;
      The_Adjusting_Start_Time := Time.In_The_Future;
      The_Adjusting_End_Time := Time.In_The_Future;
    end Reset_Adjustments;


    procedure Stop_Target is
    begin
      The_Home_Direction := Space.Unknown_Direction;
      Mount.Stop;
      if Mount_Is_Stopped then
        The_State := Stopped;
      else
        The_State := Stopping;
      end if;
      Reset_Adjustments;
    end Stop_Target;


    procedure Back_To_Target is
      Unused : Time.Ut;
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      if Encoder_Within_Goto_Limits then
        The_Start_Time := Time.Universal;
        Mount.Goto_Target (Direction       => Target_Direction (At_Time => The_Start_Time),
                           With_Speed      => Target_Speed (At_Time => The_Start_Time),
                           Completion_Time => Unused);
      else
        Stop_Target;
      end if;
    exception
    when Target_Lost =>
      Stop_Target;
    end Back_To_Target;


    function Goto_Waiting_Position return Boolean is

      use type Angle.Value;

      procedure Goto_Home_Or_Waiting is

        Limits      : constant Device.Encoder_Limits := Device.Limits;
        Lower_Limit : constant Device.Encoder_Degrees := Limits.Azm_Lower_Goto + Azm_Goto_Limit;
        Upper_Limit : constant Device.Encoder_Degrees := Limits.Azm_Upper_Goto - Azm_Goto_Limit;

        Arrival_Position : constant Earth.Direction := Objects.Direction_Of (Get_Direction (Id, The_Arrival_Time),
                                                                             Time.Lmst_Of (The_Arrival_Time));
        Leaving_Time     : constant Time.Ut := The_Next_Tracking_Period.Leaving_Time;
        Leaving_Position : constant Earth.Direction := Objects.Direction_Of (Get_Direction (Id, Leaving_Time),
                                                                             Time.Lmst_Of (Leaving_Time));
        Current_Azm_Encoder_Position : constant Device.Encoder_Degrees := Mount.Actual_Encoder.Azm;
        Arrival_Azm_Encoder_Position : Device.Encoder_Degrees := Azm_Encoder_Position_Of (Arrival_Position);
        Leaving_Azm_Encoder_Position : Device.Encoder_Degrees := Azm_Encoder_Position_Of (Leaving_Position);
        Travel_Distance : Device.Encoder_Degrees := Leaving_Azm_Encoder_Position - Arrival_Azm_Encoder_Position;

        Home_Position : Earth.Direction;

      begin -- Goto_Home_Or_Waiting
        if Travel_Distance < -180.0 then
          Travel_Distance := Travel_Distance + 360.0;
        elsif Travel_Distance > 180.0 then
          Travel_Distance := Travel_Distance - 360.0;
        end if;
        Leaving_Azm_Encoder_Position := Arrival_Azm_Encoder_Position + Travel_Distance;
        if Leaving_Azm_Encoder_Position > Upper_Limit then
          Arrival_Azm_Encoder_Position := Arrival_Azm_Encoder_Position - 360.0;
          Leaving_Azm_Encoder_Position := Leaving_Azm_Encoder_Position - 360.0;
        elsif Leaving_Azm_Encoder_Position < Lower_Limit then
          Arrival_Azm_Encoder_Position := Arrival_Azm_Encoder_Position + 360.0;
          Leaving_Azm_Encoder_Position := Leaving_Azm_Encoder_Position + 360.0;
        elsif abs (Arrival_Azm_Encoder_Position - Current_Azm_Encoder_Position) > 180.0 then
          if Arrival_Azm_Encoder_Position > Current_Azm_Encoder_Position then
            if (Leaving_Azm_Encoder_Position > Lower_Limit + 360.0) and
               (Arrival_Azm_Encoder_Position > Lower_Limit + 360.0)
            then
              Arrival_Azm_Encoder_Position := Arrival_Azm_Encoder_Position - 360.0;
              Leaving_Azm_Encoder_Position := Leaving_Azm_Encoder_Position - 360.0;
            end if;
          else
            if (Arrival_Azm_Encoder_Position < Upper_Limit - 360.0) and
               (Leaving_Azm_Encoder_Position < Upper_Limit - 360.0)
            then
              Arrival_Azm_Encoder_Position := Arrival_Azm_Encoder_Position + 360.0;
              Leaving_Azm_Encoder_Position := Leaving_Azm_Encoder_Position + 360.0;
            end if;
          end if;
        end if;
        The_Start_Time := Time.Universal;
        if abs (Arrival_Azm_Encoder_Position - Current_Azm_Encoder_Position) > 180.0 then
          Home_Position := Earth.Direction_Of (Az  => Angle.Semi_Circle + Angle.Degrees(The_Azm_Offset),
                                               Alt => Earth.Alt_Of (Arrival_Position));
          The_Waiting_Position := Arrival_Position;
          Mount.Goto_Mark (Home_Position, The_Completion_Time);
          The_Completion_Time := The_Completion_Time + Homing_Time;
        else
          The_Waiting_Position := Earth.Unknown_Direction;
          Mount.Goto_Mark (Arrival_Position, The_Completion_Time);
        end if;
      end Goto_Home_Or_Waiting;

    begin -- Goto_Waiting_Position
      The_Arrival_Time := The_Next_Tracking_Period.Arrival_Time;
      if (The_Arrival_Time - Time.Universal) < Homing_Time then
        return False;
      end if;
      Goto_Home_Or_Waiting;
      The_State := Preparing;
      return True;
    end Goto_Waiting_Position;


    Moving_Speeds : constant Angle.Values := Parameter.Moving_Speeds;
    Moving_Index  : Integer := Moving_Speeds'first + 1;

    procedure Set_Moving_Speed (Index : Integer) is
    begin
      Moving_Index := Index;
      if Moving_Index < Moving_Speeds'first then
        Moving_Index := Moving_Speeds'first;
      elsif Moving_Index > Moving_Speeds'last then
        Moving_Index := Moving_Speeds'last;
      else
        Log.Write ("set moving speed => " & Angle.Image_Of (Moving_Speeds(Moving_Index), Decimals => 3));
      end if;
    end Set_Moving_Speed;


    procedure Change_Moving_Speed (Increment : Integer) is
    begin
      Set_Moving_Speed (Moving_Index + Increment);
    end Change_Moving_Speed;


    procedure Follow_New_Target is
      use type Time.Period;
    begin
      Id := Next_Id;
      Get_Direction := Next_Get_Direction;
      Is_Fast_Tracking := The_Next_Tracking_Period /= Time.Undefined;
      if Is_Fast_Tracking then
        Log.Write ("follow from " & The_State'img & " after " & Time.Image_Of (The_Next_Tracking_Period.Arrival_Time)
                                                  & " until " & Time.Image_Of (The_Next_Tracking_Period.Leaving_Time));
        if not Goto_Waiting_Position then -- allready arrived
          Goto_Target;
        end if;
      else
        Log.Write ("follow from " & The_State'img);
        Goto_Target;
      end if;
    end Follow_New_Target;


    Time_Adjusting_Factor : constant Duration := 0.05;

    procedure Increment_Offset is
      The_Adjusting_Time : Time.Ut;
      use type Angle.Degrees;
    begin
      if The_Adjusting_Start_Time /= Time.In_The_Future then
        if The_Adjusting_End_Time = Time.In_The_Future then
          The_Adjusting_Time := Time.Universal - The_Adjusting_Start_Time;
          The_Adjusting_Start_Time := The_Adjusting_Start_Time + The_Adjusting_Time;
        else
          The_Adjusting_Time := The_Adjusting_End_Time - The_Adjusting_Start_Time;
          The_Adjusting_Start_Time := Time.In_The_Future;
          The_Adjusting_End_Time := Time.In_The_Future;
        end if;
        case The_Adjusting_Kind is
        when First_Adjusting =>
          Earth.Add_Az_To (The_Adjusted_Offset, The_First_Moving_Speed * Angle.Degrees(The_Adjusting_Time));
        when Second_Adjusting =>
          Earth.Add_Alt_To (The_Adjusted_Offset, The_Second_Moving_Speed * Angle.Degrees(The_Adjusting_Time));
        when Time_Adjusting =>
          The_Time_Adjustment := The_Time_Adjustment + (The_Adjusting_Time * The_Time_Adjusting_Factor);
        end case;
      end if;
    end Increment_Offset;


    procedure Update_Target_Position is
      Now    : constant Time.Ut := Time.Universal;
      Unused : Time.Ut;
    begin
      if Get_Direction /= null then
        if Space.Direction_Is_Known (The_Home_Direction) then
          if Now - The_Start_Time < Homing_Time then
            return;
          else
            The_Home_Direction := Space.Unknown_Direction;
          end if;
        end if;
        if Encoder_Within_Goto_Limits then
          Increment_Offset;
          Mount.Goto_Target (Direction       => Target_Direction (At_Time => Now),
                             With_Speed      => Target_Speed (At_Time => Now),
                             Completion_Time => Unused);
        else
          Stop_Target;
        end if;
      end if;
    exception
    when Target_Lost =>
      Stop_Target;
    end Update_Target_Position;


    procedure Update_Preparing_Position is
      Now    : constant Time.Ut := Time.Universal;
      Unused : Time.Ut;
    begin
      if Earth.Direction_Is_Known (The_Waiting_Position) then
        if Now - The_Start_Time < Homing_Time then
          return;
        else
          The_Start_Time := Time.Universal;
          Mount.Goto_Mark (The_Waiting_Position, The_Completion_Time);
          The_Waiting_Position := Earth.Unknown_Direction;
        end if;
      end if;
    exception
    when Target_Lost =>
      Stop_Target;
    end Update_Preparing_Position;


    procedure Do_Position is
    begin
      The_Start_Time := Time.Universal;
      Mount.Goto_Mark (Name.Direction_Of (The_Landmark), The_Completion_Time);
      Log.Write ("position to Landmark");
      The_State := Positioning;
    end Do_Position;


    procedure Start_Adjusting (Kind : Adjusting_Kind) is
    begin
      The_Adjusting_Kind := Kind;
      The_Adjusting_Start_Time := Time.Universal;
      The_Adjusting_End_Time := Time.In_The_Future;
    end Start_Adjusting;


    procedure Adjust_First (The_Speed : Angle.Signed) is
      use type Angle.Signed;
    begin
      The_First_Moving_Speed := +(The_Speed * First_Adjust_Factor);
      Start_Adjusting (First_Adjusting);
    end Adjust_First;


    procedure Adjust_Second (The_Speed : Angle.Signed) is
      use type Angle.Signed;
    begin
      The_Second_Moving_Speed := +(The_Speed * Second_Adjust_Factor);
      Start_Adjusting (Second_Adjusting);
    end Adjust_Second;


    procedure Adjust_Time (Factor : Duration) is
    begin
      The_Time_Adjusting_Factor := Factor;
      Start_Adjusting (Time_Adjusting);
    end Adjust_Time;


    procedure End_Adjust is
    begin
      The_Adjusting_End_Time := Time.Universal;
    end End_Adjust;


    procedure Adjust_Handling is
      Speed : constant Angle.Value := Moving_Speeds(Moving_Index);
      use type Angle.Signed;
    begin
      case The_User_Adjust is
      when Move_Left =>
        Adjust_First (-Speed);
      when Move_Right =>
        Adjust_First (+Speed);
      when Move_Up =>
        Adjust_Second (+Speed);
      when Move_Down =>
        Adjust_Second (-Speed);
      when End_Move =>
        End_Adjust;
      when Decrease_Time =>
        Adjust_Time (-Time_Adjusting_Factor);
      when Increase_Time =>
        Adjust_Time (+Time_Adjusting_Factor);
      when End_Change =>
        End_Adjust;
      end case;
    end Adjust_Handling;


    procedure Jog_Handling is
      use type Angle.Signed;
      Speed : constant Angle.Signed := +Moving_Speeds(Moving_Index);
    begin
      case The_User_Adjust is
      when Move_Left =>
        Mount.Jog ((Mount.D1 => -Speed, Mount.D2 => 0));
      when Move_Right =>
        Mount.Jog ((Mount.D1 => +Speed, Mount.D2 => 0));
      when Move_Up =>
        Mount.Jog ((Mount.D1 => 0, Mount.D2 => +Speed));
      when Move_Down =>
        Mount.Jog ((Mount.D1 => 0, Mount.D2 => -Speed));
      when End_Move =>
        Mount.Jog ((0, 0));
      when Decrease_Time =>
        null;
      when Increase_Time =>
        null;
      when End_Change =>
        null;
      end case;
    end Jog_Handling;


    procedure Setup_Handling is
    begin
      case The_User_Setup is
      when Decrease_Speed =>
        Change_Moving_Speed (-1);
      when Increase_Speed =>
        Change_Moving_Speed (+1);
      when Set_Guiding_Rate =>
        Set_Moving_Speed (Moving_Speeds'first);
      when Set_Centering_Rate =>
        Set_Moving_Speed (Moving_Speeds'first + 1);
      when Set_Finding_Rate =>
        Set_Moving_Speed (Moving_Speeds'last - 1);
      when Set_Slewing_Rate =>
        Set_Moving_Speed (Moving_Speeds'last);
      end case;
    end Setup_Handling;


  --=============
  --==  States ==
  --=============

    procedure Set_Tracking is
    begin
      if Is_Fast_Tracking then
        The_State := Following;
      else
        The_State := Tracking;
      end if;
    end Set_Tracking;


    function Mount_Startup_State (The_Startup_Event : Mount_Startup) return State is
    begin
      case The_Startup_Event is
      when Mount_Disconnected =>
        return Disconnected;
      when Mount_Connected =>
        return Connected;
      when Mount_Enabled =>
        return Enabled;
      when Mount_Homing =>
        return Homing;
      when Mount_Synchronised =>
        return Synchronised;
      end case;
    end Mount_Startup_State;

    -------------
    -- Unknown --
    -------------
    procedure Unknown_State is
    begin
      case The_Event is
      when Mount_Unknown =>
        null;
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        Set_Tracking;
      when others =>
        Mount.Stop;
        The_State := Disconnected;
      end case;
    end Unknown_State;

    ------------------
    -- Disconnected --
    ------------------
    procedure Disconnected_State is
    begin
      case The_Event is
      when Startup =>
        Mount.Connect;
        Focuser.Connect;
        The_State := Connecting;
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        Set_Tracking;
      when others =>
        Mount.Stop;
      end case;
    end Disconnected_State;

    -------------------
    -- Disconnecting --
    -------------------
    procedure Disconnecting_State is
    begin
      case The_Event is
      when Mount_Disconnected =>
        The_State := Disconnected;
      when Mount_Connected =>
        Mount.Disconnect;
      when Halt =>
        The_State := Connected;
      when others =>
        null;
      end case;
    end Disconnecting_State;

    ----------------
    -- Connecting --
    ----------------
    procedure Connecting_State is
    begin
      case The_Event is
      when Mount_Connected =>
        Mount.Enable;
        The_State := Enabling;
      when Mount_Enabled =>
        M3.Turn (To => Parameter.M3_Default_Place);
        Mount.Find_Home (The_Completion_Time);
        Rotator.Find_Home;
        The_State := Homing;
      when Mount_Synchronised =>
        Mount.Set_Pointing_Model;
        The_State := Initializing;
      when Mount_Stopped =>
        The_State := Stopped;
      when Halt =>
        The_State := Disconnected;
      when others =>
        null;
      end case;
    end Connecting_State;

    ---------------
    -- Connected --
    ---------------
    procedure Connected_State is
    begin
      case The_Event is
      when Mount_Disconnected =>
        The_State := Disconnected;
      when Startup =>
        Mount.Enable;
        The_State := Enabling;
      when Shutdown =>
        Mount.Disconnect;
        The_State := Disconnecting;
      when others =>
        null;
      end case;
    end Connected_State;

    ---------------
    -- Disabling --
    ---------------
    procedure Disabling_State is
    begin
      case The_Event is
      when Mount_Disconnected =>
        The_State := Disconnected;
      when Mount_Connected =>
        Mount.Disconnect;
        The_State := Disconnecting;
      when Halt =>
        The_State := Enabled;
      when others =>
        null;
      end case;
    end Disabling_State;

    -----------------
    -- Enablinging --
    -----------------
    procedure Enabling_State is
    begin
      case The_Event is
      when Mount_Enabled =>
        Fans.Turn (To => Fans.Off);
        M3.Turn (To => Parameter.M3_Default_Place);
        Mount.Find_Home (The_Completion_Time);
        Rotator.Find_Home;
        The_State := Homing;
      when Mount_Homing =>
        The_State := Homing;
      when Mount_Synchronised =>
        Mount.Set_Pointing_Model;
        The_State := Initializing;
      when Mount_Stopped =>
        The_State := Stopped;
      when Halt =>
        The_State := Connected;
      when others =>
        null;
      end case;
    end Enabling_State;

    -------------
    -- Enabled --
    -------------
    procedure Enabled_State is
    begin
      case The_Event is
      when Mount_Disconnected =>
        The_State := Disconnected;
      when Mount_Connected =>
        The_State := Connected;
      when Startup =>
        M3.Turn (To => Parameter.M3_Default_Place);
        Mount.Find_Home (The_Completion_Time);
        Rotator.Find_Home;
        The_State := Homing;
      when Shutdown =>
        Fans.Turn_On_Or_Off;
        Mount.Disable;
        The_State := Disabling;
      when others =>
        null;
      end case;
    end Enabled_State;

    ------------
    -- Homing --
    ------------
    procedure Homing_State is
      use type M3.Position;
    begin
      case The_Event is
      when Mount_Synchronised =>
        if The_M3_Position = Parameter.M3_Default_Place then
          Mount.Set_Pointing_Model;
          The_State := Initializing;
        end if;
      when Mount_Stopped =>
        The_State := Stopped;
      when Halt =>
        The_State := Enabled;
      when others =>
        null;
      end case;
    end Homing_State;

    ------------------
    -- Synchronised --
    ------------------
    procedure Synchronised_State is
    begin
      case The_Event is
      when Mount_Disconnected =>
        The_State := Disconnected;
      when Mount_Connected =>
        The_State := Connected;
      when Mount_Enabled =>
        The_State := Enabled;
      when Startup =>
        Mount.Set_Pointing_Model;
        The_State := Initializing;
      when Shutdown =>
        Fans.Turn_On_Or_Off;
        Mount.Disable;
        The_State := Disabling;
      when others =>
        null;
      end case;
    end Synchronised_State;

    ------------------
    -- Initializing --
    ------------------
    procedure Initializing_State is
    begin
      case The_Event is
      when Mount_Stopped =>
        The_State := Stopped;
      when Halt =>
        The_State := Synchronised;
      when others =>
        null;
      end case;
    end Initializing_State;

    -------------
    -- Stopped --
    -------------
    procedure Stopped_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        Set_Tracking;
      when Position =>
        Do_Position;
      when Shutdown =>
        Fans.Turn_On_Or_Off;
        Mount.Disable;
        The_State := Disabling;
      when User_Adjust =>
        Jog_Handling;
      when User_Setup =>
        Setup_Handling;
      when Follow =>
        Follow_New_Target;
      when others =>
        null;
      end case;
    end Stopped_State;

    --------------
    -- Stopping --
    --------------
    procedure Stopping_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        Set_Tracking;
      when others =>
        null;
      end case;
    end Stopping_State;

    -----------------
    -- Positioning --
    -----------------
    procedure Positioning_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Positioned;
      when Mount_Tracking =>
        Mount.Stop;
      when Halt =>
        Stop_Target;
      when User_Adjust =>
        Jog_Handling;
      when User_Setup =>
        Setup_Handling;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when others =>
        null;
      end case;
    end Positioning_State;

    ----------------
    -- Positioned --
    ----------------
    procedure Positioned_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Halt =>
        The_State := Stopped;
      when User_Adjust =>
        Jog_Handling;
      when User_Setup =>
        Setup_Handling;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when others =>
        null;
      end case;
    end Positioned_State;

    ---------------
    -- Preparing --
    ---------------
    procedure Preparing_State is
      use type Earth.Direction;
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        if The_Waiting_Position = Earth.Unknown_Direction and then (Time.Universal - The_Start_Time) > 1.0 then
          The_State := Waiting;
        else
          The_Start_Time := Time.Universal;
          Mount.Goto_Mark (The_Waiting_Position, The_Completion_Time);
          The_Waiting_Position := Earth.Unknown_Direction;
        end if;
      when Mount_Tracking =>
        Mount.Stop;
      when Halt =>
        Stop_Target;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when others =>
        null;
      end case;
    end Preparing_State;

    --------------
    -- Waiting --
    --------------
    procedure Waiting_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        if Time.Universal > The_Arrival_Time then
          Update_Target_Position;
          The_State := Approaching;
        end if;
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        Set_Tracking;
      when Halt =>
        The_State := Stopped;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when others =>
        null;
      end case;
    end Waiting_State;

    -----------------
    -- Approaching --
    -----------------
    procedure Approaching_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Tracking =>
        if Space.Direction_Is_Known (The_Home_Direction) then
          declare
            Encoder : constant Mount.Encoder_Data := Mount.Actual_Encoder;
            Limits  : constant Device.Encoder_Limits := Device.Limits;
            Unused  : Time.Ut;
          begin
            if Encoder.Azm > (Limits.Azm_Upper_Goto - Azm_Wrap_Limit) or else
               Encoder.Azm < (Limits.Azm_Lower_Goto + Azm_Wrap_Limit)
            then
              Log.Warning ("goto home direction again (simulation only?)");
              The_Start_Time := Time.Universal;
              Mount.Goto_Target (The_Home_Direction, (0, 0), Unused);
            else
              Mount.Goto_Target (Direction       => Target_Direction,
                                 With_Speed      => Target_Speed,
                                 Completion_Time => The_Completion_Time);
              The_Home_Direction := Space.Unknown_Direction;
            end if;
          end;
        else
          Set_Tracking;
        end if;
      when Halt =>
        Stop_Target;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when User_Adjust =>
        Adjust_Handling;
      when User_Setup =>
        Setup_Handling;
      when others =>
        null;
      end case;
    end Approaching_State;

    --------------
    -- Tracking --
    --------------
    procedure Tracking_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Approaching =>
        The_State := Approaching;
      when Halt =>
        Stop_Target;
      when Follow =>
        Follow_New_Target;
      when Back =>
        Back_To_Target;
      when Position =>
        Do_Position;
      when User_Adjust =>
        Adjust_Handling;
      when User_Setup =>
        Setup_Handling;
      when others =>
        null;
      end case;
    end Tracking_State;

    use type Angle.Signed;

    Has_New_Data : Boolean := True;

    The_Next_Time : Ada.Real_Time.Time;

    use type Ada.Real_Time.Time;

  begin -- Control_Task
    Reset_Adjustments;
    accept Start do
      Device.Start (Fans_State_Handler'access,
                    Mount_State_Handler'access,
                    M3_Position_Handler'access,
                    Parameter.Pointing_Model);
    end Start;
    Log.Write ("Started");
    The_State := Disconnected;
    The_Next_Time := Ada.Real_Time.Clock;
    loop
      begin
        The_Event := No_Event;
        select
          accept Close;
          exit;
        or
          accept Halt;
          Remote.Define (Target => "");
          The_Event := Halt;
        or
          accept Follow (Tracking_Period : Time.Period) do
            The_Next_Tracking_Period := Tracking_Period;
          end Follow;
          if Name.Is_Known (Next_Id) then
            Remote.Define (Target => Name.Image_Of (Next_Id));
          else
            Remote.Define (Target => "");
          end if;
          Reset_Adjustments;
          The_Event := Follow;
        or
          accept Back;
          Reset_Adjustments;
          The_Event := Back;
        or
          accept Startup;
          The_Event := Startup;
        or
          accept Shutdown;
          The_Event := Shutdown;
        or
          accept Position_To (Landmark : Name.Id) do
            The_Landmark := Landmark;
          end Position_To;
          Remote.Define (Target => "");
          Reset_Adjustments;
          The_Event := Position;
        or
          accept Set (The_Orientation : Orientation) do
            Log.Write ("Orientation => " & The_Orientation'img);
            case The_Orientation is
            when Correct =>
              First_Adjust_Factor := 1;
              Second_Adjust_Factor := 1;
            when Upside_Down =>
              First_Adjust_Factor := 1;
              Second_Adjust_Factor := -1;
            when Backwards =>
              First_Adjust_Factor := -1;
              Second_Adjust_Factor := 1;
            when Rotated =>
              First_Adjust_Factor := -1;
              Second_Adjust_Factor := -1;
            end case;
          end Set;
        or
          accept Execute (The_Command : Command) do
            if The_State >= Stopped then
              case The_Command is
              when Adjust =>
                The_Event := User_Adjust;
                The_User_Adjust := The_Command;
              when Setup =>
                The_Event := User_Setup;
                The_User_Setup := The_Command;
              end case;
            end if;
          end Execute;
        or
          accept New_Fans_State (New_State : Fans.State) do
            Log.Write ("Fans State " & New_State'img);
            The_Fans_State := New_State;
            Has_New_Data := True;
          end New_Fans_State;
        or
          accept New_Mount_State (New_State : Mount.State) do
            Log.Write ("Mount State " & New_State'img);
            Mount_Is_Stopped := True;
            case New_State is
            when Mount.Unknown =>
              The_State := Unknown;
              The_Event := Mount_Unknown;
            when Mount.Disconnected =>
              The_Event := Mount_Disconnected;
            when Mount.Connected =>
              The_Event := Mount_Connected;
            when Mount.Enabled =>
              The_Event := Mount_Enabled;
            when Mount.Homing =>
              The_Event := Mount_Homing;
              Mount_Is_Stopped := False;
            when Mount.Synchronised =>
              The_Event := Mount_Synchronised;
            when Mount.Stopped =>
              Calculate_Azm_Offset;
              The_Event := Mount_Stopped;
            when Mount.Approaching =>
              The_Event := Mount_Approaching;
              Mount_Is_Stopped := False;
            when Mount.Tracking =>
              if The_Start_Time = Time.In_The_Past then
                The_Event := Mount_Tracking;
              elsif (Time.Universal - The_Start_Time) > 1.0 then
                The_Event := Mount_Tracking;
                The_Start_Time := Time.In_The_Past;
              else
                The_Event := Mount_Approaching;
              end if;
              Mount_Is_Stopped := False;
            end case;
            Has_New_Data := True;
          end New_Mount_State;
        or
          accept New_M3_Position (New_Position : M3.Position) do
            Log.Write ("M3 Position " & New_Position'img);
            The_M3_Position := New_Position;
            Has_New_Data := True;
          end New_M3_Position;
          case The_M3_Position is
          when M3.Camera =>
            if The_State > Homing then
              Rotator.Start;
              Focuser.Move;
            end if;
          when others =>
            null;
          end case;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := The_State;
            The_Data.Time_Adjustment := Time_Delta(The_Time_Adjustment);
            The_Data.Fans_State := The_Fans_State;
            The_Data.M3_Position := The_M3_Position;
            The_Data.Focuser_Position := Focuser.Position;
            The_Data.Universal_Time := Time.Universal;
            case The_State is
            when Approaching | Preparing | Positioning | Homing =>
              The_Data.Completion_Time := The_Completion_Time;
            when others =>
              The_Data.Completion_Time := Time.In_The_Past;
            end case;
            case The_State is
            when Startup_State =>
              The_Data.Actual_J2000_Direction := Space.Unknown_Direction;
              The_Data.Actual_Direction := Space.Unknown_Direction;
              The_Data.Local_Direction := Earth.Unknown_Direction;
            when others =>
              declare
                Info : constant Mount.Information := Mount.Actual_Info;
              begin
                The_Data.Actual_J2000_Direction := Info.J2000_Direction;
                The_Data.Actual_Direction := Info.Actual_Direction;
                The_Data.Local_Direction := Info.Local_Direction;
                The_Data.Encoder := Info.Encoder;
              end;
            end case;
            if Get_Direction = null then
              The_Data.Target_Direction := Space.Unknown_Direction;
            else
              The_Data.Target_Direction := Get_Direction (Id, The_Data.Universal_Time);
            end if;
            The_Data.Target_Lost := Target_Is_Lost;
            Target_Is_Lost := False;
            The_Data.Local_Offset := The_Adjusted_Offset;
            The_Data.Moving_Speed := Moving_Speeds(Moving_Index);
          end Get;
        or
          delay until The_Next_Time;
          The_Next_Time := The_Next_Time + Ada.Real_Time.To_Time_Span(0.3);
          case The_State is
          when Approaching =>
            if The_Completion_Time < Time.Universal then
              Set_Tracking;
            end if;
            Update_Target_Position;
          when Is_Tracking =>
            Update_Target_Position;
          when Preparing =>
            Update_Preparing_Position;
          when Waiting =>
            The_Event := Mount_Stopped;
          when others =>
            null;
          end case;
        end select;
        Remote.Define (The_State in Is_Tracking);
        if The_Event /= No_Event then
          Log.Write ("State => " & The_State'img & " - Event => " & The_Event'img);
          case The_State is
          when Unknown       => Unknown_State;
          when Disconnected  => Disconnected_State;
          when Disconnecting => Disconnecting_State;
          when Connecting    => Connecting_State;
          when Connected     => Connected_State;
          when Disabling     => Disabling_State;
          when Enabling      => Enabling_State;
          when Enabled       => Enabled_State;
          when Homing        => Homing_State;
          when Synchronised  => Synchronised_State;
          when Initializing  => Initializing_State;
          when Stopped       => Stopped_State;
          when Stopping      => Stopping_State;
          when Positioning   => Positioning_State;
          when Positioned    => Positioned_State;
          when Preparing     => Preparing_State;
          when Waiting       => Waiting_State;
          when Approaching   => Approaching_State;
          when Is_Tracking   => Tracking_State;
          end case;
          Has_New_Data := True;
        end if;
        if Has_New_Data then
          Has_New_Data := False;
          Signal_Information_Update.all;
        end if;
      exception
      when Item: others =>
        Log.Termination (Item);
      end;
    end loop;
    Device.Finalize;
    Log.Write ("Control end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control_Task;

end Telescope;
