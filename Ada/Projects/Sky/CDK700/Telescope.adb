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

with Ada.Real_Time;
with Numerics;
with Parameter;
with System;
with Traces;

package body Telescope is

  package Log is new Traces ("Telescope");

  package Fans renames Device.Fans;

  package Mount renames Device.Mount;

  task type Control_Task with Priority => System.Max_Priority is

    entry Start;

    entry Execute (The_Command : Command);

    entry Set (The_Orientation : Orientation);

    entry Halt;

    entry Startup;

    entry Shutdown;

    entry Follow (Arriving_Time : Time.Ut);

    entry Position_To (Landmark : Name.Id);

    entry New_Mount_State (New_State : Mount.State);

    entry New_M3_Position (New_Position : M3.Position);

    entry New_Rotator_State (New_State : Rotator.State);

    entry Get (The_Data : out Data);

    entry Close;

  end Control_Task;

  type Control_Access is access Control_Task;

  Control : Control_Access;


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


  procedure Rotator_State_Handler (New_State : Rotator.State) is
  begin
    if not Control'terminated then
      Control.New_Rotator_State (New_State);
    end if;
  end Rotator_State_Handler;


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


  procedure Follow (Arriving_Time : Time.Ut) is
  begin
    Control.Follow (Arriving_Time);
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

    The_Arriving_Time      : Time.Ut := Time.In_The_Future;
    The_Next_Arriving_Time : Time.Ut := Time.In_The_Past;
    The_Completion_Time    : Time.Ut := Time.In_The_Past;
    The_Start_Time         : Time.Ut := Time.In_The_Past;

    Is_Fast_Tracking : Boolean := False;

    The_Landmark : Name.Id;

    type Adjusting_Kind is (First_Adjusting, Second_Adjusting, Time_Adjusting);

    The_Adjusting_Kind        : Adjusting_Kind;
    The_Adjusting_Start_Time  : Time.Ut := Time.In_The_Future;
    The_Adjusting_End_Time    : Time.Ut := Time.In_The_Future;
    The_Time_Adjusting_Factor : Duration;
    The_Time_Adjustment       : Duration := 0.0;
    First_Adjust_Factor       : Angle.Signed := 1;
    The_First_Offset          : Angle.Degrees := 0.0;
    The_First_Moving_Speed    : Angle.Degrees := 0.0;
    Second_Adjust_Factor      : Angle.Signed := 1;
    The_Second_Offset         : Angle.Degrees := 0.0;
    The_Second_Moving_Speed   : Angle.Degrees := 0.0;

    type Event is (No_Event,
                   Startup,
                   Shutdown,
                   Halt,
                   Follow,
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

    The_M3_Position : M3.Position := M3.Unknown;

    The_Rotator_State : Rotator.State := Rotator.Unknown;

    The_User_Adjust : Adjust;
    The_User_Setup  : Setup;

    Id            : Name.Id;
    Get_Direction : Get_Space_Access;

    Target_Lost    : exception;
    Target_Is_Lost : Boolean := False;


    function Target_Direction (At_Time : Time.Ut := Time.Universal) return Space.Direction is
      Direction : constant Space.Direction := Get_Direction (Id, At_Time + The_Time_Adjustment);
      use type Space.Direction;
    begin
      if Direction = Space.Unknown_Direction then
        Target_Is_Lost := True;
        raise Target_Lost;
      end if;
      return Direction;
    end Target_Direction;


    function Target_Speed (At_Time : Time.Ut := Time.Universal) return Mount.Speed is
      Direction_Before : constant Space.Direction := Get_Direction (Id, At_Time - 0.5);
      Direction_After  : constant Space.Direction := Get_Direction (Id, At_Time + 0.5);
      Direction_Delta  : Space.Direction;
      Ra_Speed         : Angle.Signed;
      Dec_Speed        : Angle.Signed;
      use type Space.Direction;
      use type Angle.Degrees;
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


    procedure Goto_Target is
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      The_Start_Time := Time.Universal;
      Mount.Goto_Target (Target_Direction (The_Start_Time), Target_Speed (The_Start_Time), The_Completion_Time);
      The_State := Approaching;
    exception
    when Target_Lost =>
      null;
    end Goto_Target;


    procedure Goto_Waiting_Position is
      Waiting_Position : constant Earth.Direction := Numerics.Direction_Of (Get_Direction (Id, The_Arriving_Time),
                                                                            Time.Lmst_Of (The_Arriving_Time));
    begin
      if Earth.Direction_Is_Known (Waiting_Position) then
        The_Start_Time := Time.Universal;
        Mount.Goto_Mark (Waiting_Position, The_Completion_Time);
        The_State := Preparing;
      end if;
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
    begin
      Id := Next_Id;
      Get_Direction := Next_Get_Direction;
      Is_Fast_Tracking := The_Next_Arriving_Time /= Time.In_The_Past;
      if Is_Fast_Tracking then
        Log.Write ("follow from " & The_State'img & " after " & Time.Image_Of (The_Next_Arriving_Time));
        The_Arriving_Time := The_Next_Arriving_Time;
        if The_Arriving_Time > Time.Universal then -- arriving in future
          Goto_Waiting_Position;
        else
          Goto_Target;
        end if;
      else
        Log.Write ("follow from " & The_State'img);
        Goto_Target;
      end if;
    end Follow_New_Target;


    procedure Stop_Target is
    begin
      Mount.Stop;
      if Mount_Is_Stopped then
        The_State := Stopped;
      else
        The_State := Stopping;
      end if;
    end Stop_Target;


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
          The_First_Offset := The_First_Offset + (The_First_Moving_Speed * Angle.Degrees(The_Adjusting_Time));
        when Second_Adjusting =>
          The_Second_Offset := The_Second_Offset + (The_Second_Moving_Speed * Angle.Degrees(The_Adjusting_Time));
        when Time_Adjusting =>
          The_Time_Adjustment := The_Time_Adjustment + (The_Adjusting_Time * The_Time_Adjusting_Factor);
        end case;
      end if;
    end Increment_Offset;


    procedure Reset_Adjustments is
    begin
      The_First_Offset := 0.0;
      The_Second_Offset := 0.0;
      The_Time_Adjustment := 0.0;
    end Reset_Adjustments;


    procedure Update_Target_Position is
      Now    : constant Time.Ut := Time.Universal;
      Unused : Time.Ut;
    begin
      Increment_Offset;
      Mount.Goto_Target (Direction       => Target_Direction (At_Time => Now),
                         With_Speed      => Target_Speed (At_Time => Now),
                         Completion_Time => Unused);
    exception
    when Target_Lost =>
      Stop_Target;
    end Update_Target_Position;


    procedure Update_Mark_Position is
      Unused : Time.Ut;
    begin
      Increment_Offset;
      Mount.Goto_Mark (Name.Direction_Of (The_Landmark), Unused);
    end Update_Mark_Position;


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
        The_State := Tracking;
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
        Rotator.Connect;
        The_State := Connecting;
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        The_State := Tracking;
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
        Fans.Turn_On_Or_Off;
        Mount.Enable;
        The_State := Enabling;
      when Mount_Enabled =>
        M3.Turn (To => M3.Ocular);
        Mount.Find_Home (The_Completion_Time);
        Rotator.Find_Home;
        The_State := Homing;
      when Mount_Synchronised =>
        Mount.Set_Pointing_Model;
        Rotator.Start;
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
        Fans.Turn_On_Or_Off;
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
        M3.Turn (To => M3.Ocular);
        Mount.Find_Home (The_Completion_Time);
        Rotator.Find_Home;
        The_State := Homing;
      when Mount_Homing =>
        The_State := Homing;
      when Mount_Synchronised =>
        Mount.Set_Pointing_Model;
        Rotator.Start;
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
        M3.Turn (To => M3.Ocular);
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
      use type Rotator.State;
    begin
      case The_Event is
      when Mount_Synchronised =>
        if The_Rotator_State /= Rotator.Homing and The_M3_Position = M3.Ocular then
          Mount.Set_Pointing_Model;
          Rotator.Start;
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
        Rotator.Start;
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
        The_State := Tracking;
      when Position =>
        Do_Position;
      when Shutdown =>
        Fans.Turn_On_Or_Off;
        Mount.Disable;
        The_State := Disabling;
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
        The_State := Tracking;
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
        Adjust_Handling;
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
        Adjust_Handling;
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
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Waiting;
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
      when Mount_Approaching =>
        The_State := Approaching;
      when Mount_Tracking =>
        The_State := Tracking;
      when Halt =>
        The_State := Stopped;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when others =>
        if Time.Universal > The_Arriving_Time then
          Update_Target_Position;
          The_State := Approaching;
        end if;
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
        The_State := Tracking;
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
    use type Angle.Value;

  begin -- Control_Task
    accept Start do
      Device.Start (Mount_State_Handler'access,
                    M3_Position_Handler'access,
                    Rotator_State_Handler'access,
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
          The_Event := Halt;
        or
          accept Follow (Arriving_Time : Time.Ut) do
            The_Next_Arriving_Time := Arriving_Time;
          end Follow;
          Reset_Adjustments;
          The_Event := Follow;
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
        or
          accept New_Rotator_State (New_State : Rotator.State) do
            Log.Write ("Rotator State " & New_State'img);
            The_Rotator_State := New_State;
            Has_New_Data := True;
          end New_Rotator_State;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := The_State;
            The_Data.Time_Adjustment := Time_Delta(The_Time_Adjustment);
            The_Data.M3_Position := The_M3_Position;
            The_Data.Rotator_State := The_Rotator_State;
            The_Data.Universal_Time := Time.Universal;
            case The_State is
            when Approaching | Positioning | Homing =>
              The_Data.Completion_Time := The_Completion_Time;
            when others =>
              The_Data.Completion_Time := Time.In_The_Past;
            end case;
            declare
              Info : constant Mount.Information := Mount.Actual_Info;
            begin
              The_Data.Actual_J2000_Direction := Info.J2000_Direction;
              The_Data.Actual_Direction := Info.Actual_Direction;
              The_Data.Local_Direction := Info.Local_Direction;
            end;
            if Get_Direction = null then
              The_Data.Target_Direction := Space.Unknown_Direction;
            else
              The_Data.Target_Direction := Get_Direction (Id, The_Data.Universal_Time);
            end if;
            The_Data.Target_Lost := Target_Is_Lost;
            Target_Is_Lost := False;
            The_Data.Local_Offset := Earth.Direction_Of (Az  => +The_First_Offset,
                                                         Alt => +The_Second_Offset);
            The_Data.Moving_Speed := Moving_Speeds(Moving_Index);
          end Get;
        or
          delay until The_Next_Time;
          The_Next_Time := The_Next_Time + Ada.Real_Time.To_Time_Span(0.3);
          case The_State is
          when Approaching =>
            if The_Completion_Time < Time.Universal then
              The_State := Tracking;
            end if;
            Update_Target_Position;
          when Positioning =>
            if The_Completion_Time < Time.Universal then
              The_State := Positioned;
            end if;
            Update_Mark_Position;
          when Tracking =>
            Update_Target_Position;
          when Positioned =>
            Update_Mark_Position;
          when others =>
            null;
          end case;
        end select;
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
          when Tracking      => Tracking_State;
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
