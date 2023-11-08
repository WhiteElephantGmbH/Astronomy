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

with Ada.Real_Time;
with Cdk_700;
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


  The_Park_Position : Earth.Direction;

  procedure Define_Park_Position (The_Position : Earth.Direction) is
  begin
    The_Park_Position := The_Position;
  end Define_Park_Position;

  function Park_Position_Defined return Boolean is (Earth.Direction_Is_Known (The_Park_Position));


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

    The_Next_Tracking_Period : Time.Period := Time.Undefined;
    The_Completion_Time      : Time.Ut := Time.In_The_Past;
    The_Start_Time           : Time.Ut := Time.In_The_Past;
    The_Arrival_Time         : Time.Ut := Time.In_The_Future;
    Is_Fast_Tracking         : Boolean := False;

    The_Landmark : Name.Id;

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
                   Mount_Error,
                   Mount_Connected,
                   Mount_Enabled,
                   Mount_Stopped,
                   Mount_Approach,
                   Mount_Track);

    subtype Mount_Startup is Event range Mount_Disconnected .. Mount_Enabled;

    The_State  : State := Unknown;
    The_Event  : Event := No_Event;

    Mount_Is_Stopped : Boolean := True;

    The_Fans_State : Fans.State := Fans.Initial_State;

    The_M3_Position : M3.Position := M3.Unknown;

    The_User_Adjust : Adjust;
    The_User_Setup  : Setup;

    Id            : Name.Id;
    Get_Direction : Get_Space_Access;

    Target_Lost    : exception;
    Target_Is_Lost : Boolean := False;


    function Target_Direction (At_Time : Time.Ut := Time.Universal) return Space.Direction is
      Direction : constant Space.Direction := Get_Direction (Id, At_Time);
    begin
      if not Space.Direction_Is_Known (Direction) then
        Target_Is_Lost := True;
        raise Target_Lost;
      end if;
      return Direction;
    end Target_Direction;


    procedure Goto_Target is
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      The_Start_Time := Time.Universal;
      Mount.Goto_Target (Direction       => Target_Direction (At_Time => The_Start_Time),
                         Completion_Time => The_Completion_Time);
      The_State := Approaching;
    exception
    when Target_Lost =>
      null;
    end Goto_Target;


    procedure Stop_Target is
    begin
      Mount.Stop;
      if Mount_Is_Stopped then
        The_State := Stopped;
      else
        The_State := Stopping;
      end if;
    end Stop_Target;


    procedure Back_To_Target is
      Unused : Time.Ut;
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      The_Start_Time := Time.Universal;
      Mount.Goto_Target (Direction       => Target_Direction (At_Time => The_Start_Time),
                         Completion_Time => Unused);
    exception
    when Target_Lost =>
      Stop_Target;
    end Back_To_Target;


    function Goto_Waiting_Position return Boolean is

      Arrival_Position : constant Earth.Direction := Objects.Direction_Of (Get_Direction (Id, The_Arrival_Time),
                                                                           Time.Lmst_Of (The_Arrival_Time));
    begin
      The_Arrival_Time := The_Next_Tracking_Period.Arrival_Time;
      The_Start_Time := Time.Universal;
      Mount.Goto_Mark (Arrival_Position, The_Completion_Time);
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


    procedure Do_Park is
    begin
      The_Start_Time := Time.Universal;
      Mount.Goto_Mark (The_Park_Position, The_Completion_Time);
      Log.Write ("do park");
      The_State := Parking;
    end Do_Park;


    procedure Do_Position is
    begin
      The_Start_Time := Time.Universal;
      Mount.Goto_Mark (Name.Direction_Of (The_Landmark), The_Completion_Time);
      Log.Write ("position to Landmark");
      The_State := Positioning;
    end Do_Position;


    procedure Do_Disable is
    begin
      Fans.Turn_On_Or_Off;
      Mount.Disable;
    end Do_Disable;


    procedure Offset_Handling is
      --Speed : constant Angle.Value := Moving_Speeds(Moving_Index);
    begin
      case The_User_Adjust is
      when Move_Left =>
        null;
      when Move_Right =>
        null;
      when Move_Up =>
        null;
      when Move_Down =>
        null;
      when End_Move =>
        null;
      when Decrease_Time =>
        null;
      when Increase_Time =>
        null;
      when End_Change =>
        null;
      end case;
    end Offset_Handling;


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
      when Mount_Error =>
        return Mount_Error;
      when Mount_Connected =>
        return Connected;
      when Mount_Enabled =>
        return Enabled;
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
      when Mount_Approach =>
        The_State := Approaching;
      when Mount_Track =>
        Set_Tracking;
      when others =>
        Mount.Stop;
        The_State := Disconnected;
      end case;
    end Unknown_State;

    ----------------
    -- Restarting --
    ----------------

    procedure Restarting_State is
    begin
      null;
    end Restarting_State;

    ------------------
    -- Disconnected --
    ------------------
    procedure Disconnected_State is
    begin
      case The_Event is
      when Startup =>
        Mount.Connect;
        The_State := Connecting;
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        The_State := Stopped;
      when Mount_Approach =>
        The_State := Approaching;
      when Mount_Track =>
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

    -----------
    -- Error --
    -----------
    procedure Error_State is
    begin
      case The_Event is
      when Shutdown =>
        Do_Disable;
        delay 3.0;
        Mount.Disconnect;
        The_State := Disconnecting;
      when others =>
        null;
      end case;
    end Error_State;

    ----------------
    -- Connecting --
    ----------------
    procedure Connecting_State is
    begin
      case The_Event is
      when Mount_Error =>
        The_State := Mount_Error;
      when Mount_Connected =>
        Mount.Enable;
        The_State := Enabling;
      when Mount_Enabled =>
        M3.Turn (To => Parameter.M3_Default_Place);
        Mount.Find_Home (The_Completion_Time);
        The_State := Homing;
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
      when Mount_Error =>
        The_State := Mount_Error;
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
      when Mount_Error =>
        The_State := Mount_Error;
      when Mount_Enabled =>
        Fans.Turn (To => Fans.Off);
        M3.Turn (To => Parameter.M3_Default_Place);
        Mount.Find_Home (The_Completion_Time);
        The_State := Homing;
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
        The_State := Homing;
      when Shutdown =>
        Do_Disable;
        The_State := Disabling;
      when others =>
        null;
      end case;
    end Enabled_State;

    ------------
    -- Homing --
    ------------
    procedure Homing_State is
    begin
      case The_Event is
      when Mount_Error =>
        The_State := Mount_Error;
      when Mount_Stopped =>
        The_State := Stopped;
      when Halt =>
        The_State := Enabled;
      when others =>
        null;
      end case;
    end Homing_State;

    -------------
    -- Stopped --
    -------------
    procedure Stopped_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Approach =>
        The_State := Approaching;
      when Mount_Track =>
        Set_Tracking;
      when Position =>
        Do_Position;
      when Shutdown =>
        if Park_Position_Defined then
          Do_Park;
          The_State := Parking;
        else
          Do_Disable;
          The_State := Disabling;
        end if;
      when User_Setup =>
        Setup_Handling;
      when Follow =>
        Follow_New_Target;
      when others =>
        null;
      end case;
    end Stopped_State;

    -------------
    -- Parking --
    -------------
    procedure Parking_State is
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        Do_Disable;
        The_State := Disabling;
      when Mount_Track =>
        Mount.Stop;
      when Halt =>
        Stop_Target;
      when others =>
        null;
      end case;
     end Parking_State;

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
      when Mount_Approach =>
        The_State := Approaching;
      when Mount_Track =>
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
      when Mount_Track =>
        Mount.Stop;
      when Halt =>
        Stop_Target;
      when User_Adjust =>
        Offset_Handling;
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
        Offset_Handling;
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
      when Mount_Track =>
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
      use type Time.Ut;
    begin
      case The_Event is
      when Mount_Startup =>
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        if Time.Universal > The_Arrival_Time then
          The_State := Approaching;
        end if;
      when Mount_Approach =>
        The_State := Approaching;
      when Mount_Track =>
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
      when Mount_Track =>
        Set_Tracking;
      when Halt =>
        Stop_Target;
      when Follow =>
        Follow_New_Target;
      when Position =>
        Do_Position;
      when User_Adjust =>
        Offset_Handling;
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
      when Mount_Approach =>
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
        Offset_Handling;
      when User_Setup =>
        Setup_Handling;
      when others =>
        null;
      end case;
    end Tracking_State;

    Has_New_Data : Boolean := True;

    The_Next_Time : Ada.Real_Time.Time;

    use type Ada.Real_Time.Time;
    use type Time.Ut;

  begin -- Control_Task
    accept Start do
      Device.Start (Fans_State_Handler'access,
                    Mount_State_Handler'access,
                    M3_Position_Handler'access);
    end Start;
    Log.Write ("Started");
    if Cdk_700.Is_Started then
      The_State := Disconnected;
    else
      The_State := Restarting;
    end if;
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
          The_Event := Follow;
        or
          accept Back;
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
          The_Event := Position;
        or
          accept Set (The_Orientation : Orientation) do
            Log.Write ("Orientation => " & The_Orientation'img);
            case The_Orientation is
            when Correct =>
              null;
              --First_Adjust_Factor := 1;
              --Second_Adjust_Factor := 1;
            when Upside_Down =>
              null;
              --First_Adjust_Factor := 1;
              --Second_Adjust_Factor := -1;
            when Backwards =>
              null;
              --First_Adjust_Factor := -1;
              --Second_Adjust_Factor := 1;
            when Rotated =>
              null;
              --First_Adjust_Factor := -1;
              --Second_Adjust_Factor := -1;
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
            when Mount.Stopped =>
              The_Event := Mount_Stopped;
            when Mount.Approaching =>
              Mount.Confirm_Goto;
              The_Event := Mount_Approach;
              Mount_Is_Stopped := False;
            when Mount.Tracking =>
              The_Event := Mount_Track;
              Mount_Is_Stopped := False;
            when Mount.Error =>
              The_Event := Mount_Error;
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
              Rotator.Enable;
            end if;
          when M3.Ocular =>
            if The_State > Homing then
              Rotator.Disable;
            end if;
          when others =>
            null;
          end case;
        or
          accept Get (The_Data : out Data) do
            if The_State = Restarting then
              if Cdk_700.Is_Started then
                The_State := Disconnected;
              end if;
            end if;
            The_Data.Status := The_State;
            The_Data.Time_Adjustment := 0.0; --!!! get from PWI
            The_Data.Fans_State := The_Fans_State;
            The_Data.M3_Position := The_M3_Position;
            The_Data.Universal_Time := Time.Universal; --!!! get from PWI
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
                The_Data.Az_Position := Info.Az_Axis.Position;
                The_Data.Alt_Position := Info.Alt_Axis.Position;
              end;
            end case;
            if Get_Direction = null then
              The_Data.Target_Direction := Space.Unknown_Direction;
            else
              The_Data.Target_Direction := Get_Direction (Id, The_Data.Universal_Time);
            end if;
            The_Data.Target_Lost := Target_Is_Lost;
            Target_Is_Lost := False;
            The_Data.Local_Offset := Earth.Unknown_Direction; --!!! get from PWI
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
          when Restarting    => Restarting_State;
          when Disconnected  => Disconnected_State;
          when Disconnecting => Disconnecting_State;
          when Mount_Error   => Error_State;
          when Connecting    => Connecting_State;
          when Connected     => Connected_State;
          when Disabling     => Disabling_State;
          when Enabling      => Enabling_State;
          when Enabled       => Enabled_State;
          when Homing        => Homing_State;
          when Stopped       => Stopped_State;
          when Stopping      => Stopping_State;
          when Parking       => Parking_State;
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
