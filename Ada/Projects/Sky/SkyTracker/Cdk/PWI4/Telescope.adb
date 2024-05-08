-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Cwe;
with Error;
with Gui;
with Http_Server;
with Input;
with Objects;
with Parameter;
with Picture;
with Remote;
with Text;
with Site;
with System;
with Targets;
with Traces;
with User;

package body Telescope is

  package Log is new Traces ("Telescope");

  package Mount renames Device.Mount;

  package M3 renames Device.M3;

  package Focuser renames Device.Focuser;

  package Rotator renames Device.Rotator;

  package Fans renames Device.Fans;

  subtype Command is Input.Mount_Command;

  subtype Adjust is Input.Adjust_Command;

  subtype Setup is Input.Setup_Command;

  use all type Input.Command;


  task type Control_Task with Priority => System.Max_Priority is

    entry Start;

    entry Execute (The_Command : Command);

    entry Go_Back;

    entry Halt;

    entry Startup;

    entry Shutdown;

    entry Follow (Tracking_Period : Time.Period);

    entry Position_To (Landmark : Name.Id);

    entry Focuser_Goto (The_Position : Device.Microns);

    entry Rotator_Goto_Field (The_Angle : Device.Degrees);

    entry Rotator_Goto_Mech (The_Position : Device.Degrees);

    entry Rotator_Goto (The_Offset : Device.Degrees);

    entry New_Mount_State (New_State : Mount.State);

    entry New_Focuser_State (New_State : Focuser.State);

    entry New_M3_Position (New_Position : M3.Position);

    entry Get (The_Data : out Data);

    entry Close;

  end Control_Task;

  type Control_Access is access Control_Task;

  Control : Control_Access;


  procedure Focuser_Goto (Position : Http_Server.Microns) is
  begin
    Control.Focuser_Goto (Device.Microns(Position));
  end Focuser_Goto;


  procedure Rotator_Goto_Field_Angle (Item : Http_Server.Degrees) is
  begin
    Control.Rotator_Goto_Field (The_Angle => Device.Degrees(Item));
  end Rotator_Goto_Field_Angle;


  procedure Rotator_Goto_Mech_Position (Item : Http_Server.Degrees) is
  begin
    Control.Rotator_Goto_Mech (The_Position => Device.Degrees(Item));
  end Rotator_Goto_Mech_Position;


  procedure Rotator_Goto_Offset (Item : Http_Server.Degrees) is
  begin
    Control.Rotator_Goto (The_Offset => Device.Degrees(Item));
  end Rotator_Goto_Offset;


  procedure Set_Server_Information (The_Data : Data) is

    function Control_Data return Http_Server.Control_Data is
      ((Window_Minimized => User.Window_Minimized));

    function Mount_Data return Http_Server.Mount_Data is
      (if The_Data.Status > Homing then
        (Exists       => True,
         Axis0        => Http_Server.Degrees(The_Data.Mount.Axis0),
         Axis1        => Http_Server.Degrees(The_Data.Mount.Axis1),
         Model_Points => Natural(The_Data.Mount.Model_Points))
       else
         (others => <>));

    function Focuser_Data return Http_Server.Focuser_Data is
      ((Exists       => The_Data.Focuser.Exists,
        Moving       => The_Data.Focuser.Moving,
        Max_Position => Http_Server.Microns(The_Data.Focuser.Max_Position),
        Zoom_Size    => Http_Server.Microns(The_Data.Focuser.Zoom_Size),
        Position     => Http_Server.Microns(The_Data.Focuser.Position),
        Set_Position => Focuser_Goto'access));

    function Rotator_Data return Http_Server.Rotator_Data is
     ((Exists             => The_Data.Focuser.Exists,
       Moving             => The_Data.Rotator.Moving,
       Slewing            => The_Data.Rotator.Slewing,
       Mech_Position      => Http_Server.Degrees(The_Data.Rotator.Mech_Position),
       Field_Angle        => Http_Server.Degrees(The_Data.Rotator.Field_Angle),
       Goto_Field_Angle   => Rotator_Goto_Field_Angle'access,
       Goto_Mech_Position => Rotator_Goto_Mech_Position'access,
       Goto_Offset        => Rotator_Goto_Offset'access));

  begin -- Set_Server_Information
    Http_Server.Set (Control_Data);
    Http_Server.Set_State (Text.Legible_Of (The_Data.Status'image));
    Http_Server.Set_Moving (Speed => The_Data.Moving_Speed);
    Http_Server.Set (Position => Http_Server.Mirror_Position'val(Device.M3.Position'pos(The_Data.M3.Position)));
    Http_Server.Set (Mount_Data);
    Http_Server.Set (Focuser_Data);
    Http_Server.Set (Rotator_Data);
  end Set_Server_Information;


  procedure Mount_State_Handler (New_State : Mount.State) is
  begin
    if not Control'terminated then
      Control.New_Mount_State (New_State);
    end if;
  end Mount_State_Handler;


  procedure Focuser_State_Handler (New_State : Focuser.State) is
  begin
    if not Control'terminated then
      Control.New_Focuser_State (New_State);
    end if;
  end Focuser_State_Handler;


  procedure M3_Position_Handler (New_Position : M3.Position) is
  begin
    if not Control'terminated then
      Control.New_M3_Position (New_Position);
    end if;
  end M3_Position_Handler;


  Signal_Information_Update : Information_Update_Handler;


  procedure Execute (The_Command : Input.Command) is
  begin
    case The_Command is
    when Command =>
      Control.Execute (The_Command);
    when Go_Back =>
     Control.Go_Back;
    when Stop =>
      User.Perform_Stop;
    end case;
  end Execute;


  procedure Start (Update_Handler : Information_Update_Handler) is
  begin
    Signal_Information_Update := Update_Handler;
    Input.Open (Execute'access);
    Control := new Control_Task;
    Control.Start;
  end Start;


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
                                 The_Id        : Name.Id := Name.No_Id) is
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


  Max_Fucuser_Position : Device.Microns;

  procedure Define_Max_Focuser_Position (The_Position : Device.Microns) is
  begin
    Log.Write ("define max focuser position =>" & The_Position'image);
    Max_Fucuser_Position := The_Position;
  end Define_Max_Focuser_Position;


  Fucuser_Zoom_Size : Device.Microns;

  procedure Define_Fucuser_Zoom_Size (The_Size : Device.Microns) is
  begin
    Log.Write ("define focuser zoom size =>" & The_Size'image);
    Fucuser_Zoom_Size := The_Size;
  end Define_Fucuser_Zoom_Size;


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
    The_Start_Direction      : Space.Direction;
    The_Arrival_Time         : Time.Ut := Time.In_The_Future;
    Is_Fast_Tracking         : Boolean := False;

    The_Adjusted_Offset : Earth.Direction;

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
                   Focuser_Connected,
                   Focuser_Moving,
                   Mount_Connected,
                   Mount_Enabled,
                   Mount_Stopped,
                   Mount_Approach,
                   Mount_Track);

    subtype Mount_Startup is Event range Mount_Disconnected .. Mount_Enabled;

    The_State  : State := Unknown;
    Is_Closing : Boolean := False;
    Is_Ending  : Boolean := False;
    The_Event  : Event := No_Event;

    Mount_Is_Stopped : Boolean := True;

    The_Focuser_State : Focuser.State := Focuser.Unknown;
    The_Mount_State   : Mount.State := Mount.Unknown;
    The_M3_Position   : M3.Position := M3.Unknown;

    use type Focuser.State;
    use type Mount.State;

    The_User_Adjust : Adjust;
    The_User_Setup  : Setup;

    Id            : Name.Id;
    Get_Direction : Get_Space_Access;

    Target_Lost    : exception;
    Target_Is_Lost : Boolean := False;


    function Target_Direction (At_Time : Time.Ut := Time.Universal) return Space.Direction is
      Direction     : constant Space.Direction := Get_Direction (Id, At_Time);
      The_Direction : Earth.Direction;
    begin
      if not Space.Direction_Is_Known (Direction) then
        Target_Is_Lost := True;
        raise Target_Lost;
      end if;
      if Earth.Direction_Is_Known (The_Adjusted_Offset) then
        The_Direction := Objects.Direction_Of (Direction, Time.Lmst_Of (At_Time));
        Earth.Add_To (The_Direction, The_Adjusted_Offset);
        return Objects.Direction_Of (The_Direction, At_Time);
      else
        return Direction;
      end if;
    end Target_Direction;


    procedure Reset_Adjustments is
    begin
      The_Adjusted_Offset := Cwe.Adjustment;
    end Reset_Adjustments;


    procedure Set_Wrap_Position_For (The_Direction : Earth.Direction) is
      The_Start_Az : Angle.Degrees;
      use type Angle.Value;
      use type Device.Degrees;
    begin
      The_Start_Az := +Earth.Az_Of (The_Direction);
      Mount.Set_Axis0_Wrap (Device.Degrees(The_Start_Az) - 180.0);
    end Set_Wrap_Position_For;


    procedure Goto_Target is
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      The_Start_Time := Time.Universal;
      The_Start_Direction := Target_Direction (At_Time => The_Start_Time);
      Set_Wrap_Position_For (Objects.Direction_Of (The_Start_Direction, Time.Lmst_Of (The_Start_Time)));
      Mount.Goto_Target (Direction       => The_Start_Direction,
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
      Reset_Adjustments;
    end Stop_Target;


    procedure Back_To_Target is
      Unused : Time.Ut;
    begin
      if Is_Fast_Tracking then
        Mount.Reset_Moving_Target;
      else
        if Get_Direction = null then
          raise Program_Error; -- unknown target;
        end if;
        The_Start_Time := Time.Universal;
        The_Start_Direction := Target_Direction (At_Time => The_Start_Time);
        Mount.Goto_Target (Direction       => The_Start_Direction,
                           Completion_Time => Unused);
      end if;
    exception
    when Target_Lost =>
      Stop_Target;
    end Back_To_Target;


    procedure Goto_Mark (The_Position : Earth.Direction) is
    begin
      The_Start_Time := Time.Universal;
      Set_Wrap_Position_For (The_Position);
      Mount.Goto_Mark (The_Position, The_Completion_Time);
    end Goto_Mark;


    procedure Goto_Waiting_Position is
    begin
      declare
        Arrival_Position : constant Earth.Direction := Objects.Direction_Of (Get_Direction (Id, The_Arrival_Time),
                                                                             Time.Lmst_Of (The_Arrival_Time));
      begin
        Goto_Mark (Arrival_Position);
        The_State := Preparing;
      end;
    end Goto_Waiting_Position;


    procedure Follow_Target (Neo_Id : Name.Id) is
    begin
      Mount.Follow_Tle (Neo_Id);
    end Follow_Target;


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
      use type Time.Ut;
    begin
      Id := Next_Id;
      Get_Direction := Next_Get_Direction;
      Is_Fast_Tracking := The_Next_Tracking_Period /= Time.Undefined;
      if Is_Fast_Tracking then
        Log.Write ("follow from " & The_State'img & " after " & Time.Image_Of (The_Next_Tracking_Period.Arrival_Time)
                                                  & " until " & Time.Image_Of (The_Next_Tracking_Period.Leaving_Time));
        The_Arrival_Time := The_Next_Tracking_Period.Arrival_Time;
        if Time.Universal < The_Arrival_Time then
          Goto_Waiting_Position;
        else
          Follow_Target (Id);
        end if;
      else
        Log.Write ("follow from " & The_State'img);
        Goto_Target;
      end if;
    end Follow_New_Target;


    procedure Do_Park is
    begin
      Goto_Mark (The_Park_Position);
      Log.Write ("do park");
      The_State := Parking;
    end Do_Park;


    procedure Do_Position is
    begin
      Goto_Mark (Name.Direction_Of (The_Landmark));
      Log.Write ("position to Landmark");
      The_State := Positioning;
    end Do_Position;


    procedure Do_Disable is
    begin
      Fans.Turn_On_Or_Off;
      Mount.Disable;
    end Do_Disable;


    procedure Adjust_First (The_Speed : Angle.Signed) is
      use type Angle.Signed;
      Moving_Speed : constant Angle.Degrees := +(The_Speed);
      use type Angle.Degrees;
      use type M3.Position;
      use type Device.Speed;
    begin
      if Is_Fast_Tracking then
        Mount.Set_Rate_Transverse (Device.Speed(Moving_Speed * 3600.0));
      elsif The_M3_Position = M3.Camera then
        Mount.Set_Rate_Ra (-Device.Speed(Moving_Speed * 3600.0));
      else
        Mount.Set_Rate_Axis0 (Device.Speed(Moving_Speed * 3600.0));
      end if;
    end Adjust_First;


    procedure Adjust_Second (The_Speed : Angle.Signed) is
      use type Angle.Signed;
      Moving_Speed : constant Angle.Degrees := +(The_Speed);
      use type Angle.Degrees;
      use type M3.Position;
      use type Device.Speed;
    begin
      if Is_Fast_Tracking then
        Mount.Set_Rate_Path (Device.Speed(Moving_Speed * 3600.0));
      elsif The_M3_Position = M3.Camera then
        Mount.Set_Rate_Dec (Device.Speed(Moving_Speed * 3600.0));
      else
        Mount.Set_Rate_Axis1 (-Device.Speed(Moving_Speed * 3600.0));
      end if;
    end Adjust_Second;


    procedure Add_Target_J2000_Direction_To_Model is
      Direction : constant Space.Direction := Targets.J2000_Direction_Of (Id);
    begin
      if Space.Direction_Is_Known (Direction) then
        Mount.Add_To_Model (Direction);
      end if;
    end Add_Target_J2000_Direction_To_Model;


    procedure Offset_Handling is
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
      when End_Command =>
        Mount.Stop_Rate;
      when Spiral_Offset_Center =>
        Mount.Spiral_Offset_Center;
      when Spiral_Offset_Next =>
        Mount.Spiral_Offset_Next;
      when Spiral_Offset_Previous =>
        Mount.Spiral_Offset_Previous;
      when Start_Time_Increase =>
        null; -- not implemented
      when Start_Time_Decrease =>
        null; -- not implemented
      when End_Time_Change =>
        null; -- not implemented
      when Add_Point =>
        Add_Target_J2000_Direction_To_Model;
      end case;
    end Offset_Handling;


    procedure Update_Target_Direction is
      Actual_Direction : constant Space.Direction := Target_Direction (At_Time => Time.Universal);
      use type Space.Direction;
      Delta_Direction  : constant Space.Direction := Actual_Direction - The_Start_Direction;
    begin
      if Space.Direction_Is_Known (Delta_Direction) then
        Mount.Update_Target (Delta_Direction);
      end if;
    end Update_Target_Direction;


    procedure Setup_Handling is
    begin
      case The_User_Setup is
      when Previous_Speed =>
        Change_Moving_Speed (-1);
      when Next_Speed =>
        Change_Moving_Speed (+1);
      when Rotate =>
        M3.Rotate;
      end case;
    end Setup_Handling;


    procedure Enabling is
      Enabling_Duration : constant Duration := 120.0;
      use type Time.Ut;
    begin
      Mount.Enable;
      if Cdk_700.Had_Powerup then
        The_Completion_Time := Time.Universal + Enabling_Duration;
        Focuser.Find_Home;
        Rotator.Find_Home;
      else
        The_Completion_Time := Time.In_The_Past;
      end if;
      The_State := Enabling;
    end Enabling;


    procedure Find_Home_And_Set_Defaults is
    begin
      Fans.Turn (To => Fans.Off);
      M3.Turn_To_Occular;
      Mount.Find_Home (The_Completion_Time);
      Focuser.Go_To (Focuser.Stored_Position);
      Rotator.Goto_Mech (180.0);
      The_State := Homing;
    end Find_Home_And_Set_Defaults;


    function Solve_Picture return Boolean is
    begin
      if not Picture.Solve (Mount.Actual_Info.Actual_Direction) then
        Log.Warning ("solve picture not started");
        return False;
      end if;
      return True;
    exception
    when Picture.Not_Solved =>
      Log.Warning ("solve picture terminated");
      return False;
    when Item: others =>
      Log.Error ("solve picture failed");
      Log.Termination (Item);
      return False;
    end Solve_Picture;


    procedure Picture_Solving is
      The_Direction : Space.Direction;
    begin
      if Picture.Solved then
        The_State := Tracking;
        The_Direction := Picture.Direction;
        Mount.Add_To_Model (The_Direction);
        Log.Write ("Added to Model Picture Direction: " & Space.Image_Of (The_Direction));
      end if;
    exception
    when Picture.Not_Solved =>
      The_State := Tracking;
    end Picture_Solving;


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
        if The_Focuser_State > Focuser.Disconnected then
          return Connected;
        else
          return Disconnected;
        end if;
      when Focuser_Connected | Focuser_Moving =>
        if The_Mount_State = Mount.Connected then
          return Connected;
        else
          return The_State;
        end if;
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
        if Site.Verified (Device.Site_Info) then
          Mount.Connect;
          Focuser.Connect;
          The_State := Connecting;
        else
          Error.Set ("Incorrect Location");
          The_State := Mount_Error;
        end if;
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
        if Is_Closing then
          Is_Closing := False;
          Gui.Close;
        else
          The_State := Disconnected;
        end if;
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
      if not Is_Ending then
        Is_Ending := True;
        Remote.Define (Is_On_Target => False);
        User.Show_Error;
        Gui.Close;
      end if;
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
        if The_Focuser_State > Focuser.Disconnected then
          Enabling;
        end if;
      when Focuser_Connected =>
        if The_Mount_State > Mount.Disconnected then
          Enabling;
        end if;
      when Focuser_Moving =>
        raise Program_Error;
      when Mount_Enabled =>
        Find_Home_And_Set_Defaults;
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
        Enabling;
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
        Find_Home_And_Set_Defaults;
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
        Find_Home_And_Set_Defaults;
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
        Log.Write ("Site LMST: " & Time.Image_Of (Device.Site_Lmst));
        Log.Write ("Time LMST: " & Time.Image_Of (Time.Lmst));
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
        Is_Closing := True;
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
          Follow_Target (Id);
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
      when User_Setup =>
        Setup_Handling;
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

    --------------
    -- Solving --
    --------------
    procedure Solving_State is
    begin
      case The_Event is
      when Mount_Startup =>
        Picture.Stop_Solving;
        The_State := Mount_Startup_State (The_Event);
      when Mount_Stopped =>
        Picture.Stop_Solving;
        The_State := Stopped;
      when Mount_Approach =>
        Picture.Stop_Solving;
        The_State := Approaching;
      when Halt =>
        Picture.Stop_Solving;
        Stop_Target;
      when others =>
        null;
      end case;
    end Solving_State;

    Has_New_Data : Boolean := True;

    The_Next_Time : Ada.Real_Time.Time;

    use type Ada.Real_Time.Time;

  begin -- Control_Task
    Reset_Adjustments;
    accept Start do
      Device.Start (Mount_State_Handler'access,
                    Focuser_State_Handler'access,
                    M3_Position_Handler'access);
    end Start;
    Log.Write ("Started");
    if Cdk_700.Had_Powerup then
      The_State := Restarting;
    else
      The_State := Disconnected;
    end if;
    The_Next_Time := Ada.Real_Time.Clock;
    loop
      begin
        The_Event := No_Event;
        select
          accept Close;
          exit;
        or
          accept Go_Back;
          Reset_Adjustments;
          The_Event := Back;
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
          accept Focuser_Goto (The_Position : Device.Microns) do
            Focuser.Go_To (The_Position);
          end Focuser_Goto;
        or
          accept Rotator_Goto_Field (The_Angle : Device.Degrees) do
            Rotator.Goto_Field (The_Angle);
          end Rotator_Goto_Field;
        or
          accept Rotator_Goto_Mech (The_Position : Device.Degrees) do
            Rotator.Goto_Mech (The_Position);
          end Rotator_Goto_Mech;
        or
          accept Rotator_Goto (The_Offset : Device.Degrees) do
            Rotator.Go_To (The_Offset);
          end Rotator_Goto;
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
            The_Mount_State := New_State;
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
              Error.Set (Device.Error_Info);
              The_Event := Mount_Error;
            end case;
            Has_New_Data := True;
          end New_Mount_State;
        or
          accept New_Focuser_State (New_State : Focuser.State) do
            Log.Write ("Focuser State " & New_State'img);
            The_Focuser_State := New_State;
            case New_State is
            when Focuser.Unknown | Focuser.Disconnected =>
              null;
            when Focuser.Connected =>
              The_Event := Focuser_Connected;
              Has_New_Data := True;
            when Focuser.Moving =>
              The_Event := Focuser_Moving;
              Has_New_Data := True;
            end case;
          end New_Focuser_State;
        or
          accept New_M3_Position (New_Position : M3.Position) do
            Log.Write ("M3 Position " & New_Position'img);
            The_M3_Position := New_Position;
            Has_New_Data := True;
          end New_M3_Position;
        or
          accept Get (The_Data : out Data) do
            if The_State = Restarting then
              if Cdk_700.Is_Started then
                The_State := Disconnected;
                Log.Write ("State => " & The_State'img);
              end if;
            end if;
            The_Data.Status := The_State;
            The_Data.M3.Position := The_M3_Position;
            The_Data.Focuser.Exists := Focuser.Exists;
            The_Data.Focuser.Moving := The_Focuser_State = Focuser.Moving;
            The_Data.Focuser.Position := Focuser.Actual_Position;
            if The_State >= Stopped then
              Focuser.Stored_Position := The_Data.Focuser.Position;
            end if;
            The_Data.Focuser.Max_Position := Max_Fucuser_Position;
            The_Data.Focuser.Zoom_Size := Fucuser_Zoom_Size;
            The_Data.Rotator.Moving := Rotator.Moving;
            The_Data.Rotator.Slewing := Rotator.Slewing;
            The_Data.Rotator.Field_Angle := Rotator.Field_Angle;
            The_Data.Rotator.Mech_Position := Rotator.Mech_Position;
            case The_State is
            when Approaching | Preparing | Positioning | Homing | Enabling =>
              The_Data.Completion_Time := The_Completion_Time;
            when others =>
              The_Data.Completion_Time := Time.In_The_Past;
            end case;
            case The_State is
            when Startup_State =>
              The_Data.Actual_Direction := Space.Unknown_Direction;
            when others =>
              declare
                Info : constant Mount.Information := Mount.Actual_Info;
              begin
                The_Data.Actual_Direction := Info.Actual_Direction;
                The_Data.Mount.Axis0 := Info.Az_Axis.Position;
                The_Data.Mount.Axis1 := Info.Alt_Axis.Position;
                The_Data.Mount.Model_Points := Info.Model.Points_Enabled;
              end;
            end case;
            The_Data.Target_Lost := Target_Is_Lost;
            Target_Is_Lost := False;
            The_Data.Moving_Speed := Moving_Speeds(Moving_Index);
            Set_Server_Information (The_Data);
          end Get;
        or
          delay until The_Next_Time;
          The_Next_Time := The_Next_Time + Ada.Real_Time.To_Time_Span(1.0);
          case The_State is
          when Tracking =>
            Update_Target_Direction;
            if Picture.Exists and then Solve_Picture then
              The_State := Solving;
            end if;
          when Solving =>
            Picture_Solving;
          when Waiting =>
            The_Event := Mount_Stopped;
          when Mount_Error =>
            Error_State;
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
          when Solving       => Solving_State;
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
    Input.Close;
    Device.Finalize;
    Log.Write ("Control end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control_Task;

end Telescope;
