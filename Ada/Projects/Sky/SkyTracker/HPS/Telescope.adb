-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Camera;
with Clock;
with Http_Server;
with Input;
with Picture;
with Pole_Axis;
with Remote;
with Text;
with Traces;
with User;
with Weather;

package body Telescope is

  package Log is new Traces ("Telescope");

  package RT renames Ada.Real_Time;

  subtype Command is Input.Mount_Command;

  use all type Input.Command;

  task type Control_Task is

    entry Align;

    entry Go_To;

    entry Go_To_Left;

    entry Go_To_Right;

    entry Go_To_Top;

    entry Go_To_Next;

    entry Park;

    entry Prepare_Tle;

    entry Stop;

    entry Unpark;

    entry Execute (The_Command : Command);

    entry Get (The_Data : out Data);

    entry Close;

  end Control_Task;


  type Control_Access is access Control_Task;

  Control : Control_Access;

  Signal_Information_Update : Information_Update_Handler;

  procedure Set_Server_Information (The_Data : Data) is

    function Mount_Data return Http_Server.Mount_Data is
      (if not (The_Data.Status in Disconnected | Unknown) then
        (Exists       => True,
         Axis0        => 0.0, -- not implemented
         Axis1        => 0.0, -- not implemented
         Model_Points => 0)   -- not implemented
       else
         (others => <>));

  begin -- Set_Server_Information
    Http_Server.Set_State (Text.Legible_Of (The_Data.Status'image));
    Http_Server.Set_Moving (Speed => The_Data.Moving_Speed);
    Http_Server.Set (Mount_Data);
  end Set_Server_Information;


  procedure Execute (The_Command : Input.Command) is
  begin
    case The_Command is
    when Input.Mount_Command =>
      Control.Execute (The_Command);
    when Go_Back =>
      User.Perform_Goto;
    when Stop =>
      User.Perform_Stop;
    end case;
  end Execute;


  procedure Start (Update_Handler : Information_Update_Handler) is
  begin
    Input.Open (Execute'access);
    Signal_Information_Update := Update_Handler;
    Control := new Control_Task;
  end Start;


  procedure Align is
  begin
    Control.Align;
  end Align;


  procedure Go_To is
  begin
    Control.Go_To;
  end Go_To;


  procedure Go_To_Left is
  begin
    Control.Go_To_Left;
  end Go_To_Left;


  procedure Go_To_Right is
  begin
    Control.Go_To_Right;
  end Go_To_Right;


  procedure Go_To_Top is
  begin
    Control.Go_To_Top;
  end Go_To_Top;


  procedure Go_To_Next is
  begin
    Control.Go_To_Next;
  end Go_To_Next;


  procedure Prepare_Tle is
  begin
    Control.Prepare_Tle;
  end Prepare_Tle;


  procedure Park is
  begin
    Control.Park;
  end Park;


  procedure Stop is
  begin
    Control.Stop;
  end Stop;


  procedure Unpark is
  begin
    Control.Unpark;
  end Unpark;


  Next_Id            : Name.Id;
  Next_Get_Direction : Get_Space_Access := null;
  The_Next_Star      : Space.Direction;


  function Target_Kind return Ten_Micron.Target_Kind is
  begin
    if Name.Is_Known (Next_Id) then
      case Name.Kind_Of (Next_Id) is
      when Name.Axis_Position =>
        return Ten_Micron.Axis_Position;
      when Name.Near_Earth_Object =>
        return Ten_Micron.Near_Earth_Object;
      when others =>
        return Ten_Micron.Other_Targets;
      end case;
    else
      return Ten_Micron.Other_Targets;
    end if;
  end Target_Kind;


  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id) is
  begin
    Next_Id := The_Id;
    Next_Get_Direction := Get_Direction;
  end Define_Space_Access;


  function Actual_Target_Direction return Space.Direction is
  begin
    if User.In_Setup_Mode then
      if Space.Direction_Is_Known (The_Next_Star) then
        return The_Next_Star;
      else
        return Space.Pole_Search_Direction;
      end if;
    elsif Next_Get_Direction = null then
      return Space.Unknown_Direction;
    else
      return Next_Get_Direction (Next_Id, Time.Universal);
    end if;
  end Actual_Target_Direction;


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

    Time_Update_Interval : constant RT.Time_Span := RT.Minutes(10);

    Aligning_Enabled : Boolean := False;

    Is_Preparing_For_Capture : Boolean := False;

    The_Information : Ten_Micron.Information;

    The_Picture_Direction : Space.Direction;
    The_Picture_Lmst      : Time.Value;

    Evaluate_Pole_Setup : access procedure;


    type Timer_State is (Stopped, Increasing, Decreasing);

    The_Timer_State : Timer_State := Stopped;

    The_Start_Time : RT.Time;
    The_Delta_Time : Time_Offset := 0.0;

    The_Next_Update_Time : RT.Time := RT.Clock;


    function "-" (Left, Right : RT.Time) return Duration is
      use type RT.Time;
    begin
      return RT.To_Duration (Left - Right);
    end "-";


    procedure Update_Offset (Now         : RT.Time;
                             Time_Change : Duration) is
      Offset : Time_Offset;
      use type Time_Offset;
    begin
      The_Start_Time := Now;
      Offset := Time_Offset(Time_Change / 50.0);
      if Ten_Micron.Updated (Offset) then
        The_Delta_Time := @ + Offset;
        The_Start_Time := Now;
      end if;
    exception
    when others =>
      null;
    end Update_Offset;


    procedure Update_Time_Delta is
      Now : constant RT.Time := RT.Clock;
    begin
      if The_Information.Status in Transit_State | Following then
        case The_Timer_State is
        when Increasing =>
          Update_Offset (Now, Now - The_Start_Time);
        when Decreasing =>
          Update_Offset (Now, The_Start_Time - Now);
        when Stopped =>
          null;
        end case;
      else
        The_Timer_State := Stopped;
        The_Delta_Time := 0.0;
      end if;
    end Update_Time_Delta;


    function Is_Satellite return Boolean is
    begin
      if The_Information.Status in Transit_State | Following then
        return True;
      end if;
      The_Delta_Time := 0.0;
      return False;
    end Is_Satellite;


    procedure Start_Increase_Time is
    begin
      if Is_Satellite then
        The_Timer_State := Increasing;
        The_Start_Time := RT.Clock;
      end if;
    end Start_Increase_Time;


    procedure Start_Decrease_Time is
    begin
      if Is_Satellite then
        The_Timer_State := Decreasing;
        The_Start_Time := RT.Clock;
      end if;
    end Start_Decrease_Time;


    procedure End_Change_Time is
    begin
      if Is_Satellite then
        Update_Time_Delta;
        The_Timer_State := Stopped;
      end if;
    end End_Change_Time;


    procedure Get_Information is
    begin
      if The_Information.Status = Disconnected then
        Ten_Micron.Startup;
      end if;
      The_Information := Ten_Micron.Get;
      if The_Information.Status /= Disconnected and then User.In_Setup_Mode then
        Alignment.Update_Info;
      end if;
    end Get_Information;


    function Solve_Picture return Boolean is
    begin
      if not Picture.Solve (Actual_Target_Direction) then
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


    procedure Update_Time_And_Weather_Data is
      use type RT.Time;
    begin
      if The_Next_Update_Time < RT.Clock then
        The_Next_Update_Time := RT.Clock + Time_Update_Interval;
        Clock.Define_Time;
        if Weather.Requested then
          Ten_Micron.Define (Weather.Air_Pressure);
          Ten_Micron.Define (Weather.Temperature);
        end if;
      end if;
    end Update_Time_And_Weather_Data;


    procedure Update_Handling is

      procedure Capture_Handling is
      begin
        if Is_Preparing_For_Capture then
          Ten_Micron.Start_Capturing;
          Camera.Capture;
          Is_Preparing_For_Capture := False;
        end if;
      end Capture_Handling;

    begin -- Update_Handling
      Get_Information;
      Update_Time_Delta;
      case The_Information.Status is
      when Positioned =>
        Update_Time_And_Weather_Data;
        if User.In_Setup_Mode then
          Capture_Handling;
        end if;
      when Tracking =>
        if User.In_Setup_Mode then
          Capture_Handling;
        elsif not Aligning_Enabled and then Picture.Exists and then Solve_Picture then
          Ten_Micron.Start_Solving;
        end if;
      when Capturing =>
        if Picture.Exists and then Solve_Picture then
          Ten_Micron.Start_Solving;
        end if;
      when Solving =>
        begin
          if Picture.Solved then
            Ten_Micron.End_Solving;
            Picture.Evaluate (Center => The_Picture_Direction,
                              Lmst   => The_Picture_Lmst);
            if User.In_Setup_Mode then
              if Evaluate_Pole_Setup /= null then
                Evaluate_Pole_Setup.all;
              else
                Alignment.Define (Direction => The_Picture_Direction,
                                  Lmst      => The_Picture_Lmst,
                                  Pier_Side => The_Information.Pier_Side);
                User.Perform_Goto_Next;
              end if;
            else
              User.Enable_Align_On_Picture;
              Aligning_Enabled := True;
            end if;
          end if;
        exception
        when Picture.Not_Solved =>
          Ten_Micron.End_Solving;
          if User.In_Setup_Mode and Evaluate_Pole_Setup = null then
            User.Perform_Goto_Next;
          end if;
        end;
      when Stopped | Waiting =>
        Aligning_Enabled := False;
        Is_Preparing_For_Capture := False;
        Update_Time_And_Weather_Data;
      when Disconnected =>
        Aligning_Enabled := False;
        Is_Preparing_For_Capture := False;
        Picture.Stop_Solving;
      when others =>
        Aligning_Enabled := False;
      end case;
      Signal_Information_Update.all;
    end Update_Handling;


    procedure Synch_On_Picture is
    begin
      Aligning_Enabled := False;
      Ten_Micron.Synch_To (The_Picture_Direction);
      The_Picture_Direction := Space.Unknown_Direction;
    end Synch_On_Picture;


    procedure Position_To (The_Direction : Space.Direction) is
    begin
      Alignment.Clear;
      The_Next_Star := Space.Unknown_Direction;
      Ten_Micron.Slew_To (The_Direction, Ten_Micron.Axis_Position);
      Remote.Define (Target => "");
    end Position_To;

  begin -- Control_Task
    loop
      begin
        select
          accept Close;
          exit;
        or
          accept Align do
            if Alignment.Star_Count > 0 then
              Alignment.Generate;
            else
              Synch_On_Picture;
              Ten_Micron.Slew_To (Actual_Target_Direction, Target_Kind);
            end if;
          end Align;
        or
          accept Go_To do
            Evaluate_Pole_Setup := null;
            Alignment.Clear;
            Ten_Micron.Slew_To (Actual_Target_Direction, Target_Kind);
            if Name.Is_Known (Next_Id) then
              case Name.Kind_Of (Next_Id) is
              when Name.Axis_Position =>
                Remote.Define (Target => "");
              when others =>
                Remote.Define (Target => Name.Image_Of (Next_Id));
              end case;
            else
              Remote.Define (Target => "");
            end if;
          end Go_To;
        or
          accept Go_To_Left do
            Evaluate_Pole_Setup := Pole_Axis.Evaluate_Left'access;
            Position_To (Space.Axis_Pole_Left);
            Is_Preparing_For_Capture := True;
          end Go_To_Left;
        or
          accept Go_To_Right do
            Evaluate_Pole_Setup := Pole_Axis.Evaluate_Right'access;
            Position_To (Space.Axis_Pole_Right);
            Is_Preparing_For_Capture := True;
          end Go_To_Right;
        or
          accept Go_To_Top do
            Evaluate_Pole_Setup := Pole_Axis.Evaluate_Top'access;
            Position_To (Space.Axis_Pole_Top);
            Is_Preparing_For_Capture := True;
          end Go_To_Top;
        or
          accept Go_To_Next do
            Evaluate_Pole_Setup := null;
            The_Picture_Direction := Space.Unknown_Direction;
            The_Next_Star := Alignment.Next_Star;
            if Space.Direction_Is_Known (The_Next_Star) then
              Ten_Micron.Slew_To (The_Next_Star);
              Is_Preparing_For_Capture := True;
            else
              Ten_Micron.Stop;
            end if;
          end Go_To_Next;
        or
          accept Prepare_Tle do
            Ten_Micron.Load_Tle (Name.Image_Of (Next_Id));
          end Prepare_Tle;
        or
          accept Park do
            Remote.Define (Target => "");
            Ten_Micron.Park;
          end Park;
        or
          accept Stop do
            Remote.Define (Target => "");
            case The_Information.Status is
            when Solving =>
              Picture.Stop_Solving;
            when Capturing =>
              Camera.Stop;
            when others =>
              null;
            end case;
            Ten_Micron.Stop;
           end Stop;
        or
          accept Unpark do
            Remote.Define (Target => "");
            Ten_Micron.Unpark;
            Update_Time_And_Weather_Data;
          end Unpark;
        or
          accept Execute (The_Command : Command) do
            case The_Command is
            when Move_Left =>
              Ten_Micron.Execute (Ten_Micron.Move_Left);
            when Move_Right =>
              Ten_Micron.Execute (Ten_Micron.Move_Right);
            when Move_Up =>
              Ten_Micron.Execute (Ten_Micron.Move_Up);
            when Move_Down =>
              Ten_Micron.Execute (Ten_Micron.Move_Down);
            when End_Command =>
              Ten_Micron.Execute (Ten_Micron.Move_End);
            when Spiral_Offset_Center | Spiral_Offset_Next | Spiral_Offset_Previous =>
              null; -- not implemented
            when Start_Time_Increase =>
              Start_Increase_Time;
            when Start_Time_Decrease =>
              Start_Decrease_Time;
            when End_Time_Change =>
              End_Change_Time;
            when Add_Point =>
              null; -- not implemented
            when Next_Speed =>
              Ten_Micron.Execute (Ten_Micron.Increase_Moving_Rate);
            when Previous_Speed =>
              Ten_Micron.Execute (Ten_Micron.Decrease_Moving_Rate);
            when Rotate =>
              null; -- not implemented
            end case;
          end Execute;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := Telescope.State(The_Information.Status);
            The_Data.Target_Direction := Actual_Target_Direction;
            The_Data.Actual_Direction := The_Information.Direction;
            The_Data.Actual_Position := The_Information.Position;
            The_Data.Picture_Direction := The_Picture_Direction;
            The_Data.Mount_Pier_Side := The_Information.Pier_Side;
            The_Data.Moving_Speed := The_Information.Moving_Speed;
            The_Data.Universal_Time := The_Information.Date_Time;
            The_Data.Time_Delta := The_Delta_Time;
            The_Data.Align_Points := Alignment.Star_Count;
            The_Data.Alignment_Info := Alignment.Info;
            The_Data.Cone_Error := Pole_Axis.Cone_Error;
            The_Data.Pole_Offsets := Pole_Axis.Offsets;
            Set_Server_Information (The_Data);
          end Get;
        or delay 0.5;
          Update_Handling;
        end select;
        Remote.Define (Is_On_Target => The_Information.Status in Tracking | Following);
      exception
      when Item: others =>
        Log.Termination (Item);
      end;
    end loop;
    Input.Close;
    Log.Write ("end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control_Task;

end Telescope;
