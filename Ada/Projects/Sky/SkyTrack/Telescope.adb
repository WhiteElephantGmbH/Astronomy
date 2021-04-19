-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Alignment;
with Device;
with Matrix;
with Numerics;
with Parameter;
with Picture;
with Site;
with System;
with Traces;
with User;

package body Telescope is

  package Log is new Traces ("Telescope");


  task type Control_Task with Priority => System.Max_Priority is

    entry Start;

    entry Execute (The_Command : Command);

    entry Set (The_Orientation : Orientation);

    entry Halt;

    entry Follow (Arriving_Time : Time.Ut);

    entry Position_To (Landmark : Name.Id);

    entry Align;

    entry Synch_On_Target;

    entry Synch_Park_Position;

    entry Park;

    entry New_Motor_State (New_State : Motor.State);

    entry Get (The_Data : out Data);

    entry Close;

  end Control_Task;


  type Control_Access is access Control_Task;

  Control : Control_Access;


  procedure Motor_State_Handler (New_State : Motor.State) is
  begin
    Control.New_Motor_State (New_State);
  end Motor_State_Handler;


  Signal_Information_Update : Information_Update_Handler;

  procedure Start (Update_Handler : Information_Update_Handler) is
  begin -- Start
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


  procedure Align is
  begin
    Control.Align;
  end Align;


  Next_Id            : Name.Id;
  Next_Get_Direction : Get_Space_Access;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id) is
  begin
    Next_Id := The_Id;
    Next_Get_Direction := Get_Direction;
  end Define_Space_Access;


  function Actual_Target_Direction return Space.Direction is
  begin
    return Next_Get_Direction (Next_Id, Time.Universal);
  end Actual_Target_Direction;


  procedure Synch_On_Target is
  begin
    Control.Synch_On_Target;
  end Synch_On_Target;


  function Synch_On_Picture return Boolean is
    Actual_Direction : constant Space.Direction := Actual_Target_Direction;
  begin
    return Picture.Solve (Actual_Direction);
  exception
  when Picture.Not_Solved =>
    Log.Warning ("No sulution for synch on picture");
    return False;
  when Item: others =>
    Log.Termination (Item);
    return False;
  end Synch_On_Picture;


  function Synch_On_Picture_Complete return Boolean is
    use type Space.Direction;
    use type Angle.Signed;
    use type Angle.Value;
    Actual_Direction : constant Space.Direction := Actual_Target_Direction;
    The_Direction    : Space.Direction;
  begin
    if Picture.Solved then
      The_Direction := Actual_Direction - Picture.Actual_Direction;
      Alignment.Set (Ra_Offset  => Angle.Degrees'(+Angle.Signed'(+Space.Ra_Of (The_Direction))),
                     Dec_Offset => Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (The_Direction))));
      User.Enable_Align_On_Picture;
      return True;
    end if;
    return False;
  exception
  when Picture.Not_Solved =>
    Log.Warning ("No sulution for synch on picture");
    return True;
  when Item: others =>
    Log.Termination (Item);
    return True;
  end Synch_On_Picture_Complete;


  procedure Synch_Park_Position is
  begin
    Control.Synch_Park_Position;
  end Synch_Park_Position;


  procedure Park is
  begin
    Control.Park;
  end Park;


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

    The_Current_Time       : Time.Ut := Time.In_The_Past;
    The_Arriving_Time      : Time.Ut := Time.In_The_Future;
    The_Time_When_Stopped  : Time.Ut := Time.In_The_Future;
    The_Next_Arriving_Time : Time.Ut := Time.In_The_Past;

    The_Landmark : Name.Id;

    type Adjusting_Kind is (First_Adjusting, Second_Adjusting, Time_Adjusting);

    Adjusting_Stopped : constant Angle.Signed := 0;

    The_Adjusting_Kind        : Adjusting_Kind;
    The_Adjusting_Start_Time  : Time.Ut := Time.In_The_Future;
    The_Adjusting_End_Time    : Time.Ut := Time.In_The_Future;
    The_Time_Adjusting_Factor : Duration;
    The_Time_Adjustment       : Duration := 0.0;

    type Event is (No_Event,
                   Align,
                   Halt,
                   Synch_On_Target,
                   Synch_Park_Position,
                   Park,
                   Follow,
                   Position,
                   User_Command,
                   Motor_Disconnected,
                   Motor_Fault,
                   Motor_Startup,
                   Motor_Ready,
                   Motor_Parked,
                   Motor_Parking,
                   Motor_Positioned,
                   Motor_Positioning,
                   Motor_Directing,
                   Time_Increment);

    The_State  : State := Disconnected;
    The_Event  : Event := No_Event;

    Event_After_Stop : Event := No_Event;

    The_User_Command         : Command;
    Is_Aligning              : Boolean := False;
    Picture_Aligning_Enabled : Boolean := False;

    Id            : Name.Id;
    Get_Direction : Get_Space_Access;

    Tb : Time.Ut := Time.In_The_Past;
    Te : Time.Ut := Time.In_The_Past;
    Tn : Time.Ut := Time.In_The_Past;

    Pb : Motor.Position;
    Pe : Motor.Position;
    Pn : Motor.Position;

    Target_Lost    : exception;
    Target_Is_Lost : Boolean := False;


    function Position_At (The_Time : Time.Ut) return Motor.Position is
      Target_Direction : constant Space.Direction := Get_Direction (Id, The_Time + The_Time_Adjustment);
      use type Space.Direction;
    begin
      if Target_Direction = Space.Unknown_Direction then
        Target_Is_Lost := True;
        raise Target_Lost;
      end if;
      return Numerics.Position_Of (Direction => Target_Direction,
                                   Rotations => Alignment.Corrections,
                                   At_Time   => The_Time + The_Time_Adjustment);
    end Position_At;


    function Goto_Target return Boolean is
      At_Time   : Time.Ut := Te;
      use type Motor.Position;
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target;
      end if;
      The_Arriving_Time := Motor.Arriving_Time (At_Position => Position_At (Te),
                                                With_Speed  => Position_At (Tn) -
                                                               Position_At (Tb));
      for Count in 1 .. 42 loop
        if abs (At_Time - The_Arriving_Time) < Time.Epsilon then
          if Count > 21 then
            Log.Warning ("count =>" & Count'img);
          end if;
          if Motor.Approach_Target_At (The_Position => Position_At (The_Arriving_Time),
                                       With_Speed   => Position_At (The_Arriving_Time + Motor.Time_Delta) -
                                                       Position_At (The_Arriving_Time - Motor.Time_Delta))
          then
            The_State := Approaching;
            return True;
          end if;
          Log.Warning ("approach failed");
          At_Time := At_Time + 10.0; -- Try in future
        end if;
        if The_Arriving_Time = Time.In_The_Future then
          At_Time := At_Time + 3.0;
        else
          At_Time := The_Arriving_Time;
        end if;
        begin
          The_Arriving_Time := Motor.Arriving_Time (At_Position => Position_At (At_Time),
                                                    With_Speed  => Position_At (At_Time + Motor.Time_Delta) -
                                                                   Position_At (At_Time - Motor.Time_Delta));
        exception
        when Target_Lost =>
          raise;
        when others =>
          Log.Warning ("speed too high (try later)");
          At_Time := At_Time + 3.0;
          The_Arriving_Time := Motor.Arriving_Time (At_Position => Position_At (At_Time),
                                                    With_Speed  => Position_At (At_Time + Motor.Time_Delta) -
                                                                   Position_At (At_Time - Motor.Time_Delta));
        end;
      end loop;
      Log.Warning ("approching target not possible");
      return False;
    exception
    when Motor.At_Limit =>
      Log.Warning ("Goto_Target: at limit");
      return False;
    when Target_Lost =>
      Log.Warning ("target lost");
      return False;
    end Goto_Target;


    function Following_Target return Boolean is
      use type Motor.Position;
    begin
      if Get_Direction = null then
        raise Program_Error; -- unknown target
      end if;
      return Motor.Following (To          => Pe,
                              Final_Speed => Pn - Pb,
                              At_Time     => Te);
    exception
    when Motor.At_Limit =>
      Log.Warning ("Following_Target: at limit");
      return False;
    end Following_Target;


    function Goto_Waiting_Position_Or_Target return Boolean is
      Waiting_Position : constant Motor.Position := Numerics.Position_Of (Get_Direction (Id, The_Arriving_Time),
                                                                          Alignment.Corrections,
                                                                          The_Arriving_Time);
    begin
      The_Time_When_Stopped := Motor.Time_When_At (Waiting_Position);
      if The_Time_When_Stopped < The_Arriving_Time and then Motor.Approach_Target_At (Waiting_Position) then
        The_State := Preparing;
        return True;
      else
        return Goto_Target;
      end if;
    exception
    when Motor.At_Limit =>
      Log.Warning ("Goto_Waiting_Position_Or_Target: at limit");
      return False;
    end Goto_Waiting_Position_Or_Target;


    Moving_Speeds   : constant Angle.Values := Parameter.Moving_Speeds;
    Adjusting_Index : Integer := Moving_Speeds'first + 1;
    Directing_Index : Integer := Moving_Speeds'last - 1;

    procedure Set_Directing_Speed (Index : Integer) is
    begin
      Directing_Index := Index;
      if Directing_Index < Moving_Speeds'first then
        Directing_Index := Moving_Speeds'first;
      elsif Directing_Index > Moving_Speeds'last then
        Directing_Index := Moving_Speeds'last;
      else
        Log.Write ("set directing speed => " & Angle.Image_Of (Moving_Speeds(Directing_Index),
                                                               Decimals => 3,
                                                               Show_Signed => True));
      end if;
    end Set_Directing_Speed;


    procedure Change_Directing_Speed (Increment : Integer) is
    begin
      Set_Directing_Speed (Directing_Index + Increment);
    end Change_Directing_Speed;


    procedure Set_Adjusting_Speed (Index : Integer) is
    begin
      Adjusting_Index := Index;
      if Adjusting_Index < Moving_Speeds'first then
        Adjusting_Index := Moving_Speeds'first;
      elsif Adjusting_Index > Moving_Speeds'last - 1 then
        Adjusting_Index := Moving_Speeds'last - 1;
      else
        Log.Write ("set adjusting speed => " & Angle.Image_Of (Moving_Speeds(Adjusting_Index),
                                                               Decimals => 3,
                                                               Show_Signed => True));
      end if;
    end Set_Adjusting_Speed;


    procedure Change_Adjusting_Speed (Increment : Integer) is
    begin
      Set_Adjusting_Speed (Adjusting_Index + Increment);
    end Change_Adjusting_Speed;


    function Follow_New_Target return Boolean is
    begin
      Id := Next_Id;
      Get_Direction := Next_Get_Direction;
      Log.Write ("follow from " & The_State'img & " after " & Time.Image_Of (The_Next_Arriving_Time));
      The_Arriving_Time := The_Next_Arriving_Time;
      Motor.Allow_Inversion;
      if The_Arriving_Time > Tb then -- arriving in future
        return Goto_Waiting_Position_Or_Target;
      else
        return Goto_Target;
      end if;
    end Follow_New_Target;


    procedure Time_Control_End is
    begin
      The_Time_Adjustment := 0.0;
      The_Current_Time := Time.In_The_Past;
      Is_Aligning := False;
    end Time_Control_End;


    procedure Set_State (Item : State) is
    begin
      The_State := Item;
    end Set_State;


    procedure Set_Stopping is
    begin
      Picture_Aligning_Enabled := False;
      Picture.Stop_Solving;
      Set_State (Stopping);
    end Set_Stopping;


    procedure Stop_And_Park is
    begin
      Motor.Stop;
      Set_Stopping;
      Event_After_Stop := Park;
    end Stop_And_Park;


    procedure Stop_And_Position is
    begin
      Motor.Stop;
      Set_Stopping;
      Event_After_Stop := Position;
    end Stop_And_Position;


    procedure Stop_And_Synch is
    begin
      Motor.Stop;
      Set_Stopping;
      Event_After_Stop := Synch_On_Target;
    end Stop_And_Synch;


    procedure Stop_And_Follow is
    begin
      Motor.Stop;
      Set_Stopping;
      Event_After_Stop := Follow;
    end Stop_And_Follow;


    procedure Stop_Target is
    begin
      if Event_After_Stop /= No_Event then
        Event_After_Stop := No_Event;
      else
        Motor.Stop;
      end if;
      Set_Stopping;
    end Stop_Target;


    procedure Do_Position is
    begin
      The_Time_When_Stopped := Motor.Time_When_Positioned (Numerics.Position_Of (Name.Direction_Of (The_Landmark)));
      if The_Time_When_Stopped > Time.In_The_Past then
        Set_State (Positioning);
      end if;
    end Do_Position;


    procedure Do_Park is
    begin
      Alignment.Clear_Corrections;
      The_Time_When_Stopped := Motor.Time_When_Parked;
      Set_State (Parking);
    end Do_Park;


    procedure Do_Synch_On_Target is
      Target_Direction : Space.Direction;
      use type Space.Direction;
    begin
      Set_State (Stopped);
      if Next_Get_Direction = null then
        Log.Error ("undefined synch target");
      else
        Target_Direction := Actual_Target_Direction;
        if Target_Direction = Space.Unknown_Direction then
          Log.Error ("undefined synch target direction");
        else
          Motor.Synch_Position (Numerics.Position_Of (Direction => Target_Direction,
                                                      Rotations => Alignment.Corrections,
                                                      At_Time   => Time.Universal));
          User.Perform_Goto;
        end if;
      end if;
    end Do_Synch_On_Target;


    procedure Synchronize_Time is
    begin
      Motor.Synchronize (The_Current_Time);
      Log.Write ("start time => " & Time.Image_Of (The_Current_Time));
    end Synchronize_Time;


    procedure Handle_Positioned is
    begin
      Set_State (Stopped);
      Time_Control_End;
      case Event_After_Stop is
      when Park =>
        Do_Park;
      when Position =>
        Do_Position;
      when Synch_On_Target =>
        Do_Synch_On_Target;
      when Follow =>
        Synchronize_Time;
      when others =>
        null;
      end case;
      Event_After_Stop := No_Event;
    end Handle_Positioned;


    procedure Direct_Handling is
      Speed : constant Angle.Value := Moving_Speeds(Directing_Index);
      use type Angle.Signed;
    begin
      case The_User_Command is
      when Move_Left =>
        Motor.Direct (Device.D1, -Speed);
      when Move_Right =>
        Motor.Direct (Device.D1, +Speed);
      when Move_Up =>
        Motor.Direct (Device.D2, +Speed);
      when Move_Down =>
        Motor.Direct (Device.D2, -Speed);
      when End_Move =>
        Motor.Stop;
      when Increase =>
        Change_Directing_Speed (+1);
      when Decrease =>
        Change_Directing_Speed (-1);
      when End_Change =>
        null;
      when Set_Guiding_Rate =>
        Set_Directing_Speed (Moving_Speeds'first);
      when Set_Centering_Rate =>
        Set_Directing_Speed (Moving_Speeds'first + 1);
      when Set_Finding_Rate =>
        Set_Directing_Speed (Moving_Speeds'last - 1);
      when Set_Slewing_Rate =>
        Set_Directing_Speed (Moving_Speeds'last);
      end case;
    end Direct_Handling;


    procedure End_Direct_Handling is
    begin
      case The_User_Command is
      when End_Move =>
        Motor.Stop;
      when others =>
        null;
      end case;
    end End_Direct_Handling;


    First_Adjust_Factor : Angle.Signed := 1;

    procedure Adjust_First (The_Speed : Angle.Signed) is
      use type Angle.Signed;
    begin
      Motor.Adjust (Device.D1, The_Speed * First_Adjust_Factor);
      The_Adjusting_Kind := First_Adjusting;
      Set_State (Adjusting);
    end Adjust_First;


    Second_Adjust_Factor : Angle.Signed := 1;

    procedure Adjust_Second (The_Speed : Angle.Signed) is
      use type Angle.Signed;
    begin
      Motor.Adjust (Device.D2, The_Speed * Second_Adjust_Factor);
      The_Adjusting_Kind := Second_Adjusting;
      Set_State (Adjusting);
    end Adjust_Second;


    procedure Adjust_Time (Factor : Duration) is
    begin
      The_Time_Adjusting_Factor := Factor;
      The_Adjusting_Start_Time := Time.Universal;
      The_Adjusting_End_Time := Time.In_The_Future;
      The_Adjusting_Kind := Time_Adjusting;
      Set_State (Adjusting);
    end Adjust_Time;


    procedure End_Adjust is
    begin
      case The_Adjusting_Kind is
      when First_Adjusting =>
        Motor.Adjust (Device.D1, Adjusting_Stopped);
      when Second_Adjusting =>
        Motor.Adjust (Device.D2, Adjusting_Stopped);
      when Time_Adjusting =>
        The_Adjusting_End_Time := Time.Universal;
      end case;
      Set_State (Tracking);
    end End_Adjust;


    Time_Adjusting_Factor : constant Duration := 0.05;

    procedure Increment_Offset is
      The_Adjusting_Time : Time.Ut;
      The_Time_Increment : Duration;
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
        when First_Adjusting | Second_Adjusting =>
          null;
        when Time_Adjusting =>
          The_Time_Increment := The_Adjusting_Time * The_Time_Adjusting_Factor;
          The_Time_Adjustment := The_Time_Adjustment + The_Time_Increment;
        end case;
      end if;
    end Increment_Offset;


    Is_Fast_Tracking : Boolean := False;

    procedure Adjust_Handling is
      Speed : constant Angle.Value := Moving_Speeds(Adjusting_Index);
      use type Angle.Signed;
    begin
      case The_User_Command is
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
      when Increase =>
        if Is_Fast_Tracking then
          Adjust_Time (+Time_Adjusting_Factor);
        else
          Change_Adjusting_Speed (+1);
        end if;
      when Decrease =>
        if Is_Fast_Tracking then
          Adjust_Time (-Time_Adjusting_Factor);
        else
          Change_Adjusting_Speed (-1);
        end if;
      when End_Change =>
        End_Adjust;
      when Set_Guiding_Rate =>
        Set_Adjusting_Speed (Moving_Speeds'first);
      when Set_Centering_Rate =>
        Set_Adjusting_Speed (Moving_Speeds'first + 1);
      when Set_Finding_Rate =>
        Set_Adjusting_Speed (Moving_Speeds'last - 1);
      when Set_Slewing_Rate =>
        Set_Adjusting_Speed (Moving_Speeds'last);
      end case;
    end Adjust_Handling;


    procedure End_Adjust_Handling is
    begin
      case The_User_Command is
      when End_Move | End_Change =>
        End_Adjust;
      when others =>
        null;
      end case;
    end End_Adjust_Handling;


    procedure Increment_Time is
    begin
      Motor.Change_Time (The_Current_Time);
      The_Current_Time := The_Current_Time + Motor.Time_Delta; -- At Tb
      Increment_Offset;
      Tb := The_Current_Time;
      Log.Write ("increment time => Tb: " & Time.Image_Of (Tb));
      Te := Tb + Motor.Time_Delta;
      Tn := Te + Motor.Time_Delta;
      if Get_Direction = null then
        Motor.Set_Undefined (Pb);
        Motor.Set_Undefined (Pe);
        Motor.Set_Undefined (Pn);
      else
        if Motor.Is_Defined (Pe) then
          Pb := Pe;
        else
          Pb := Position_At (Tb);
        end if;
        if Motor.Is_Defined (Pn) then
          Pe := Pn;
        else
          Pe := Position_At (Te);
        end if;
        Pn := Position_At (Tn);
      end if;
      The_Event := Time_Increment;
    exception
    when Target_Lost =>
      Log.Warning ("increment time: Target_Lost");
      Motor.Stop;
      The_Event := No_Event;
    end Increment_Time;


    procedure Handle_Time_Increment is
    begin
      if Follow_New_Target then
        Motor.Update;
      else
        Motor.Stop;
      end if;
    end Handle_Time_Increment;


  --=============
  --==  States ==
  --=============

    ------------------
    -- Disconnected --
    ------------------
    procedure Disconnected_State is
    begin
      case The_Event is
      when Motor_Startup =>
        Motor.Initialize;
        Set_State (Startup);
      when Motor_Ready =>
        Motor.Define_Positions;
        Set_State (Ready);
      when Motor_Parked =>
        Set_State (Parked);
      when Motor_Positioned =>
        Set_State (Stopped);
      when others =>
        Motor.Stop;
      end case;
      Event_After_Stop := No_Event;
    end Disconnected_State;

    -------------
    -- Startup --
    -------------
    procedure Startup_State is
    begin
      case The_Event is
      when Motor_Startup =>
        Motor.Initialize;
        Set_State (Startup);
      when Motor_Ready =>
        Motor.Define_Positions;
        Set_State (Ready);
      when Motor_Parked =>
        Set_State (Parked);
      when Motor_Positioned =>
        Set_State (Stopped);
      when others =>
        null;
      end case;
    end Startup_State;

    -----------
    -- Ready --
    -----------
    procedure Ready_State is
    begin
      case The_Event is
      when User_Command =>
        Direct_Handling;
      when Synch_Park_Position =>
        Motor.Synch_Park_Position;
      when Synch_On_Target =>
        Do_Synch_On_Target;
      when Park =>
        Do_Park;
      when Motor_Parked =>
        Set_State (Parked);
      when Motor_Parking =>
        Set_State (Parking);
      when Motor_Directing =>
        Set_State (Directing);
      when others =>
        null;
      end case;
    end Ready_State;

    ------------
    -- Parked --
    ------------
    procedure Parked_State is
    begin
      case The_Event is
      when User_Command =>
        Direct_Handling;
      when Park =>
        Do_Park;
      when Synch_Park_Position =>
        Motor.Synch_Park_Position;
      when Synch_On_Target =>
        Do_Synch_On_Target;
      when Motor_Ready =>
        Set_State (Ready);
      when Motor_Parking =>
        Set_State (Parking);
      when Motor_Directing =>
        Set_State (Directing);
      when Motor_Positioning =>
        Set_State (Positioning);
      when Follow =>
        Synchronize_Time;
      when Position =>
        Do_Position;
      when Time_Increment =>
        Handle_Time_Increment;
      when others =>
        null;
      end case;
    end Parked_State;

    -------------
    -- Parking --
    -------------
    procedure Parking_State is
    begin
      case The_Event is
      when Motor_Parked =>
        Set_State (Parked);
      when Halt =>
        Motor.Stop;
        Set_Stopping;
      when Motor_Positioned =>
        Set_State (Stopped);
      when others =>
        null;
      end case;
    end Parking_State;

    -------------
    -- Stopped --
    -------------
    procedure Stopped_State is
    begin
      case The_Event is
      when User_Command =>
        Direct_Handling;
      when Park =>
        Do_Park;
      when Position =>
        Do_Position;
      when Follow =>
        Synchronize_Time;
      when Time_Increment =>
        Handle_Time_Increment;
      when Synch_Park_Position =>
        Motor.Synch_Park_Position;
      when Synch_On_Target =>
        Do_Synch_On_Target;
      when Motor_Parking =>
        Set_State (Parking);
      when Motor_Parked =>
        Set_State (Parked);
      when Motor_Directing =>
        Set_State (Directing);
      when Motor_Positioning =>
        Set_State (Positioning);
      when Motor_Positioned =>
        Handle_Positioned;
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
      when Halt =>
        Event_After_Stop := No_Event;
      when Motor_Parked =>
        Set_State (Parked);
      when Motor_Parking =>
        Set_State (Parking);
      when Motor_Ready =>
        Set_State (Ready);
      when Motor_Positioned =>
        Handle_Positioned;
      when Time_Increment =>
        Handle_Time_Increment;
      when others =>
        null;
      end case;
    end Stopping_State;

    ---------------
    -- Directing --
    ---------------
    procedure Directing_State is
    begin
      case The_Event is
      when Halt =>
        Motor.Stop;
      when User_Command =>
        End_Direct_Handling;
      when Motor_Parked =>
        Set_State (Parked);
      when Motor_Ready =>
        Set_State (Ready);
      when Motor_Positioned =>
        Set_State (Stopped);
      when others =>
        null;
      end case;
    end Directing_State;

    -----------------
    -- Positioning --
    -----------------
    procedure Positioning_State is
    begin
      case The_Event is
      when Park =>
        Stop_And_Park;
      when Position =>
        Stop_And_Position;
      when Follow =>
        Stop_And_Follow;
      when Halt =>
        Stop_Target;
      when Motor_Fault =>
        Set_Stopping;
      when Motor_Positioned =>
        Handle_Positioned;
      when Motor_Parked =>
        Set_State (Parked);
      when Time_Increment =>
        Handle_Time_Increment;
      when others =>
        null;
      end case;
    end Positioning_State;

    ---------------
    -- Preparing --
    ---------------
    procedure Preparing_State is
    begin
      case The_Event is
      when Park =>
        Stop_And_Park;
      when Position =>
        Stop_And_Position;
      when Halt =>
        Stop_Target;
      when Motor_Positioning | Motor_Fault  =>
        Set_Stopping;
        Time_Control_End;
      when Motor_Positioned =>
        Handle_Positioned;
      when Follow =>
        if not Follow_New_Target then
          Motor.Stop;
          Set_Stopping;
        end if;
      when Time_Increment =>
        if The_Time_When_Stopped <= Tb then
          Set_State (Waiting);
        end if;
        Motor.Update;
      when others =>
        null;
      end case;
    end Preparing_State;

    -------------
    -- Waiting --
    -------------
    procedure Waiting_State is
    begin
      case The_Event is
      when Park =>
        Stop_And_Park;
      when Position =>
        Stop_And_Position;
      when Halt =>
        Stop_Target;
      when Motor_Positioning | Motor_Fault =>
        Set_Stopping;
        Time_Control_End;
      when Motor_Positioned =>
        Handle_Positioned;
      when Follow =>
        if not Follow_New_Target then
          Motor.Stop;
          Set_State (Stopped);
        end if;
      when Time_Increment =>
        if The_Arriving_Time <= Tb then
          if Goto_Target then
            Motor.Update;
          else
            Motor.Stop;
            Set_State (Stopped);
          end if;
        else
          Motor.Update;
        end if;
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
      when Park =>
        Stop_And_Park;
      when Position =>
        Stop_And_Position;
      when Halt =>
        Stop_Target;
      when Motor_Positioning | Motor_Fault =>
        Set_Stopping;
        Time_Control_End;
      when Motor_Positioned =>
        Handle_Positioned;
      when Follow =>
        if not Follow_New_Target then
          Motor.Stop;
          Set_Stopping;
        end if;
      when Time_Increment =>
        if The_Arriving_Time <= Tb then
          if Following_Target then
            Set_State (Tracking);
            Motor.Update;
          else
            if Goto_Target then
              Motor.Update;
            else
              Motor.Stop;
              Set_Stopping;
            end if;
          end if;
        else
          Motor.Update;
        end if;
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
      when Park =>
        Stop_And_Park;
      when Position =>
        Stop_And_Position;
      when Halt =>
        Stop_Target;
      when Motor_Positioning | Motor_Fault =>
        Set_Stopping;
        Time_Control_End;
      when Motor_Positioned =>
        Handle_Positioned;
      when Follow =>
        if not Follow_New_Target then
          Motor.Stop;
          Set_Stopping;
        end if;
      when Align =>
        Picture_Aligning_Enabled := False;
        Is_Aligning := True;
      when Synch_On_Target =>
        Stop_And_Synch;
      when Time_Increment =>
        if Following_Target then
          Motor.Update;
          if Is_Aligning then
            if Alignment.Is_One_Star then
              Motor.Synchronize_Positions;
            else
              Motor.Update_Positions;
              Motor.Set_Undefined (Pb);
              Motor.Set_Undefined (Pe);
              Motor.Set_Undefined (Pn);
            end if;
            Is_Aligning := False;
          end if;
          if The_State = Solving then
            if Synch_On_Picture_Complete then
              Picture_Aligning_Enabled := True;
              The_State := Tracking;
            end if;
          elsif not Picture_Aligning_Enabled and then Picture.Exists and then Synch_On_Picture then
            The_State := Solving;
          end if;
        else
          if Goto_Target then
            Motor.Update;
          else
            Motor.Stop;
            Set_Stopping;
          end if;
        end if;
      when User_Command =>
        Adjust_Handling;
      when others =>
        null;
      end case;
    end Tracking_State;

    ---------------
    -- Adjusting --
    ---------------
    procedure Adjusting_State is
    begin
      case The_Event is
      when Park =>
        Stop_And_Park;
      when Position =>
        Stop_And_Position;
      when Halt =>
        Motor.Stop;
      when Motor_Positioning | Motor_Fault =>
        Set_Stopping;
        Time_Control_End;
      when Motor_Positioned =>
        Handle_Positioned;
      when Follow =>
        if not Follow_New_Target then
          Motor.Stop;
          Set_Stopping;
        end if;
      when Time_Increment =>
        if Following_Target then
          Motor.Update;
        else
          if Goto_Target then
            Motor.Update;
          else
            Motor.Stop;
            Set_Stopping;
          end if;
        end if;
      when User_Command =>
        End_Adjust_Handling;
      when others =>
        null;
      end case;
    end Adjusting_State;

    use type Earth.Direction;
    use type Angle.Signed;

    Has_New_Data : Boolean := True;

  begin -- Control_Task
    accept Start do
      Motor.Open_Communication (Motor_State_Handler'access);
    end Start;
    Set_State (Disconnected);
    loop
      begin
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
          The_Event := Follow;
        or
          accept Position_To (Landmark : Name.Id) do
            The_Landmark := Landmark;
          end Position_To;
          The_Event := Position;
        or
          accept Align;
          The_Event := Align;
        or
          accept Park;
          The_Event := Park;
        or
          accept Synch_Park_Position;
          The_Event := Synch_Park_Position;
        or
          accept Synch_On_Target;
          The_Event := Synch_On_Target;
        or
          accept Set (The_Orientation : Orientation) do
            Log.Write ("orientation => " & The_Orientation'img);
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
            if The_State > Startup then
              The_Event := User_Command;
              The_User_Command := The_Command;
            end if;
          end Execute;
        or
          accept New_Motor_State (New_State : Motor.State) do
            case New_State is
            when Motor.Disconnected =>
              Set_State (Disconnected);
              The_Event := Motor_Disconnected;
            when Motor.Startup =>
              The_Event := Motor_Startup;
            when Motor.Fault =>
              The_Event := Motor_Fault;
            when Motor.Ready =>
              The_Event := Motor_Ready;
            when Motor.Parked =>
              The_Event := Motor_Parked;
            when Motor.Parking =>
              The_Event := Motor_Parking;
            when Motor.Positioned =>
              The_Event := Motor_Positioned;
            when Motor.Positioning =>
              The_Event := Motor_Positioning;
            when Motor.Directing =>
              The_Event := Motor_Directing;
            when Motor.Update =>
              Increment_Time;
            when Motor.Terminated =>
              raise Program_Error;
            end case;
            Has_New_Data := True;
          end New_Motor_State;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := The_State;
            The_Data.Time_Adjustment := The_Time_Adjustment;
            case The_State is
            when Approaching =>
              The_Data.Arriving_Time := The_Arriving_Time;
            when Parking | Preparing | Positioning =>
              The_Data.Arriving_Time := The_Time_When_Stopped;
            when others =>
              The_Data.Arriving_Time := 0.0;
            end case;
            The_Data.Motor_Positions := Motor.Positions;
            if Site.Is_Defined then
              Numerics.Calculate_Horizontal_Coordinates_For (The_Data.Motor_Positions,
                                                             The_Data.Local_Direction,
                                                             The_Data.Adjustment);
            end if;
            case The_State is
            when Preparing | Waiting | Approaching | Tracking | Solving | Adjusting =>
              The_Data.Universal_Time := The_Current_Time - Motor.Time_Delta;
              declare
                Alignment_Offsets : constant Earth.Direction := Matrix.Actual_Offset;
              begin
                The_Data.Local_Direction := The_Data.Local_Direction - Alignment_Offsets;
                if Alignment_Offsets = Earth.Zero_Direction then
                  The_Data.Alignment_Offsets := Earth.Unknown_Direction;
                else
                  The_Data.Alignment_Offsets := Alignment_Offsets;
                end if;
              end;
            when others =>
              The_Data.Universal_Time := Time.Universal;
              The_Data.Alignment_Offsets := Earth.Unknown_Direction;
            end case;
            The_Data.Cone_Error := Alignment.Cone_Error;
            The_Data.Pole_Offsets := Alignment.Pole_Offsets;
            The_Data.Rotations := Alignment.Rotations;
            The_Data.System_Error := Alignment.System_Error;
            The_Data.Actual_Direction := Numerics.Direction_Of (The_Data.Local_Direction, The_Data.Universal_Time);
            if Get_Direction = null then
              The_Data.Target_Direction := Space.Unknown_Direction;
            else
              The_Data.Target_Direction := Get_Direction (Id, The_Data.Universal_Time);
            end if;
            The_Data.Target_Lost := Target_Is_Lost;
            Target_Is_Lost := False;
            The_Event := No_Event;
          end Get;
        end select;
        case The_State is
        when Tracking | Adjusting =>
          Is_Fast_Tracking := The_Next_Arriving_Time /= Time.In_The_Past;
        when others =>
          Is_Fast_Tracking := False;
          The_Adjusting_Start_Time := Time.In_The_Future;
        end case;
        if The_Event /= No_Event then
          Log.Write ("state => " & The_State'img & " - event => " & The_Event'img);
          case The_State is
          when Disconnected       => Disconnected_State;
          when Startup            => Startup_State;
          when Ready              => Ready_State;
          when Parked             => Parked_State;
          when Parking            => Parking_State;
          when Stopped            => Stopped_State;
          when Stopping           => Stopping_State;
          when Directing          => Directing_State;
          when Positioning        => Positioning_State;
          when Preparing          => Preparing_State;
          when Waiting            => Waiting_State;
          when Approaching        => Approaching_State;
          when Tracking | Solving => Tracking_State;
          when Adjusting          => Adjusting_State;
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
    Motor.Close_Communication;
    declare
      Is_Terminated : Boolean := False;
      use type Motor.State;
    begin
      while not Is_Terminated loop
        begin
          accept New_Motor_State (New_State : Motor.State) do
            Is_Terminated := New_State = Motor.Terminated;
          end New_Motor_State;
        exception
        when others =>
          exit;
        end;
      end loop;
    end;
    Log.Write ("end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control_Task;

end Telescope;
