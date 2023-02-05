-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with System;
with Traces;

package body Motor.Io.Protocol.Simulation is

  package Log is new Traces ("Simulation");

  use type Time.Ut;

  task type Simulation_Task with Priority => System.Max_Priority is

    entry Start;

    entry Get_Device_State (The_State : out Device.State);

    entry Get_Position_Known (Is_Known : out Boolean);

    entry Get_Step_Data (The_Data : out Step_Information);

    entry Set_Initial_Count (C0_1 : Natural;
                             C0_2 : Natural);

    entry Set_Positions (P : Step_Positions);

    entry Start_Updating;

    entry Update (The_Drive  : Device.Drive;
                  The_Action : Action);

    entry Update_Offsets (Offsets : Step_Positions);

    entry Update_Positions (Offsets : Step_Positions);

    entry Define_For (The_Drive         : Device.Drive;
                      Offset_Per_Action : Step_Count);

    entry Stop_All;

    entry Finish;

  end Simulation_Task;


  use all type Device.Drive;
  use all type Device.State;

  type Simulation_Task_Access is access Simulation_Task;

  The_Simulation : Simulation_Task_Access;


  procedure Start is
  begin
    The_Simulation := new Simulation_Task;
    The_Simulation.Start;
  end Start;


  function Actual_Stepper_State return Device.State is
    The_State : Device.State;
  begin
    The_Simulation.Get_Device_State (The_State);
    Log.Write ("actual device state => " & The_State'img);
    return The_State;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Actual_Stepper_State;


  function Step_Position_Known return Boolean is
    The_Position_Is_Known : Boolean;
  begin
    The_Simulation.Get_Position_Known (The_Position_Is_Known);
    Log.Write ("step position known => " & The_Position_Is_Known'img);
    return The_Position_Is_Known;
  end Step_Position_Known;


  function Actual_Step_Data return Step_Information is
    The_Data : Step_Information;
  begin
    The_Simulation.Get_Step_Data (The_Data);
    Log.Write ("actual step positions: M1 =>" & The_Data.Positions.M1'img & "; M2 =>" & The_Data.Positions.M2'img);
    Log.Write ("actual step offsets  : M1 =>" & The_Data.Offsets.M1'img & "  ; M2 =>" & The_Data.Offsets.M2'img);
    return The_Data;
  end Actual_Step_Data;


  procedure Set_Initial_Count (C0_1 : Natural;
                               C0_2 : Natural) is
  begin
    Log.Write ("set initial count: M1 =>" & C0_1'img& "; M2 =>" & C0_2'img);
    The_Simulation.Set_Initial_Count (C0_1, C0_2);
  end Set_Initial_Count;


  procedure Set_Step_Positions (The_Positions : Step_Positions) is
  begin
    Log.Write ("set step positions: M1 =>" & The_Positions.M1'img & "; M2 =>" & The_Positions.M2'img);
    The_Simulation.Set_Positions (The_Positions);
  end Set_Step_Positions;


  procedure Synchronize_Start_Time (The_Time : out Time.Ut) is
  begin
    The_Time := Time.Synchronized_Universal (Base => Motor.Time_Delta);
    Log.Write ("start time => " & Time.Image_Of (The_Time));
    delay (abs Duration(The_Time - Time.Universal));
    The_Simulation.Start_Updating;
  end Synchronize_Start_Time;


  procedure Update (The_Drive : Device.Drive;
                    The_Action : Action) is
  begin
    The_Simulation.Update (The_Drive, The_Action);
  end Update;


  procedure Update_Step_Offsets (Offsets : Step_Positions) is
  begin
    Log.Write ("update step offsets: M1 =>" & Offsets.M1'img & "; M2 =>" & Offsets.M2'img);
    The_Simulation.Update_Offsets (Offsets);
  end Update_Step_Offsets;


  procedure Update_Step_Positions (Offsets : Step_Positions) is
  begin
    Log.Write ("update step positions: M1 =>" & Offsets.M1'img & "; M2 =>" & Offsets.M2'img);
    The_Simulation.Update_Positions (Offsets);
  end Update_Step_Positions;


  procedure Transfer_Actions (M1 : Action_List := No_Actions;
                              M2 : Action_List := No_Actions) is
    M1_Index : Natural := M1'first;
    M2_Index : Natural := M2'first;

  begin
    Log.Write ("transfer actions");
    loop
      if M1_Index <= M1'last then
        Update (D1, M1(M1_Index));
        M1_Index := M1_Index + 1;
      else
        exit when M2_Index > M2'last;
      end if;
      if M2_Index <= M2'last then
        Update (D2, M2(M2_Index));
        M2_Index := M2_Index + 1;
      else
        exit when M1_Index > M1'last;
      end if;
    end loop;
  end Transfer_Actions;


  procedure Adjust (The_Drive         : Device.Drive;
                    Offset_Per_Action : Step_Count) is
  begin
    Log.Write ("adjust " & The_Drive'img & " - steps:" & Offset_Per_Action'img);
    The_Simulation.Define_For (The_Drive, Offset_Per_Action);
  end Adjust;


  procedure Stop_All is
  begin
    Log.Write ("stop all");
    The_Simulation.Stop_All;
  end Stop_All;


  procedure Finish is
  begin
    Log.Write ("finish");
    The_Simulation.Finish;
  end Finish;


  task body Simulation_Task is

    type Clock_Time is range 0 .. 2**63 - 1;

    Half_Second : constant Clock_Time := Clock_Time(Freq) / 2;

    subtype CARD32 is Natural;

    subtype INT32 is Integer;

    type Actions_Range is mod 2**8;

    type Action_Ring is array (Actions_Range) of Action;

    type Action_State is (Inactive, Accelleration, Decelleration, Continuation, Halted);

    type Action_Data is record
      Ring          : Action_Ring;
      State         : Action_State;
      Put_Index     : Actions_Range;
      Get_Index     : Actions_Range;
      Count         : CARD32;
      Next_Step     : Clock_Time;
      R             : CARD32;
      I             : CARD32;
      Cs            : CARD32;
      C0            : CARD32;
      Position      : INT32;
      Offset        : INT32;
      Has_Position  : Boolean;
      Sa            : Integer; -- adjustment offset
    end record;

    type Actions is array (Device.Drive) of Action_Data;

    The_Event      : Device.State := Startup;
    Send_Event     : Boolean := True;
    Next_Send_Time : Clock_Time;

    The_Actions : Actions;

    Has_Error  : Boolean := False;


    procedure Initialize_Actions is
    begin
      for The_Action of The_Actions loop
        The_Action.State := Halted;
        The_Action.Offset := 0;
        The_Action.Sa := 0;
        The_Action.Count := 0;
        The_Action.Put_Index := Actions_Range'first;
        The_Action.Get_Index := Actions_Range'first;
        The_Action.Has_Position := False;
        The_Action.Next_Step := Clock_Time'last;
      end loop;
    end Initialize_Actions;


    function Other_Of (The_Device : Device.Drive) return Device.Drive is
    begin
      if The_Device = D1 then
        return D2;
      else
        return D1;
      end if;
    end Other_Of;


    procedure Stop (The_Drive : Device.Drive) is
      D : Action_Data renames The_Actions(The_Drive);
    begin
      if D.State /= Halted then
        if D.Cs < D.C0 then
          if D.Cs = 0 then
            D.Cs := D.C0;
            Has_Error := True;
            Log.Error ("would leed to division by zero");
          end if;
          declare
            A : Action renames D.Ring(D.Get_Index);
          begin
            A.C := D.Cs;
            A.N := D.C0 * 3 / D.Cs; -- calculate _N from cs and c0
            A.S := A.N * A.N / 16;
            if A.S = 0 then
              A.S := 1;
            end if;
            A.N := A.S;
            D.R := 0;
          end;
          D.State := Decelleration;
          D.Count := 1;
        else
          D.Cs := D.C0;
          D.State := Halted;
          Log.Write ("task: halted from stop " & The_Drive'img);
          D.Offset := 0;
          D.Count := 0;
          D.Put_Index := D.Get_Index;
          if The_Actions(Other_Of (The_Drive)).State = Halted then
            The_Event := Stopped;
            Has_Error := False;
            Send_Event := True;
          end if;
        end if;
      end if;
    end Stop;


    procedure Stop_Motors is
    begin
      Stop (D1);
      Stop (D2);
      if The_Event = Synchronised then
        The_Event := Moving;
        Send_Event := True;
      end if;
    end Stop_Motors;


    ----------
    -- Work --
    ----------

    The_Time : Clock_Time := 0;


    procedure Work_For (The_Drive : Device.Drive) is
      D    : Action_Data renames The_Actions(The_Drive);
      Q, X : CARD32;
    begin
      if D.Count = 0 then
        if D.State = Halted then
          if The_Actions(Other_Of (The_Drive)).State = Halted then
            if The_Event = Moving then
              The_Event := Stopped;
              Has_Error := False;
            end if;
          end if;
        else
          Stop (The_Drive);
          Has_Error := True;
          Log.Error ("no actions received");
        end if;
      else
        if D.State = Halted then
          The_Event := Moving;
          D.State := Inactive;
          D.Cs := D.C0;
          D.Next_Step := The_Time;
        end if;
      end if;
      declare
        A: Action renames D.Ring(D.Get_Index);
      begin
        if D.State = Inactive then
          if A.C = 0 then
            A.C := D.Cs;
          end if;
          case A.K is
          when Accelerate =>
            D.State := Accelleration;
          when Decelerate =>
            A.N := A.N + 1;
            D.State := Decelleration;
          when Keep_Speed =>
            D.State := Continuation;
            if D.Sa /= 0 then
              D.Offset := D.Offset + D.Sa;
              declare
                T  : constant Integer := A.S * A.C + A.N;
                Sc : Integer;
              begin
                case A.D is
                when Backward =>
                  Sc := D.Sa - A.S;
                when Forward =>
                  Sc := D.Sa + A.S;
                when Undefined =>
                  if A.S /= 0 then
                    Log.Error ("Undefined direction");
                    Has_Error := True;
                  end if;
                  Sc := D.Sa;
                end case;
                if Sc = 0 then
                  A.D := Undefined;
                  A.S := 0;
                  A.N := T;
                  A.C := 0;
                else
                  if Sc > 0 then
                    A.D := Forward;
                    A.S := Sc;
                  else -- Sc < 0
                    A.D := Backward;
                    A.S := -Sc;
                  end if;
                  A.C := T / A.S;
                  A.N := T - A.S * A.C;
                end if;
              end;
            end if;
            if A.N > 0 then
              D.I := A.S;
            end if;
          end case;
          D.R := 0;
        end if;
        case D.State is
        when Accelleration =>
          A.N := A.N + 1;
          D.I := 2 * A.C + D.R;
          X := 4 * A.N + 1;
          Q := D.I / X;
          A.C := A.C - Q;
          D.R := D.I - Q * X;
          D.Cs := A.C;
        when Decelleration =>
          X := 4 * A.N - 1;
          Q := 2 * A.C + X - 1 - D.R;
          Q := Q / X;
          D.R := Q * X + D.R - 2 * A.C;
          A.C := A.C + Q;
          A.N := A.N - 1;
          D.Cs := A.C;
        when Continuation =>
          if A.S > 0 then
            D.Cs := A.C;
            if A.N > 0 then
              D.R := D.R + A.N;
              if D.R >= D.I then
                D.R := D.R - D.I;
                D.Cs := D.Cs + 1;
              end if;
            end if;
          else
            if A.N = 0 then
              Log.Error ("no increment in time");
              Has_Error := True;
            end if;
            D.Cs := A.N;
            A.N := 0;
          end if;
        when Halted =>
          D.Next_Step := D.Next_Step + Clock_Time(D.C0);
          return;
        when Inactive =>
          raise Program_Error;
        end case;
        if A.S > 0 then
          A.S := A.S - 1;
          if A.D = Forward then
            D.Position := D.Position + 1;
          else
            D.Position := D.Position - 1;
          end if;
        end if;
        if A.S = 0 then
          if D.State = Decelleration then
            D.State := Inactive;
            if A.N <= 1 then
              D.Cs := D.C0;
              if The_Event = Moving then
                D.State := Halted;
                Log.Write ("task: halted from moving " & The_Drive'img);
                D.Offset := 0;
                D.Get_Index := D.Get_Index + 1;
                D.Put_Index := D.Get_Index;
                D.Count := 0;
                return;
              end if;
            end if;
          else
            D.State := Inactive;
          end if;
          D.Get_Index := D.Get_Index + 1;
          D.Count := D.Count - 1;
        end if;
        D.Next_Step := D.Next_Step + Clock_Time(D.Cs);
      exception
      when others =>
        Log.Write ("  Motor        => " & The_Drive'img);
        Log.Write ("  State        => " & D.State'img);
        Log.Write ("  Has_Position => " & D.Has_Position'img);
        Log.Write ("  Position     =>" & D.Position'img);
        Log.Write ("  Offset       =>" & D.Offset'img);
        Log.Write ("  Put_Index    =>" & D.Put_Index'img);
        Log.Write ("  Get_Index    =>" & D.Get_Index'img);
        Log.Write ("  Count        =>" & D.Count'img);
        Log.Write ("  Next_Step    =>" & D.Next_Step'img);
        Log.Write ("  R            =>" & D.R'img);
        Log.Write ("  I            =>" & D.I'img);
        Log.Write ("  Cs           =>" & D.Cs'img);
        Log.Write ("  C0           =>" & D.C0'img);
        Log.Write ("  A.C          =>" & A.C'img);
        Log.Write ("  A.N          =>" & A.N'img);
        Log.Write ("  A.S          =>" & A.S'img);
        Log.Write ("  Q            =>" & Q'img);
        Log.Write ("  X            =>" & X'img);
        raise;
      end;
    end Work_For;


    procedure Work is
      Has_Worked : Boolean := True;
    begin
      while Has_Worked loop
        Has_Worked := False;
        if The_Actions(D1).Next_Step <= The_Time then
          Work_For (D1);
          Has_Worked := True;
        end if;
        if The_Actions(D2).Next_Step <= The_Time then
          Work_For (D2);
          Has_Worked := True;
        end if;
      end loop;
    end Work;

    The_Next_Time : Ada.Real_Time.Time;

    Step_Positions_Known : Boolean := False;

    use type Ada.Real_Time.Time;

  begin
    accept Start;
    The_Next_Time := Ada.Real_Time.Clock;
    Initialize_Actions;
    Next_Send_Time := The_Time;
    loop
      select
        when Send_Event => accept Get_Device_State (The_State : out Device.State) do
          The_State := The_Event;
          Send_Event := False;
        end Get_Device_State;
      or
        accept Get_Position_Known (Is_Known : out Boolean) do
          Is_Known := Step_Positions_Known;
        end Get_Position_Known;
      or
        accept Get_Step_Data (The_Data : out Step_Information) do
          The_Data := (Positions => (M1 => The_Actions(D1).Position,
                                     M2 => The_Actions(D2).Position),
                       Offsets   => (M1 => The_Actions(D1).Offset,
                                     M2 => The_Actions(D2).Offset));
        end Get_Step_Data;
      or
        accept Set_Initial_Count (C0_1 : Natural;
                                  C0_2 : Natural) do
          The_Actions(D1).C0 := C0_1;
          The_Actions(D1).Cs := C0_1;
          The_Actions(D2).C0 := C0_2;
          The_Actions(D2).Cs := C0_2;
          The_Event := Initialized;
          Send_Event := True;
        end Set_Initial_Count;
      or
        accept Set_Positions (P : Step_Positions) do
          The_Actions(D1).Position := P.M1;
          The_Actions(D2).Position := P.M2;
          The_Actions(D1).Offset := 0;
          The_Actions(D2).Offset := 0;
          Step_Positions_Known := True;
          The_Event := Stopped;
          Has_Error := False;
          Send_Event := True;
        end Set_Positions;
      or
        accept Start_Updating;
        Log.Write ("task: start updating");
        The_Event := Synchronised;
        Send_Event := True;
        The_Time := 0;
        Next_Send_Time := Half_Second;
        declare
          D : Action_Data renames The_Actions(D1);
        begin
          D.Next_Step := Half_Second;
          D.State := Inactive;
        end;
        declare
          D : Action_Data renames The_Actions(D2);
        begin
          D.Next_Step := Half_Second;
          D.State := Inactive;
        end;
      or
        accept Update (The_Drive  : Device.Drive;
                       The_Action : Action) do
          declare
            D : Action_Data renames The_Actions(The_Drive);
          begin
            if not Has_Error then
              D.Ring(D.Put_Index) := The_Action;
              D.Put_Index := D.Put_Index + 1;
              D.Count := D.Count + 1;
            end if;
            if D.State = Halted then
              D.Next_Step := The_Time;
              Log.Write ("task: update " & The_Drive'img & " D.Next_Step =>" & The_Time'img);
            end if;
          end;
        end Update;
      or
        accept Update_Offsets (Offsets : Step_Positions) do
          The_Actions(D1).Offset := The_Actions(D1).Offset + Offsets.M1;
          The_Actions(D2).Offset := The_Actions(D2).Offset + Offsets.M2;
          The_Actions(D1).Position := The_Actions(D1).Position + Offsets.M1;
          The_Actions(D2).Position := The_Actions(D2).Position + Offsets.M2;
        end Update_Offsets;
      or
        accept Update_Positions (Offsets : Step_Positions) do
          The_Actions(D1).Position := The_Actions(D1).Position - The_Actions(D1).Offset + Offsets.M1;
          The_Actions(D2).Position := The_Actions(D2).Position - The_Actions(D2).Offset + Offsets.M2;
          The_Actions(D1).Offset := 0;
          The_Actions(D2).Offset := 0;
        end Update_Positions;
      or
        accept Define_For (The_Drive         : Device.Drive;
                           Offset_Per_Action : Step_Count) do
          The_Actions(The_Drive).Sa := Offset_Per_Action;
        end Define_For;
      or
        accept Stop_All;
        Stop_Motors;
      or
        accept Finish;
        exit;
      or
        delay until The_Next_Time;
        Work;
        if The_Time > Next_Send_Time then
          Log.Write ("task: send time =>" & The_Time'img);
          Next_Send_Time := Next_Send_Time + Half_Second;
          Send_Event := True;
        end if;
        The_Next_Time := The_Next_Time + Ada.Real_Time.To_Time_Span(0.01);
        The_Time := The_Time + Clock_Time(Freq / 100);
      end select;
    end loop;
    Log.Write ("task: end");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Simulation_Task;

end Motor.Io.Protocol.Simulation;
