-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Astro;
with Motor.Io.Protocol;
with Parameter;
with Traces;

package body Motor.Io is

  use all type Device.Drive;

  package Log is new Traces ("Stepper");

  type Counter is new Long_Long_Integer;

  Infinite : constant := Natural'last;

  The_Frequency : Natural := 0;

  function Freq return Natural is
  begin
    if The_Frequency = 0 then
      raise Program_Error;
    end if;
    return The_Frequency;
  end Freq;


  The_Nspr : Device.Steps_Per_Revolution; -- steps per revolution

  function Nspr_Of (The_Drive : Device.Drive) return Natural is
  begin
    return The_Nspr(The_Drive);
  end Nspr_Of;


  function C_Of (T : Time.Ut) return Natural is
  begin
    return Natural(Freq * T);
  end C_Of;


  function V_Of (C : Natural;
                 D : Device.Drive) return Value is
    use type Value;
  begin
    if C = Infinite then
      return 0.0;
    else
      return Value(Freq) * 360.0 / (Value(Nspr_Of (D)) * Value(C));
    end if;
  end V_Of;


  function Dt_Of (V : Value;
                  D : Device.Drive) return Natural is
    use type Value;
  begin
    return abs Integer(Value(Freq) * 360.0 / (Value(Nspr_Of (D)) * V));
  exception
  when others =>
    return Infinite;
  end Dt_Of;


  function C0_Of (T  : Natural;
                  Ve : Value;
                  D  : Device.Drive) return Natural is
    use Astro;
  begin
    return Natural(0.676 * Value(Freq) * SQRT(Value(T) / Value(Freq) * 720.0 / (Value(Nspr_Of (D)) * abs Ve)));
  end C0_Of;


  function C0_Of (A : Value;
                  D : Device.Drive) return Natural is
    use Astro;
  begin
    return Natural(0.676 * Value(Freq) * SQRT(720.0 / (Value(Nspr_Of (D)) * abs A)));
  end C0_Of;


  function N_Of (Dt : Natural;
                 V  : Value;
                 Ve : Value;
                 D  : Device.Drive) return N_Type is
    use type Value;
  begin
    return N_Type(V * V * Value(Dt) / Value(Freq) * Value(Nspr_Of (D)) / (abs (Ve - V) * 720.0));
  exception
  when others =>
    return N_Type'last;
  end N_Of;


  function N_Of (S : Value;
                 D : Device.Drive) return Integer is
    use type Value;
  begin
    return Integer(Value(Nspr_Of (D)) * S / 360.0);
  exception
  when others =>
    Log.Error ("S =>" & S'img);
    raise Program_Error;
  end N_Of;


  function S_Of (N : Integer;
                 D : Device.Drive) return Value is
    use type Value;
  begin
    return Value(N) * 360.0 / Value(Nspr_Of (D));
  end S_Of;


  function Distance (From, To : Integer;
                     D        : Device.Drive) return Integer is
    The_Distance : Integer := To - From;
    N180         : constant Natural := Nspr_Of (D) / 2; -- steps for 180 degrees
  begin
    loop
      if The_Distance > N180 then
        The_Distance := The_Distance - Nspr_Of (D);
      elsif The_Distance < - N180 then
        The_Distance := The_Distance + Nspr_Of (D);
      else
        return The_Distance;
      end if;
    end loop;
  end Distance;


  type Drive_Data is record
    T    : Counter;
    Tr   : Integer;
    S    : Integer;
    Sr   : Integer;
    D    : Step_Direction;
    Tb   : Time.Ut;
    Se   : Value;
    V    : Value;
    C    : Natural;
    Cmin : Natural;
    Cmax : Natural;
  end record;


  procedure Simulate (T  : Natural;
                      Cb : Natural;
                      Nb : Natural;
                      K  : Acceleration_Kind;
                      Cm : Natural;
                      Te : out Natural;
                      Sn : out Natural;
                      Ce : out Natural) is

    C : Natural;
    D : Natural;
    I : Natural;
    N : Natural;
    R : Natural;

  begin
    --Log.Write ("+++++++++++++");
    --Log.Write ("T :" & T'img);
    --Log.Write ("Cb:" & Cb'img);
    --Log.Write ("Nb:" & Nb'img);
    Ce := Cb;
    N := Nb + 1;
    R := 0;
    Te := 0;
    if K = Accelerate then
      loop
        I  := 2 * Ce + R;
        D  := 4 * N + 1;
        C := Ce - I / D;
        exit when (Te + C >= T) or (C < Cm);
        R := I mod D;
        Te := Te + C;
        N := N + 1;
        Ce := C;
      end loop;
      Sn := N - Nb - 1;
    else
      loop
        D := 4 * N - 1;
        I := 2 * Ce + D - 1 - R;
        I := I / D;
        C := Ce + I;
        exit when Te + C >= T;
        R := I * D + R - 2 * Ce;
        Te := Te + C;
        N := N - 1;
        if N <= 1 then
          Ce := Infinite;
          exit;
        end if;
        Ce := C;
      end loop;
      Sn := Nb + 1 - N;
    end if;
    --Log.Write ("Te:" & Te'img);
    --Log.Write ("Sn:" & Sn'img);
    --Log.Write ("Ce:" & Ce'img);
    --Log.Write ("+++++++++++++");
  exception
  when others =>
    Log.Error ("not simulated");
    raise;
  end Simulate;


  type Data is array (Drive) of Drive_Data;

  The_Data : Data;

  type Counters is array (Drive) of Natural;

  C0 : Counters;
  Cm : Counters;


  procedure Initialize_Data is
  begin
    for The_Drive in The_Data'range loop
      The_Data(The_Drive) := (T    => 0,
                              Tr   => 0,
                              S    => 0,
                              Sr   => 0,
                              D    => Undefined,
                              Tb   => 0.0,
                              Se   => 0.0,
                              V    => 0.0,
                              C    => Infinite,
                              Cmin => Cm(The_Drive) - 1,
                              Cmax => C0(The_Drive));
    end loop;
  end Initialize_Data;


  function Device_Version return Hardware_Version is
  begin
    return Protocol.Connected_Device_Version;
  end Device_Version;


  procedure Define (Parameters_1 :     Parameters;
                    Parameters_2 :     Parameters;
                    The_Epsilon  : out Values) is

    use type Value;

  begin
    The_Frequency := Parameter.Clocks_Per_Second;
    The_Nspr := Parameter.Steps_Per_Revolution;
    for D in Device.Drive loop
      The_Epsilon(D) := 360.0 / Value(The_Nspr(D));
    end loop;
    C0(D1) := C0_Of (Parameters_1.Am, D1);
    C0(D2) := C0_Of (Parameters_2.Am, D2);
    Cm(D1) := Dt_Of (Parameters_1.Vm, D1);
    Cm(D2) := Dt_Of (Parameters_2.Vm, D2);
    Initialize_Data;
    Log.Write ("define");
    Log.Write ("  frequency  =>" & The_Frequency'img);
    Log.Write ("  steps1/rev =>" & The_Nspr(D1)'img);
    Log.Write ("  steps2/rev =>" & The_Nspr(D2)'img);
    Log.Write ("  C1 minimum =>" & The_Data(D1).Cmin'img);
    Log.Write ("  C1 maximum =>" & The_Data(D1).Cmax'img);
    Log.Write ("  C2 minimum =>" & The_Data(D2).Cmin'img);
    Log.Write ("  C2 maximum =>" & The_Data(D2).Cmax'img);
  end Define;


  procedure Connect renames Protocol.Do_Connect;


  procedure Open is
  begin
    Log.Write ("open");
    Protocol.Do_Open_Communication;
  end Open;


  procedure Startup_Initialization is
  begin
    Log.Write ("startup initialization");
    Protocol.Initialize (C0_1 => C0(D1),
                         C0_2 => C0(D2));
  end Startup_Initialization;


  The_Autoguiding_Speed : Value;

  procedure Set_Autoguiding (The_Speed : Angle.Value) is
    use type Angle.Value;
  begin
    The_Autoguiding_Speed := +The_Speed;
  end Set_Autoguiding;


  function Actual_State return Device.State is
  begin
    return Protocol.Actual_Device_State;
  end Actual_State;


  function Actual_Synch_State return Device.Time_Synch_State is
  begin
    return Protocol.Actual_Device_Synch_State;
  end Actual_Synch_State;


  procedure Set_Positions (The_Positions : Values) is
  begin
    Protocol.Define_Positions ((M1 => N_Of (The_Positions(D1), D1),
                                M2 => N_Of (The_Positions(D2), D2)));
  end Set_Positions;


  procedure Update_Positions (Offsets : Alignment.Offsets) is
  begin
    Protocol.Update_Positions ((M1 => N_Of (Offsets(D1), D1),
                                M2 => N_Of (Offsets(D2), D2)));
  end Update_Positions;


  function Position_Is_Known return Boolean is
  begin
    return Protocol.Position_Known;
  end Position_Is_Known;


  function Actual_Data return Information is
    D : constant Step_Information := Protocol.Stepper_Data;
  begin
    return (Positions => (D1 => S_Of (D.Positions.M1, D1),
                          D2 => S_Of (D.Positions.M2, D2)),
            Offsets   => (D1 => S_Of (D.Offsets.M1, D1),
                          D2 => S_Of (D.Offsets.M2, D2)));
  end Actual_Data;


  function Actual_Board_Temperature return Celsius is
  begin
    return Protocol.Hardware_Board_Temperature;
  end Actual_Board_Temperature;


  function Action_List_For (S   : Integer;
                            C_0 : Natural;
                            C_M : Natural) return Action_List is

    The_Action_List : Action_List(1..3);
    The_Last_Action : Natural := The_Action_List'first - 1;

    procedure Append (The_Action : Action) is
    begin
      if (The_Action.K = Keep_Speed) and (The_Action.S = 0) and (The_Action.N = 0) then
        Log.Error ("action list for => S = N = 0");
      end if;
      The_Last_Action := The_Last_Action + 1;
      The_Action_List(The_Last_Action) := The_Action;
    end Append;

    function N_Of_Cm return Natural is
      type Long is range 0 .. 2**63 - 1;
    begin
      return Natural((Long(Long(C_0) * Long(C_0) * 9) / (Long(C_M) * Long(C_M) * 16)));
    exception
    when others =>
      return 0;
    end N_Of_Cm;

    N  : constant Natural := abs S;
    Nm : Natural := N_Of_Cm;
    R  : Natural;
    D  : Step_Direction;

  begin -- Action_List_For
    if S /= 0 then
      if S > 0 then
        D := Forward;
      else
        D := Backward;
      end if;
      if Nm > 0 then
        if N < 2 * Nm then
          Nm := N / 2;
        end if;
        R := N - (2 * Nm);
        Append ((N => 0,
                 C => C_0,
                 S => Nm,
                 K => Accelerate,
                 D => D));
        if R > 0 then
          Append ((N => 0,
                   C => 0,
                   S => R,
                   K => Keep_Speed,
                   D => D));
        end if;
        Append ((N => Nm,
                 C => 0,
                 S => Nm,
                 K => Decelerate,
                 D => D));
      else
        Append ((N => 0,
                 C => C_M,
                 S => N,
                 K => Keep_Speed,
                 D => D));
      end if;
    end if;
    return The_Action_List(The_Action_List'first .. The_Last_Action);
  end Action_List_For;


  procedure Move (The_Distance : Values) is

    Distance_1 : constant Integer := N_Of (The_Distance(D1), D1);
    Distance_2 : constant Integer := N_Of (The_Distance(D2), D2);

    M1_Actions : constant Action_List := Action_List_For (Distance_1, C0(D1), Cm(D1));
    M2_Actions : constant Action_List := Action_List_For (Distance_2, C0(D2), Cm(D2));

  begin -- Move
    Log.Write ("move");
    Protocol.Transfer (M1 => M1_Actions,
                       M2 => M2_Actions);
  end Move;


  function Maximum_Steps_Of (The_Speed : Value) return Integer is
    use type Value;
  begin
    if The_Speed > 0.0 then
      return Integer'last;
    else
      return -Integer'last;
    end if;
  end Maximum_Steps_Of;


  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Value) is

    Actions : constant Action_List := Action_List_For (S   => Maximum_Steps_Of (With_Speed),
                                                       C_0 => C0(The_Drive),
                                                       C_M => Dt_Of (With_Speed, The_Drive));
  begin
    Log.Write ("direct " & The_Drive'img & " with speed" & With_Speed'img);
    case The_Drive is
    when D1 =>
      Protocol.Transfer (M1 => Actions);
    when D2 =>
      Protocol.Transfer (M2 => Actions);
    end case;
  end Direct;


  T_End : Time.Ut;

  procedure Synchronize_Time (The_Time : out Time.Ut) is
  begin
    Log.Write ("synchronize time");
    Protocol.Do_Synchronize_Time (The_Time);
    Initialize_Data;
    T_End := 0.0;
  end Synchronize_Time;


  procedure Update (Profile : Update_Profile) is

    procedure Append (D  :        Drive;
                      Da : in out Drive_Data;
                      A  : in out Action;
                      Ve :        Value;
                      K  :        Boolean;
                      S  : in out Integer;
                      T  : in out Natural;
                      Tr : in out Integer) is

      C_Begin : constant Natural := Da.C;
      C_End   : constant Natural := Dt_Of (Ve, D);

      procedure Append_Slow_Continue is
        use type Value;
      begin
        if S > 0 then
          Da.D := Forward;
        elsif S < 0 then
          Da.D := Backward;
        else
          Da.D := Undefined;
        end if;
        A.S := abs (S);
        A.K := Keep_Speed;
        if A.S = 0 then
          A.D := Undefined;
          A.N := T; -- rest clocks
          A.C := 0;
          Da.V := 0.0;
        else
          A.C := T / A.S;
          A.N := T - A.C * A.S; -- rest clocks
          Da.V := Ve;
          A.D := Da.D;
          if Ve = 0.0 then
            Da.C := Infinite;
          else
            Da.C := C_End;
          end if;
        end if;
      end Append_Slow_Continue;

      procedure Append_Continue is
        use type Value;
        Tn : Natural;
      begin
        if S > 0 then
          Da.D := Forward;
        elsif S < 0 then
          Da.D := Backward;
        else
          Da.D := Undefined;
        end if;
        A.S := abs (S);
        A.K := Keep_Speed;
        if A.S = 0 then
          raise Program_Error;
        else
          A.C := T / A.S;
          if A.C < Da.Cmin then
            A.C := Da.Cmin;
            A.S := T / A.C;
            A.N := 0;
            S := A.S;
            Tn := A.S * A.C;
            Tr := T - Tn;
            T := Tn;
            if Da.D = Backward then
              S := -S;
            end if;
            Log.Warning ("continue correction Tr:" & Tr'img);
          else
            A.N := T - A.C * A.S; -- rest clocks
          end if;
          Da.V := Ve;
          A.D := Da.D;
          if Ve = 0.0 then
            Da.C := Infinite;
          else
            Da.C := C_End;
          end if;
        end if;
      end Append_Continue;

      procedure Append_Simulation is

        use type Value;

        Te : Natural := T;

        procedure Simulate (Sn : out Natural) is
        begin
          Simulate (T  => T,
                    Cb => A.C,
                    Nb => A.N,
                    K  => A.K,
                    Cm => Da.Cmin,
                    Te => Te,
                    Sn => Sn,
                    Ce => Da.C);
        end Simulate;

        Sn1 : Natural;

      begin -- Append_Simulation
        if S > 0 then
          Da.D := Forward;
        elsif S < 0 then
          Da.D := Backward;
        else
          Da.D := Undefined;
        end if;
        A.S := abs (S);
        A.D := Da.D;
        if Da.V < Ve then
          if Da.D = Forward then
            A.K := Accelerate;
          else
            A.K := Decelerate;
          end if;
        elsif Da.V > Ve then
          if Da.D = Forward then
            A.K := Decelerate;
          else
            A.K := Accelerate;
          end if;
        end if;
        if Da.C >= Da.Cmax then
          A.C := C0_Of (T, Ve, D);
          A.N := 0;
          if Log.Is_Enabled then
            Log.Write ("## simulate startup - D.C =>" & Da.C'img);
          end if;
          Simulate (Sn1);
        else
          Da.V := V_Of (Da.C, D);
          if Da.D = Backward then
            Da.V := - Da.V;
          end if;
          A.N := N_Of (T, Da.V, Ve, D);
          A.C := Da.C;
          if Log.Is_Enabled then
            Log.Write ("## simulate continuation: D.V =>" & Da.V'img & "; Ve =>" & Ve'img);
          end if;
          Simulate (Sn1);
        end if;
        if Da.D = Forward then
          S := A.S;
        else
          S := -A.S;
        end if;
        Tr := T - Te;
        T := Te;
        Da.V := V_Of (Da.C, D);
        if Da.D = Backward then
          Da.V := - Da.V;
        end if;
        if A.K = Decelerate then
          if A.N < A.S then
            A.N := A.S;
          end if;
        end if;
      end Append_Simulation;

    begin -- Append
      T := T + Tr;
      Tr := 0;
      if Log.Is_Enabled then
        Log.Write ("########################");
        Log.Write ("## C begin =>" & C_Begin'img);
        Log.Write ("## C end   =>" & C_End'img);
        Log.Write ("## V end   =>" & Ve'img);
        Log.Write ("## steps   =>" & S'img);
        Log.Write ("## clocks  =>" & T'img);
        Log.Write ("##======================");
      end if;
      if ((C_Begin >= Da.Cmax / 2) and (C_End >= Da.Cmax / 2)) then
        Log.Write ("## append case 1");
        Append_Slow_Continue;
      elsif S /= 0 then
        declare
          Ck : constant Natural := abs (T / S);
          Cd : constant Natural := Ck / 64;
        begin
          if Log.Is_Enabled then
            Log.Write ("## Ck:" & Ck'img);
            Log.Write ("## Cd:" & Cd'img);
          end if;
          if K then
            Log.Write ("## append case 2");
            Append_Continue;
          elsif (abs S = 1) or abs (C_Begin - C_End) <= Cd then
            Log.Write ("## append case 3");
            Append_Continue;
          else
            Log.Write ("## append case 4");
            Append_Simulation;
          end if;
        end;
      else
        Log.Write ("## append case 1a");
        Append_Slow_Continue;
      end if;
      if Log.Is_Enabled then
        Log.Write ("## A.N =>" & A.N'img);
        Log.Write ("## A.C =>" & A.C'img);
        Log.Write ("## A.S =>" & A.S'img);
        Log.Write ("## A.K => " & A.K'img);
        Log.Write ("## A.D => " & A.D'img);
      end if;
      Da.S := Da.S + S;
      Da.T := Da.T + Counter(T);
      if Log.Is_Enabled then
        Log.Write ("## Tr  =>" & Tr'img);
        Log.Write ("########################");
      end if;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Append;

    Delta_Time : constant Time.Ut := Time_Delta / Actions_Per_Update;

    subtype Update_List is Action_List (1 .. Actions_Per_Update);

    type Update_Lists is array (Device.Drive) of Update_List;

    The_Updates : Update_Lists;
    T           : Time.Ut := T_End;
    P           : Update_Profile := Profile;

    use type Angle.Unsigned;
    use type Angle.Value;
    use type Value;

  begin -- Update
    for D in Device.Drive loop
      declare
        Da  : Drive_Data renames The_Data(D);
      begin
        Da.Tb := T;
        Da.S := N_Of (P(D).Sb, D) + Da.Sr;
      end;
    end loop;
    for The_Index in Update_List'range loop
      T := T + Delta_Time;
      for D in Device.Drive loop
        declare
          Da : Drive_Data renames The_Data(D);
          Dt : Natural := C_Of (Delta_Time);
          Ve : Value;
          Ds : Integer;
          Es : Integer;
        begin
          if Log.Is_Enabled then
            Log.Write ("=========================");
            Log.Write ("  Update Parameters");
            Log.Write ("-------------------------");
            Log.Write ("  Sb  => " & Angle.Image_Of (+P(D).Sb));
            Log.Write ("  Vb  =>" & P(D).Vb'img);
            Log.Write ("  Tb  =>" & Da.Tb'img);
            Log.Write ("  T   =>" & T'img);
          end if;
          Get (S       => Da.Se,
               V       => Ve,
               T       => T,
               T_Begin => Da.Tb,
               P       => P(D));
          Ds := Distance (From => Da.S, To => N_Of (Da.Se, D), D => D);
          if Log.Is_Enabled then
            Log.Write ("  Se  => " & Angle.Image_Of (+Da.Se));
            Log.Write ("  Ve  =>" & Ve'img);
            Log.Write ("-------------------------");
            Log.Write ("  Da.S  =>" & Da.S'img);
            Log.Write ("  Da.T  =>" & Da.T'img);
            Log.Write ("  Da.V  =>" & Da.V'img);
            Log.Write ("  Da.Tr =>" & Da.Tr'img);
            Log.Write ("  Dt    =>" & Dt'img);
            Log.Write ("-------------------------");
          end if;
          Es := Ds;
          Append (D  => D,
                  A  => The_Updates(D)(The_Index),
                  Da => Da,
                  Ve => Ve,
                  K  => Da.V = Ve,
                  S  => Ds,
                  T  => Dt,
                  Tr => Da.Tr);
          Da.Sr := Ds - Es;
          Log.Write ("==> Sr:" & Da.Sr'img);
          if abs Da.Sr > 1 then
            if abs Da.Sr > 7 then
              Log.Error ("too big Da.Sr:" & Da.Sr'img);
            else
              Log.Warning ("D.Sr:" & Da.Sr'img);
            end if;
          end if;
        end;
      end loop;
    end loop;
    Protocol.Transfer (M1 => The_Updates(D1),
                       M2 => The_Updates(D2));
    T_End := T;
  end Update;


  function Offset_Per_Action_For (The_Speed : Value;
                                  The_Drive : Device.Drive) return Step_Count is
    The_Offset : Step_Count := N_Of (The_Speed, The_Drive);
  begin
    if The_Offset > 0 then
      The_Offset := (The_Offset + (Actions_Per_Second / 2)) / Actions_Per_Second;
      if The_Offset = 0 then
        The_Offset := 1;
      end if;
    elsif The_Offset < 0 then
      The_Offset := (The_Offset - (Actions_Per_Second / 2)) / Actions_Per_Second;
      if The_Offset = 0 then
        The_Offset := -1;
      end if;
    end if;
    return The_Offset;
  end Offset_Per_Action_For;


  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Value) is
  begin
    Log.Write ("adjust " & The_Drive'img & " with speed" & With_Speed'img);
    Protocol.Do_Adjust (The_Drive, Offset_Per_Action_For (With_Speed, The_Drive));
  end Adjust;


  function Autoguiding_Offset_Of (The_Drive : Device.Drive) return Step_Count is
  begin
    return Offset_Per_Action_For (The_Autoguiding_Speed, The_Drive);
  end Autoguiding_Offset_Of;


  procedure Halt is
  begin
    Log.Write ("halt");
    Protocol.Do_Stop;
  end Halt;


  procedure Close is
  begin
    Log.Write ("close");
    Protocol.Do_Close_Communication;
  end Close;

end Motor.Io;
