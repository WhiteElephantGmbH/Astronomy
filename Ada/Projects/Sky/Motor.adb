-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Astro;
with Motor.Io;
with Persistent;
with System;
with Traces;

package body Motor is

  package Log is new Traces ("Motor");


  P1 : Parameters; -- Motor1
  P2 : Parameters; -- Motor2

  Epsilon : Values;

  use all type Device.Drive;


  function Is_Equal (Left, Right : Value;
                     D           : Device.Drive) return Boolean is
    use type Value;
  begin
    return abs (Left - Right) < Epsilon(D);
  end Is_Equal;


  function "=" (Left, Right : Values) return Boolean is
  begin
    return Is_Equal (Left(D1), Right(D1), D1) and Is_Equal (Left(D2), Right(D2), D2);
  end "=";


  function Is_Zero (Item : Values) return Boolean is
    use type Value;
  begin
    return (abs (Item(D1)) < Epsilon(D1)) and (abs (Item(D2)) < Epsilon(D2));
  end Is_Zero;


  function "-" (Left, Right : Values) return Values is
    use type Value;
  begin
    return (D1 => Left(D1) - Right(D1),
            D2 => Left(D2) - Right(D2));
  end "-";


  type Stored_Data is record
    Is_Parked       : Boolean := False;
    Is_Inverted     : Boolean := False;
    Actual_Position : Values; -- actual telescope position
  end record;

  package Persistent_Telescope is new Persistent (Stored_Data, "Telescope");

  The_Telescope : Persistent_Telescope.Data;

  type Movement is (Directing, Parking, Positioning);

  The_Movement : Movement := Directing;

  Is_Inverted : Boolean renames The_Telescope.Storage.Is_Inverted;
  Is_Parked   : Boolean renames The_Telescope.Storage.Is_Parked;

  Was_Parked     : Boolean;
  AP : Values renames The_Telescope.Storage.Actual_Position;
  PP : Values; -- park position
  LL : Values; -- lower limits
  UL : Values; -- upper limits
  VM : Values; -- maximum speed
  AM : Values; -- maximum accelleration


  procedure Invert (The_Position : in out Values) is
    use type Value;
  begin
    The_Position(D1) := The_Position(D1) - 180.0;
    if The_Position(D1) < 0.0 then
      The_Position(D1) := The_Position(D1) + 360.0;
    end if;
    The_Position(D2) := 180.0 - The_Position(D2);
  end Invert;


  function In_Limits (Item : Value;
                      D    : Drive) return Boolean is
    use type Value;
  begin
    return Item >= LL(D) and Item < UL(D);
  end In_Limits;


  function In_Limits (Item : Values) return Boolean is
  begin
    return In_Limits (Item(D1), D1) and In_Limits (Item(D2), D2);
  end In_Limits;


  function Is_Stepper return Boolean is
  begin
    return Io.Is_Stepper_Driver;
  end Is_Stepper;


  function Version return Hardware_Version is
  begin
    return Io.Device_Version;
  end Version;


  procedure Define_Parameters is
    use type Value;
  begin
    if Persistent_Telescope.Storage_Is_Empty then
      Is_Inverted := PP(D1) >= 180.0;
      AP := PP;
      if Is_Inverted then
        Invert (AP);
      end if;
      Is_Parked := True;
    end if;
    Was_Parked := Is_Parked;
  end Define_Parameters;


  procedure Define (First_Acceleration   : Angle.Value;
                    Second_Acceleration  : Angle.Value;
                    Maximum_Speed        : Angle.Value;
                    Park_Position        : Position;
                    Pole_Height          : Angle.Value;
                    Meridian_Flip_Offset : Angle.Value) is

    use type Angle.Value;
    use type Angle.Signed;
    use type Value;

    PH  : constant Value := +Pole_Height;
    MFO : constant Value := +Meridian_Flip_Offset;

  begin
    P1 := (Am => +First_Acceleration,
           Vm => +Maximum_Speed,
           D  => D1);
    P2 := (Am => +Second_Acceleration,
           Vm => P1.Vm,
           D  => D2);
    Io.Define (Parameters_1 => P1,
               Parameters_2 => P2,
               The_Epsilon  => Epsilon);
    Log.Write ("Epsilon_1 =>" & Epsilon(D1)'img);
    Log.Write ("Epsilon_2 =>" & Epsilon(D2)'img);
    AM(D1) := +First_Acceleration;
    AM(D2) := +Second_Acceleration;
    VM(D1) := +Maximum_Speed;
    VM(D2) := +Maximum_Speed;
    LL(D1) := -MFO;
    LL(D2) := PH - 90.0 - Epsilon(D2);
    UL(D1) := 180.0 + MFO;
    UL(D2) := 270.0 - PH + Epsilon(D2);
    PP(D1) := +Park_Position.First;
    PP(D2) := +Angle.Signed'(+Park_Position.Second);
    Log.Write ("Lower Limit - M1:" & LL(D1)'img);
    Log.Write ("            - M2:" & LL(D2)'img);
    Log.Write ("Upper Limit - M1:" & UL(D1)'img);
    Log.Write ("            - M2:" & UL(D2)'img);
    Log.Write ("Park Posit. - M1:" & PP(D1)'img);
    Log.Write ("            - M2:" & PP(D2)'img);
    Define_Parameters;
  end Define;


  type Profile is record
    Sb : Value;          -- distance at begin
    Se : Value;          -- distance at end
    Tb : Time.Ut := 0.0; -- time at begin
    Tm : Time.Ut := 0.0; -- maximum speed reached
    Td : Time.Ut := 0.0; -- deaccelleration begin
    Tf : Time.Ut := 0.0; -- following begin (deaccelleration end)
    Te : Time.Ut := 0.0; -- time at end
    V  : Value   := 0.0; -- actual speed
    Vb : Value   := 0.0; -- speed at begin
    Ve : Value   := 0.0; -- speed at end
    A  : Value   := 0.0; -- actual accelleration
    E  : Value;
  end record;


  M1   : Profile; -- Motor1
  M2   : Profile; -- Motor2
  M1_C : Profile; -- Motor1 current
  M2_C : Profile; -- Motor2 current
  M1_N : Profile; -- Motor1 next
  M2_N : Profile; -- Motor2 next

  Tb : Time.Ut;
  Te : Time.Ut;

  Inversion_Is_Enabled : Boolean := False;


  function Distance (From, To: Value;
                     D       : Drive) return Value is

    use type Value;

    R : constant Integer := Integer((AP(D) - From) / 360.0);

    FP  : constant Value := Value(360 * R) + From; -- real from position

    TP  : Value; -- real to position
    TPL : Value; -- nearest to position before FP
    TPU : Value; -- nearest to position after FP

  begin -- Distance
    Log.Write ("Distance ");
    Log.Write ("  From:" & From'img);
    Log.Write ("  To  :" & To'img);
    TPL := Value ((R + 1) * 360) + To;
    while TPL > FP loop
      TPL := TPL - 360.0;
    end loop;
    TPU := TPL + 360.0;
    if TPU >= UL(D) then
      if TPL >= LL(D) then
        TP := TPL;
      else
        raise At_Limit;
      end if;
    elsif TPL < LL(D) then
      if TPU < UL(D) then
        TP := TPU;
      else
        raise At_Limit;
      end if;
    else
      raise At_Limit;
    end if;
    return TP - FP;
  end Distance;


  The_State_Handler : State_Handler_Access;


  task type Receiver with Priority => System.Max_Priority is
    entry Start;
  end Receiver;

  type Receiver_Access is access Receiver;

  The_Receiver   : Receiver_Access;
  Is_Closing     : Boolean := False;
  The_Last_State : State := Update;


  task body Receiver is
    The_Duration     : Time.Ut := 0.0;
    The_Start_Time   : Time.Ut := 0.0;
    The_Counter      : Natural := 0;
    The_Device_State : Device.State;
    The_State        : State;
    use all type Device.State;
  begin
    accept Start;
    while not Is_Closing loop
      begin
        The_Device_State := Io.Actual_State;
        case The_Device_State is
        when Disconnected =>
          The_State := Motor.Disconnected;
        when Fault =>
          The_State := Motor.Fault;
        when Startup =>
          The_State := Motor.Startup;
        when Initialized =>
          The_State := Motor.Ready;
        when Stopped =>
          AP := Io.Actual_Data.Positions;
          Log.Write ("Stopped AP - M1:" & AP(D1)'img);
          Log.Write ("           - M2:" & AP(D2)'img);
          Log.Write ("     - Inverted: " & Is_Inverted'img);
          declare
            APP : Values := PP;
          begin
            if Is_Inverted then
              Invert (APP);
            end if;
            if AP = APP then
              Is_Parked := True;
              Was_Parked := True;
              The_State := Motor.Parked;
            else
              Is_Parked := False;
              if Was_Parked then
                The_State := Motor.Positioned;
              else
                The_State := Motor.Ready;
              end if;
            end if;
          end;
        when Moving =>
          AP := Io.Actual_Data.Positions;
          Is_Parked := False;
          case The_Movement is
          when Parking =>
            The_State := Motor.Parking;
          when Positioning =>
            The_State := Motor.Positioning;
          when Directing =>
            The_State := Motor.Directing;
          end case;
        when Synchronised =>
          AP := Io.Actual_Data.Positions;
          Is_Parked := False;
          The_State := Motor.Update;
        end case;
        if The_State = Motor.Update then
          if The_Start_Time = 0.0 then
            The_Start_Time := Time.Universal;
          else
            The_Counter := The_Counter + 1;
            The_Duration := (Time.Universal - The_Start_Time) / The_Counter;
            Log.Write ("<COUNTER>" & The_Counter'img);
            Log.Write ("<DURATION>" & The_Duration'img);
          end if;
          The_State_Handler (The_State);
        elsif The_State /= The_Last_State then
          The_Duration := 0.0;
          The_Start_Time := 0.0;
          The_Counter := 0;
          The_State_Handler (The_State);
        end if;
        The_Last_State := The_State;
      exception
      when Driver_Closing =>
        null;
      when Item: others =>
        Log.Termination (Item);
      end;
    end loop;
    The_State_Handler (Terminated);
  end Receiver;


  procedure Connect_Communication renames Io.Connect;

  procedure Open_Communication (State_Handler : State_Handler_Access) is
  begin
    The_State_Handler := State_Handler;
    Io.Open;
    The_Receiver := new Receiver;
    The_Receiver.Start;
  end Open_Communication;


  procedure Initialize is
  begin
    Io.Startup_Initialization;
  end Initialize;


  procedure Set (Autoguiding_Rate : Device.Autoguiding_Rate) is
  begin
    Io.Set_Autoguiding (Autoguiding_Rate);
  end Set;


  procedure Define_Positions is
  begin
    Io.Set_Positions (AP);
  end Define_Positions;


  procedure Synch_Park_Position is
    APP : Values := PP;
    use type Value;
  begin
    Is_Inverted := PP(D1) >= 180.0;
    if Is_Inverted then
      Invert (APP);
    end if;
    Io.Set_Positions (APP);
  end Synch_Park_Position;


  function Assigned (The_Values    : out Values;
                     The_Position  :     Position) return Boolean is

    use type Angle.Value;
    use type Value;

    Se : Values;

  begin
    The_Values(D1) := +The_Position.First;
    The_Values(D2) := +The_Position.Second;
    if The_Values (D2) > (UL(D2) + Epsilon(D2)) then
      The_Values(D2) := The_Values(D2) - 360.0;
    end if;
    Log.Write ("Assigned - V1:" & The_Values(D1)'img & " - V2:" & The_Values(D2)'img & " - INV: " & Is_Inverted'img);
    Se := The_Values;
    if Is_Inverted then
      Invert (Se);
    end if;
    if not In_Limits (Se) then
      if Inversion_Is_Enabled then
        Is_Inverted := not Is_Inverted;
      else
        return False;
      end if;
    end if;
    if Is_Inverted then
      Invert (The_Values);
    end if;
    return True;
  end Assigned;


  procedure Synch_Position (To : Position) is
    P : Values;
  begin
    Allow_Inversion;
    if Assigned (P, To) then
      AP := P;
      Io.Set_Positions (P);
    else
      Log.Error ("synch position not assigned");
    end if;
  end Synch_Position;


  function Time_For_Positioning (To : Values) return Time.Ut is

    Pe : Values := To;

    The_Distances : Values;
    The_Time_1    : Time.Ut;
    The_Time_2    : Time.Ut;

    use type Value;

    function Time_For (D : Drive) return Time.Ut is
      S  : constant Value := abs The_Distances(D);
      Sm : constant Value := VM(D) * VM(D) / AM(D);
      use Astro;
    begin
      if S >= Sm then
        return Time.Ut (S / VM(D) + VM(D) / AM(D));
      else
        return Time.Ut (2.0 * SQRT (S /AM(D)));
      end if;
    end Time_For;

  begin -- Time_For_Positioning
    Log.Write ("Time_For_Positioning To - M1:" & To(D1)'img & " - M2:" & To(D2)'img);
    Log.Write ("                     AP - M1:" & AP(D1)'img & " - M2:" & AP(D2)'img);
    Log.Write ("                 Is_Inverted: " & Is_Inverted'img);
    if Is_Inverted then
      if Pe(D1) < 180.0 then
        Is_Inverted := False;
      else
        Invert (Pe);
      end if;
    else
      if Pe(D1) >= 180.0 then
        Invert (Pe);
        Is_Inverted := True;
      end if;
    end if;
    The_Distances := Pe - AP;
    if Is_Zero (The_Distances) then
      return Time.In_The_Past;
    end if;
    Io.Move (The_Distances);
    The_Time_1 := Time_For(D1);
    The_Time_2 := Time_For(D2);
    if The_Time_1 >= The_Time_2 then
      return Time.Universal + The_Time_1;
    else
      return Time.Universal + The_Time_2;
    end if;
  exception
  when At_Limit =>
    Log.Error ("Can't position");
    return Time.In_The_Past;
  end Time_For_Positioning;


  function Time_When_Parked return Time.Ut is
  begin
    The_Movement := Parking;
    return Time_For_Positioning (To => (D1 => PP(D1),
                                        D2 => PP(D2)));
  end Time_When_Parked;


  function Time_When_Positioned (To : Position) return Time.Ut is
    use type Angle.Signed;
    use type Angle.Value;
  begin
    The_Movement := Positioning;
    return Time_For_Positioning (To => (D1 => +To.First,
                                        D2 => +Angle.Signed'(+To.Second)));
  end Time_When_Positioned;


  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed) is
    use type Angle.Signed;
  begin
    The_Movement := Directing;
    Io.Direct (The_Drive, Value'(+With_Speed));
  end Direct;


  procedure Log_Values (T : String;
                        M : Profile) is
    use type Angle.Value;
  begin
    if Log.Is_Enabled then
      Log.Write (T);
      Log.Write ("  Tb => " & Time.Image_Of (M.Tb));
      Log.Write ("  Tm => " & Time.Image_Of (M.Tm));
      Log.Write ("  Td => " & Time.Image_Of (M.Td));
      Log.Write ("  Tf => " & Time.Image_Of (M.Tf));
      Log.Write ("  Te => " & Time.Image_Of (M.Te));
      Log.Write ("  Sb => " & Angle.Image_Of (+M.Sb));
      Log.Write ("  Se => " & Angle.Image_Of (+M.Se));
      Log.Write ("  V  =>" & M.V 'img);
      Log.Write ("  Vb =>" & M.Vb'img);
      Log.Write ("  Ve =>" & M.Ve'img);
      Log.Write ("  A  =>" & M.A 'img);
    end if;
  end Log_Values;


  function Max (X, Y : Time.Ut) return Time.Ut is
  begin
    if X > Y then
      return X;
    else
      return Y;
    end if;
  end Max;


  function Max (X, Y : Value) return Value is
    use type Value;
  begin
    if X > Y then
      return X;
    else
      return Y;
    end if;
  end Max;


  function Min (X, Y : Value) return Value is
    use type Value;
  begin
    if X < Y then
      return X;
    else
      return Y;
    end if;
  end Min;


  function Assigned (The_Position : Position;
                     The_Speed    : Speed) return Boolean is

    use type Angle.Signed;
    use type Value;

    The_Values : Values;

  begin
    if Assigned (The_Values, The_Position) then
      M1.Se := The_Values(D1);
      M1.Ve := +The_Speed.First;
      M2.Se := The_Values(D2);
      M2.Ve := +The_Speed.Second;
      if Is_Inverted then
        M2.Ve := - M2.Ve;
      end if;
      return True;
    else
      return False;
    end if;
  end Assigned;


  procedure Initialize_Profile (T :        Time.Ut;
                                S :        Value;
                                V :        Value := 0.0;
                                M : in out Profile) is
  begin
    M.Tb := T;
    M.Tm := M.Tb;
    M.Td := M.Tb;
    M.Tf := M.Tb;
    M.Te := T + Time_Delta;
    M.Sb := S;
    M.Se := S;
    M.Vb := V;
    M.Ve := V;
    M.V := V;
    M.A := 0.0;
  end Initialize_Profile;


  procedure Split (M  : in out Profile;
                   Mn :    out Profile) is
    use type Value;
  begin
    Mn := M;
    Mn.Te := Te;
    M.Tb := Te;
    if Te < M.Tm then
      Mn.Ve := M.Vb + (Value(Te - Tb) * M.A);
      Mn.Se := Mn.Sb + ((M.Vb + Mn.Ve) * Value(Te - Tb) / 2.0);
      Mn.Tm := Te;
      Mn.Td := Te;
      Mn.Tf := Te;
      Mn.V := Mn.Ve;
    elsif Te <= M.Td then
      Mn.Ve := M.V;
      Mn.Se := Mn.Sb + ((M.Vb + Mn.V) * Value(M.Tm - Tb) / 2.0)
                     + (M.V * Value(Te - M.Tm));
      Mn.Td := Te;
      Mn.Tf := Te;
      Mn.Ve := M.V;
      M.Tm := Te;
    elsif Te < M.Tf then
      Mn.Ve := M.V - (Value(Te - M.Td) * M.A);
      Mn.Se := Mn.Sb + ((M.Vb + Mn.V) * Value(M.Tm - Tb) / 2.0)
                     + (M.V * Value(M.Td - M.Tm))
                     + ((M.V + Mn.Ve) * Value(Te - M.Td) / 2.0);
      Mn.Tf := Te;
      M.Tm := Te;
      M.Td := Te;
      M.V := Mn.Ve;
    elsif Te < M.Te then
      Mn.Se := Mn.Sb + ((M.Vb + Mn.V) * Value(M.Tm - Tb) / 2.0)
                     + (M.V * Value(M.Td - M.Tm))
                     + ((M.V + Mn.Ve) * Value(M.Tf - M.Td) / 2.0)
                     + (M.Ve * Value(Te - M.Tf));
      M.Tm := Te;
      M.Td := Te;
      M.Tf := Te;
    else
      M.Tm := Te;
      M.Td := Te;
      M.Tf := Te;
      M.V := Mn.Ve;
    end if;
    M.Sb := Mn.Se;
    M.Vb := Mn.Ve;
  end Split;


  function Approaches (M : in out Profile;
                       P :        Parameters) return Boolean is
    Dte : Time.Ut;
    Dse, Dsi, Dsa, Dsb, Dsl, Dsu, Am_Dte, Vb_P_Ve, Vb_M_Ve, Vb_Ve2, X, Y : Value;
    use Astro;
    use type Angle.Value;
  begin
    if Log.Is_Enabled then
      Log.Write ("Sb  => " & Angle.Image_Of (+M.Sb));
      Log.Write ("Se  => " & Angle.Image_Of (+M.Se));
      Log.Write ("Tb  => " & Time.Image_Of (M.Tb));
      Log.Write ("Te  => " & Time.Image_Of (M.Te));
      Log.Write ("Vb  =>" & M.Vb'img);
      Log.Write ("Ve  =>" & M.Ve'img);
    end if;
    if abs M.Ve > P.Vm then
      return False;
    end if;
    Dte := M.Te - M.Tb;
    Dse := Distance (From => M.Sb, To => M.Se, D => P.D);
    Dsi := Value(Dte) * (M.Vb + M.Ve) / 2.0;
    Vb_P_Ve := M.Vb + M.Ve;
    Vb_M_Ve := M.Vb - M.Ve;
    if abs(Vb_M_Ve) < M.E then
      Vb_M_Ve := 0.0;
    end if;
    Vb_Ve2 := Vb_M_Ve * Vb_M_Ve;
    X := Vb_Ve2 / (2.0 * P.Am);
    Dsa := Max (M.Vb, M.Ve) * Value(Dte) - X;
    Dsb := Min (M.Vb, M.Ve) * Value(Dte) + X;
    Am_Dte := P.Am * Value(Dte);
    Y := (Am_Dte * Value(Dte)) / 4.0 - X / 2.0;
    Dsl := Dsi - Y;
    Dsu := Dsi + Y;
    if Log.Is_Enabled then
      Log.Write ("Dte =>" & Dte'img);
      Log.Write ("Dse =>" & Dse'img);
      Log.Write ("Dsi =>" & Dsi'img);
      Log.Write ("Dsa =>" & Dsa'img);
      Log.Write ("Dsb =>" & Dsb'img);
      Log.Write ("Dsl =>" & Dsl'img);
      Log.Write ("Dsu =>" & Dsu'img);
    end if;
    if Dse < (Dsl - M.E) then
      Log.Warning ("Dse < Dsl");
      return False;
    else
      if Dse > Dsu + Epsilon(P.D) then
        Log.Warning ("Dse > Dsu");
        return False;
      end if;
    end if;
    if Vb_M_Ve /= 0.0 then
      if Dse >= Dsb and Dse <= Dsa then
        M.Tm := M.Tb;
        M.V := M.Vb;
        if Dse >= Dsi then
          if M.Ve > M.Vb then
            declare
              Dtf : constant Time.Ut := Time.Ut(2.0 * (Dse - Value(Dte) * M.Ve) / Vb_M_Ve);
            begin
              M.A := Vb_M_Ve / Value(Dtf);
              if Log.Is_Enabled then
                Log.Write ("@1 Dse >= Dsi & Ve > Vb");
                Log.Write ("Dtf =>" & Dtf'img);
                Log.Write ("M.A =>" & M.A'img);
              end if;
              M.Td := M.Tb;
              M.Tf := M.Tb + Dtf;
              X := Value(Dte) * M.Ve - (Value(Dtf) * (M.Ve - M.Vb) / 2.0);
              if abs(Dse - X) > Epsilon(P.D) then
                Log.Error ("@1 Dse <> X - Dse ->" & Dse'img & " - X =>" & X'img);
                return False;
              end if;
            end;
          else -- Ve < Vb
            declare
              Dtd : constant Time.Ut := Time.Ut((2.0 * Dse - Value(Dte) * Vb_P_Ve) / Vb_M_Ve);
            begin
              M.A := Vb_M_Ve / Value(Dte - Dtd);
              if Log.Is_Enabled then
                Log.Write ("@2 Dse >= Dsi & Ve < Vb");
                Log.Write ("Dtd =>" & Dtd'img);
                Log.Write ("M.A =>" & M.A'img);
              end if;
              M.Td := M.Tb + Dtd;
              M.Tf := M.Te;
              X := Value(Dte) * M.Vb - (Value(Dte - Dtd) * (M.Vb - M.Ve) / 2.0);
              if abs(Dse - X) > Epsilon(P.D) then
                Log.Error ("@2 Dse <> X - Dse ->" & Dse'img & " - X =>" & X'img);
                return False;
              end if;
            end;
          end if;
        else -- Dse < Dsi
          if M.Ve > M.Vb then
            declare
              Dtd : constant Time.Ut := Time.Ut((2.0 * Dse - Value(Dte) * Vb_P_Ve) / Vb_M_Ve);
            begin
              M.A := Vb_M_Ve / Value(Dte - Dtd);
              if Log.Is_Enabled then
                Log.Write ("@3 Dse < Dsi & Ve > Vb");
                Log.Write ("Dtd =>" & Dtd'img);
                Log.Write ("M.A =>" & M.A'img);
              end if;
              M.Td := M.Tb + Dtd;
              M.Tf := M.Te;
              X := Value(Dte) * M.Vb + (Value(Dte - Dtd) * (M.Ve - M.Vb) / 2.0);
              if abs(Dse - X) > Epsilon(P.D) then
                Log.Error ("@3 Dse <> X - Dse ->" & Dse'img & " - X =>" & X'img);
                return False;
              end if;
            end;
          else -- Ve < Vb
            declare
              Dtf : constant Time.Ut := Time.Ut(2.0 * (Dse - Value(Dte) * M.Ve) / Vb_M_Ve);
            begin
              M.A := Vb_M_Ve / Value(Dtf);
              if Log.Is_Enabled then
                Log.Write ("@4 Dse < Dsi & Ve < Vb");
                Log.Write ("Dtf =>" & Dtf'img);
                Log.Write ("M.A =>" & M.A'img);
              end if;
              M.Td := M.Tb;
              M.Tf := M.Tb + Dtf;
              X := Value(Dte) * M.Ve + (Value(Dtf) * (M.Vb - M.Ve) / 2.0);
              if abs(Dse - X) > Epsilon(P.D) then
                Log.Error ("@4 Dse <> X - Dse ->" & Dse'img & " - X =>" & X'img);
                return False;
              end if;
            end;
          end if;
        end if;
        return True;
      end if;
    end if;
    M.Tf := M.Te;
    X := Am_Dte * Am_Dte - Vb_Ve2;
    Y := 2.0 * P.Am * (Value(Dte) * Vb_P_Ve - 2.0 * Dse);
    if Log.Is_Enabled then
      Log.Write ("X   =>" & X'img);
      Log.Write ("Y   =>" & Y'img);
    end if;
    if Dse < Dsi then
      M.A := -P.Am;
      X := X - Y;
      if X <= 0.0 then
        if X < -Epsilon(P.D) then
          Log.Error ("X - Y =>" & X'img);
        end if;
        X := 0.0;
      else
        X := SQRT (X);
      end if;
    else
      M.A := P.Am;
      X := X + Y;
      if X <= 0.0 then
        if X < -Epsilon(P.D) then
          Log.Error ("X + Y =>" & X'img);
        end if;
        X := 0.0;
      else
        X := -SQRT (X);
      end if;
    end if;
    M.V := (M.A * Value(Dte) + Vb_P_Ve + X) / 2.0;
    if (abs(M.V) - P.Vm) > Epsilon(P.D) then
      Log.Warning ("speed to high =>" & M.V'img);
      return False;
    end if;
    if M.V > P.Vm then
      M.V := P.Vm;
    elsif M.V < -P.Vm then
      M.V := -P.Vm;
    end if;
    declare
      Dtd, Dtm : Time.Ut;
    begin
      Dtm := Time.Ut((M.V - M.Vb) / M.A);
      if Dtm < 0.0 then
        if Dtm < -Time.Epsilon then
          Log.Error ("Dtm =>" & Dtm'img);
        end if;
        Dtm := 0.0;
      end if;
      Dtd := Dte - Time.Ut((M.V - M.Ve) / M.A);
      if Dtd < Dtm then
        if (Dtd - Dtm) < -Time.Epsilon then
          Log.Error ("Dtd =>" & Dtd'img);
        end if;
        Dtd := Dtm;
      end if;
      M.Tm := M.Tb + Dtm;
      M.Td := M.Tb + Dtd;
    end;
    return True;
  exception
  when At_Limit =>
    raise;
  when Item: others =>
    Log.Termination (Item);
    return False;
  end Approaches;


  function Has_Arriving_Time (M : in out Profile;
                              P :        Parameters) return Boolean is
    Dte          : Time.Ut;
    Dse, X, Y, V : Value;
    use Astro;
  begin
    if abs(M.Ve) > P.Vm then
      Log.Warning ("Speed =>" & M.Ve'img & " (target not reachable)");
      return False;
    end if;
    Log_Values ("Has_Arriving_Time", M);
    Dse := Distance (From => M.Sb, To => M.Se, D => P.D);
    X := (M.Vb * M.Vb + M.Ve * M.Ve) / 2.0;
    Y := P.Am * Dse;
    V := SQRT(X + abs(Y));
    if V = 0.0 then
      Dte := 0.0;
    else
      if V <= P.Vm then
        V := 2.0 * V;
        if X < -Y then
          Dte := Time.Ut((V + M.Vb + M.Ve) / P.Am);
        else
          Dte := Time.Ut((V - M.Vb - M.Ve) / P.Am);
        end if;
      else
        X := P.Vm * P.Vm + X;
        Y := Y - P.Vm * (M.Vb + M.Ve);
        if X < -Y then
          Dte := Time.Ut((X - Y) / (P.Am * P.Vm));
        else
          Dte := Time.Ut((X + Y) / (P.Am * P.Vm));
        end if;
      end if;
    end if;
    M.Te := M.Tb + Dte;
    return True;
  exception
  when At_Limit =>
    raise;
  when Item: others =>
    Log.Termination (Item);
    return False;
  end Has_Arriving_Time;


  procedure Synchronize (Tbegin : out Time.Ut) is
    P : constant Values  := Io.Actual_Data.Positions;
  begin
    Io.Synchronize_Time (Tbegin);
    if Log.Is_Enabled then
      Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
      Log.Write ("synchronize => " & Time.Image_Of (Tbegin));
      Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
    end if;
    declare
      Tend : constant Time.Ut := Tbegin + Time_Delta;
    begin
      Initialize_Profile (T => Tbegin, M => M1_C, S => P(D1));
      Initialize_Profile (T => Tbegin, M => M2_C, S => P(D2));
      Initialize_Profile (T => Tbegin, M => M1_N, S => P(D1));
      Initialize_Profile (T => Tbegin, M => M2_N, S => P(D2));
      Initialize_Profile (T => Tend, M => M1, S => P(D1));
      Initialize_Profile (T => Tend, M => M2, S => P(D2));
    end;
  end Synchronize;


  procedure Change_Time (T : Time.Ut) is
  begin
    Tb := T + Time_Delta;
    Te := Tb + Time_Delta;
    M1_C := M1_N;
    M2_C := M2_N;
    if Log.Is_Enabled then
      Log.Write ("change time => " & Time.Image_Of (T));
      Log.Write ("Tb => " & Time.Image_Of (Tb));
      Log.Write ("Te => " & Time.Image_Of (Te));
    end if;
  end Change_Time;


  procedure Update is
    P : Update_Profile;
  begin
    Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
    Log.Write ("update M1");
    Log_Values ("old M1", M1);
    Split (M1, M1_N);
    Log_Values ("next M1", M1_N);
    Log_Values ("new M1", M1);
    Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
    Log.Write ("update M2");
    Log_Values ("old M2", M2);
    Split (M2, M2_N);
    Log_Values ("next M2", M2_N);
    Log_Values ("new M2", M2);
    Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
    P(D1) := (Sb  => M1_N.Sb,
              Vb  => M1_N.Vb,
              Dtm => M1_N.Tm - M1_N.Tb,
              Dtd => M1_N.Td - M1_N.Tb,
              Dtf => M1_N.Tf - M1_N.Tb,
              Se  => M1_N.Se,
              Ve  => M1_N.Ve,
              V   => M1_N.V,
              A   => M1_N.A);
    P(D2) := (Sb  => M2_N.Sb,
              Vb  => M2_N.Vb,
              Dtm => M2_N.Tm - M2_N.Tb,
              Dtd => M2_N.Td - M2_N.Tb,
              Dtf => M2_N.Tf - M2_N.Tb,
              Se  => M2_N.Se,
              Ve  => M2_N.Ve,
              V   => M2_N.V,
              A   => M2_N.A);
    Io.Update (P);
  end Update;


  procedure Update_Positions is
    use type Angle.Value;
    use type Value;
    Alignment_Offsets : constant Alignment.Offsets := Alignment.Synchronized_Offsets;
  begin
    Io.Update_Positions (Alignment_Offsets);
    M1.Sb := M1.Sb + Alignment_Offsets(D1);
    M2.Sb := M2.Sb + Alignment_Offsets(D2);
    if Log.Is_Enabled then
      Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
      Log.Write ("M1 Offset => " & Alignment_Offsets(D1)'img);
      Log.Write ("   New Sb => " & Angle.Image_Of (+M1.Sb));
      Log.Write ("M2 Offset => " & Alignment_Offsets(D2)'img);
      Log.Write ("   New Sb => " & Angle.Image_Of (+M2.Sb));
      Log.Write ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
    end if;
  end Update_Positions;


  procedure Synchronize_Positions is
  begin
    Io.Update_Positions (Alignment.Zero_Offset);
  end Synchronize_Positions;


  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed) is
    use type Angle.Signed;
  begin
    Io.Adjust (The_Drive, +With_Speed);
  end Adjust;


  procedure Stop is
  begin
    Io.Halt;
  end Stop;


  procedure Allow_Inversion is
  begin
    Inversion_Is_Enabled := True;
  end Allow_Inversion;


  function Time_When_At (The_Position : Position) return Time.Ut is
    Motors_Stopped : constant Speed := Resting;
  begin
    return Arriving_Time (The_Position, Motors_Stopped);
  end Time_When_At;


  function Arriving_Time (At_Position : Position;
                          With_Speed  : Speed) return Time.Ut is
    Tend : Time.Ut;
    use type Angle.Value;
  begin
    if Log.Is_Enabled then
      Log.Write ("arriving at - first  => " & Angle.Image_Of (At_Position.First, Decimals => 3));
      Log.Write ("            - second => " & Angle.Image_Of (At_Position.Second, Decimals => 3, Show_Signed => True));
      Log.Write ("speed       - first  => " & Angle.Image_Of (+With_Speed.First, Decimals => 3, Show_Signed => True) &
                                          " (" & With_Speed.First'img & ")");
      Log.Write ("            - second => " & Angle.Image_Of (+With_Speed.Second, Decimals => 3, Show_Signed => True) &
                                          " (" & With_Speed.Second'img & ")");
    end if;
    if not Assigned (At_Position, With_Speed) then
      return Time.In_The_Future;
    end if;
    if not Has_Arriving_Time (M1, P1) then
      return Time.In_The_Future;
    end if;
    if not Has_Arriving_Time (M2, P2) then
      return Time.In_The_Future;
    end if;
    Tend := Time.Synchronized_Universal_Of (Max(M1.Te, M2.Te), Time_Delta);
    if Log.Is_Enabled then
      Log.Write ("arriving time => " & Time.Image_Of (Tend));
    end if;
    M1.Te := Tend;
    M2.Te := M1.Te;
    return Tend;
  end Arriving_Time;


  function Approach_Target_At (The_Position : Position;
                               With_Speed   : Speed := Resting) return Boolean is
    use type Angle.Value;
  begin
    if Log.Is_Enabled then
      Log.Write ("approach - first  => " & Angle.Image_Of (The_Position.First, Decimals => 3));
      Log.Write ("         - second => " & Angle.Image_Of (The_Position.Second, Decimals => 3, Show_Signed => True));
      Log.Write ("speed    - first  => " & Angle.Image_Of (+With_Speed.First, Decimals => 3, Show_Signed => True) &
                                       " (" & With_Speed.First'img & ")");
      Log.Write ("         - second => " & Angle.Image_Of (+With_Speed.Second, Decimals => 3, Show_Signed => True) &
                                       " (" & With_Speed.Second'img & ")");
    end if;
    if not Assigned (The_Position, With_Speed) then
      Log.Warning ("target not within limits");
      return False;
    end if;
    if not Approaches (M1, P1) then
      Log.Warning ("M1 not reachable");
      return False;
    end if;
    Log_Values ("approach M1", M1);
    if not Approaches (M2, P2) then
      Log.Warning ("M2 not reachable");
      return False;
    end if;
    Log_Values ("approach M2", M2);
    return True;
  end Approach_Target_At;


  function Following (To          : Position;
                      Final_Speed : Speed;
                      At_Time     : Time.Ut) return Boolean is
    use type Angle.Value;
  begin
    if Log.Is_Enabled then
      Log.Write ("following - first  => " & Angle.Image_Of (To.First, Decimals => 3));
      Log.Write ("          - second => " & Angle.Image_Of (To.Second, Decimals => 3, Show_Signed => True));
      Log.Write ("  speed   - first  => " & Angle.Image_Of (+Final_Speed.First, Decimals => 3, Show_Signed => True) &
                                        " (" & Final_Speed.First'img & ")");
      Log.Write ("          - second => " & Angle.Image_Of (+Final_Speed.Second, Decimals => 3, Show_Signed => True) &
                                        " (" & Final_Speed.Second'img & ")");
      Log.Write ("  At Time          => " & Time.Image_Of (At_Time));
    end if;
    Inversion_Is_Enabled := False;
    if not Assigned (To, Final_Speed) then
      Log.Error ("target not within limits (check limit definitions)");
      return False;
    end if;
    M1.Te := At_Time;
    if Approaches (M1, P1) then
      Log_Values ("follow M1", M1);
      M2.Te := At_Time;
      if Approaches (M2, P2) then
        Log_Values ("follow M2", M2);
        return True;
      end if;
    end if;
    return False;
  end Following;


  function Synch_State return Device.Time_Synch_State is
  begin
    return Io.Actual_Synch_State;
  end Synch_State;


  function Positions return Position_Data is
    use type Angle.Value;
    use type Value;
    I : Information;
  begin
    if Io.Position_Is_Known then
      I := Io.Actual_Data;
    else
      return Position_Data'(others => <>);
    end if;
    if Is_Inverted then
      I.Positions(D1) := I.Positions(D1) + 180.0;
      I.Positions(D2) := 180.0 - I.Positions(D2);
    end if;
    return (Positions => (First      => +I.Positions(D1),
                          Second     => +I.Positions(D2),
                          Is_Defined => True),
            Offsets   => (First      => +I.Offsets(D1),
                          Second     => +I.Offsets(D2),
                          Is_Defined => True),
            Inverted  => Is_Inverted);
  end Positions;


  function Is_Inverse return Boolean is
  begin
    return Is_Inverted;
  end Is_Inverse;


  procedure Close_Communication is
  begin
    Log.Write ("close communication");
    Is_Closing := True;
    Io.Close;
  end Close_Communication;


  function Position_Of (First  : Angle.Value;
                        Second : Angle.Value) return Position is
  begin
    return (First      => First,
            Second     => Second,
            Is_Defined => True);
  end Position_Of;


  function Is_Defined (The_Position : Position) return Boolean is
  begin
    return The_Position.Is_Defined;
  end Is_Defined;


  function First_Of (The_Position : Position) return Angle.Value is
  begin
    return The_Position.First;
  end First_Of;


  function Second_Of (The_Position : Position) return Angle.Value is
  begin
    return The_Position.Second;
  end Second_Of;


  procedure Set_Undefined (The_Position : in out Position) is
  begin
    The_Position.Is_Defined := False;
  end Set_Undefined;


  function "-" (Left, Right : Position) return Position is
    use type Angle.Value;
  begin
    return (First      => Left.First - Right.First,
            Second     => Left.Second - Right.Second,
            Is_Defined => Left.Is_Defined and Right.Is_Defined);
  end "-";


  function "-" (Left, Right : Position) return Speed is
    use type Angle.Value;
  begin
    return (First  => Left.First - Right.First,
            Second => Left.Second - Right.Second);
  end "-";


  function Board_Temperature_Is_Known return Boolean is
  begin
    return Io.Actual_Board_Temperature /= Unknown_Board_Temperature;
  end Board_Temperature_Is_Known;


  function Board_Temperature return Celsius is
  begin
    return Io.Actual_Board_Temperature;
  end Board_Temperature;


  procedure Get (T       :        Time.Ut;
                 S       :    out Value;
                 V       :    out Value;
                 T_Begin : in out Time.Ut;
                 P       : in out Delta_Profile) is

    Dt : Time.Ut := T - T_Begin;

    use type Value;

  begin
    if Dt /= 0.0 then
      if P.Dtf /= 0.0 then
        if P.Dtd /= 0.0 then
          if P.Dtm /= 0.0 then
            if Dt < P.Dtm then
              V := P.Vb + (Value(Dt) * P.A);
              S := P.Sb + ((P.Vb + V) * Value(Dt) / 2.0);
              return;
            end if;
            -- set begin to accelleration end
            P.Sb := P.Sb + ((P.Vb + P.V) * Value(P.Dtm) / 2.0);
            P.Dtd := P.Dtd - P.Dtm;
            P.Dtf := P.Dtf - P.Dtm;
            T_Begin := T_Begin + P.Dtm;
            Dt := T - T_Begin;
            P.Dtm := 0.0;
            P.Vb := P.V;
          end if;
          if Dt < P.Dtd then
            V := P.V;
            S := P.Sb + (P.V * Value(Dt));
            return;
          end if;
          -- set begin to deaccelleration begin
          P.Sb := P.Sb + (P.V * Value(P.Dtd));
          P.Dtf := P.Dtf - P.Dtd;
          T_Begin := T_Begin + P.Dtd;
          Dt := T - T_Begin;
          P.Dtd := 0.0;
          P.Vb := P.V;
        end if;
        if Dt < P.Dtf then
          V := P.V - (Value(Dt) * P.A);
          S := P.Sb + ((P.V + V) * Value(Dt) / 2.0);
          return;
        end if;
        -- set begin to following begin
        P.Sb := P.Sb + ((P.V + P.Ve) * Value(P.Dtf) / 2.0);
        T_Begin := T_Begin + P.Dtf;
        Dt := T - T_Begin;
        P.Dtf := 0.0;
        P.Vb := P.Ve;
      end if;
      V := P.Ve;
      S := P.Sb + (P.Ve * Value(Dt));
      return;
    end if;
    V := P.Vb;
    S := P.Sb;
  end Get;

end Motor;
