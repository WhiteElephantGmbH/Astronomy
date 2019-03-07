-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Error;
with Network.Udp;
with Parameter;
with Program;
with Unsigned;
with Strings;
with System;
with User.Input;
with Traces;

package body Motor.Io.Protocol.Udp is

  package Log is new Traces ("Udp");

  type Command is (Initialize,
                   Get_Data,
                   Define_Directions,
                   Synchronize,
                   Update,
                   Update_Directions,
                   Adjust_1,
                   Adjust_2,
                   Set_Autoguiding,
                   Stop);

  type Protocol_Version is record
    Major_Id : Unsigned.Byte := Program.Major_Id;
    Minor_Id : Unsigned.Byte := Program.Minor_Id;
    Revision : Unsigned.Word := Program.Revision;
  end record;

  type Protocol_Type is new Unsigned.Byte;

  Stepper_Motor_Protocol : constant Protocol_Type := 1;

  type Clocks is new Unsigned.Quadword;

  type Longword is new Unsigned.Longword;

  type Word is new Unsigned.Word;

  type Sentinel is new Longword;

  Start_Sentinel : constant Sentinel := 16#42424242#;

  One_Second : constant := 1000000; -- in microseconds

  subtype Micro_Seconds is Longword;

  subtype Deci_Kelvin is Word;

  Unknown_Temperature : constant Deci_Kelvin := Deci_Kelvin'first;

  type Telescope_Identifier is new Unsigned.Byte;

  type Autoguiding_Parameters is record
    Steps_Per_Revolution_1 : Device.Step_Number;
    Steps_Per_Revolution_2 : Device.Step_Number;
    Rate                   : Device.Autoguiding_Rate;
  end record
  with
    Alignment => 4,
    Size      => 64;
  for Autoguiding_Parameters use record
    Rate                   at  0 range 0  .. 7;
    Steps_Per_Revolution_1 at  1 range 0  .. 27;
    Steps_Per_Revolution_2 at  1 range 28 .. 55;
  end record;

  type Transmit_Data (The_Command       : Command := Initialize;
                      The_Protocol_Type : Protocol_Type := Stepper_Motor_Protocol;
                      Actions_Length_1  : Action_List_Length := 0;
                      Actions_Length_2  : Action_List_Length := 0) is record
    The_Sentinel     : Sentinel := Start_Sentinel;
    The_Version      : Protocol_Version;
    Steps_Per_Action : Step_Count := 0;
    case The_Command is
    when Initialize =>
      Start_Count_1 : Natural;
      Start_Count_2 : Natural;
    when Define_Directions =>
      Positions : Step_Positions;
    when Update_Directions =>
      Offsets : Step_Positions;
    when Set_Autoguiding =>
      Parameters : Autoguiding_Parameters;
    when Synchronize =>
      Actual_Time : Time.Ut;
    when Update =>
      Action_List_1 : Action_List(1..Actions_Length_1);
      Action_List_2 : Action_List(1..Actions_Length_2);
    when Get_Data | Adjust_1 | Adjust_2 | Stop =>
      null;
    end case;
  end record
  with
    Alignment => 4;
  for Transmit_Data use record
    The_Sentinel      at  0 range 0..31;
    The_Version       at  4 range 0..31;
    The_Protocol_Type at  8 range 0..7;
    The_Command       at  9 range 0..7;
    Actions_Length_1  at 10 range 0..7;
    Actions_Length_2  at 11 range 0..7;
    Steps_Per_Action  at 12 range 0..31;
    Start_Count_1     at 16 range 0..31;
    Start_Count_2     at 20 range 0..31;
    Positions         at 16 range 0..63;
    Offsets           at 16 range 0..63;
    Parameters        at 16 range 0..63;
    Actual_Time       at 16 range 0..63;
  end record;

  use all type Device.State;

  use all type Device.Time_Synch_State;

  type Error_State is (No_Error, No_Action_Provided, No_Increment_In_Time, Division_By_Zero, Undefined_Direction);

  type Telescope_Data is record
    The_Sentinel       : Sentinel;
    The_Version        : Protocol_Version;
    The_Protocol_Type  : Protocol_Type := Stepper_Motor_Protocol;
    The_Device_State   : Device.State := Startup;
    The_Error_State    : Error_State := No_Error;
    Telescope_Id       : Telescope_Identifier := 0;
    Temperature        : Deci_Kelvin;
    Left               : Boolean;
    Right              : Boolean;
    Up                 : Boolean;
    Down               : Boolean;
    The_Synch_State    : Device.Time_Synch_State;
    Clocks_Per_Minute  : Clocks;
    The_Time           : Time.Ut;
    Actual_Data        : Step_Information;
    Next_Maximum_Delay : Micro_Seconds;
    Next_Sentinel      : Sentinel;
  end record
  with
    Alignment => 8,
    Size      => 7 * 64;
  for Telescope_Data use record
    The_Sentinel       at  0 range 0..31;
    The_Version        at  4 range 0..31;
    The_Protocol_Type  at  8 range 0..7;
    The_Device_State   at  9 range 0..7;
    The_Error_State    at 10 range 0..7;
    Telescope_Id       at 11 range 0..7;
    Temperature        at 12 range 0..15;
    Left               at 14 range 0..0;
    Right              at 14 range 1..1;
    Up                 at 14 range 2..2;
    Down               at 14 range 3..3;
    The_Synch_State    at 15 range 0..7;
    Clocks_Per_Minute  at 16 range 0..63;
    The_Time           at 24 range 0..63;
    Actual_Data        at 32 range 0..127;
    Next_Maximum_Delay at 48 range 0..31;
    Next_Sentinel      at 52 range 0..31;
  end record;


  procedure Send is new Network.Udp.Send_Datagram_To  (Transmit_Data);

  function Datagram_From is new Network.Udp.Datagram_From  (Telescope_Data);

  task type Control with Priority => System.Max_Priority is

    entry Start;

    entry Transmit (Data : Transmit_Data);

    entry Get_State (The_State : out Device.State);

    entry Get_Positions_Known (Positions_Known : out Boolean);

    entry Get_Step_Data (The_Data : out Step_Information);

    entry Get_Temperature (The_Value : out Deci_Kelvin);

    entry Get_Start_Time (The_Time : out Time.Ut);

    entry Get_Synch_State (The_State : out Device.Time_Synch_State);

    entry Finalize;

  end Control;


  function Hardware_Version_Of (The_Version : Protocol_Version) return Hardware_Version is
    use type Unsigned.Byte;
  begin
    if The_Version.Major_Id = 2 then
      case The_Version.Minor_Id is
      when 3 =>
        return Stepper_Version_0;
      when 4 =>
        return Stepper_Version_1;
      when others =>
        null;
      end case;
    end if;
    return Unknown;
  end Hardware_Version_Of;

  The_Hardware_Version : Hardware_Version := Undefined;


  task body Control is

    The_Connection : Parameter.Connection;

    The_Transmit_Data     : Transmit_Data;
    Has_New_Transmit_Data : Boolean := False;

    The_Update_Direction_Data : Transmit_Data;
    Has_Update_Direction_Data : Boolean := False;

    The_Guiding_Data   : Transmit_Data;
    Has_Guiding_Data   : Boolean := False;
    Is_Autoguiding     : Boolean := False;

    The_Actual_State : Device.State := Disconnected;
    Has_New_State    : Boolean := False;

    The_Actual_Command : Device.Command := Device.No_Command;

    The_Actual_Data     : Step_Information := (others => <>);
    Positions_Are_Known : Boolean := False;

    The_Start_Time     : Time.Ut := 0.0;
    The_Update_Time    : Time.Ut;
    Has_New_Start_Time : Boolean := False;

    The_Time_Synch_State : Device.Time_Synch_State := Idle;

    The_Temperature : Deci_Kelvin := Unknown_Temperature;

    The_Telescope_Data   : Telescope_Data;
    Latest_Transmit_Time : Ada.Real_Time.Time;

    The_Expected_Sentinel : Sentinel := Start_Sentinel;

    use type Ada.Real_Time.Time_Span;

    Maximum_Timeout : constant Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (Time_Delta);

    Default_Delay_Time : constant Duration := Parameter.Datagram_Timeout;
    The_Delay_Time     : Duration := Default_Delay_Time;

    The_Error_State : Error_State := No_Error;

    procedure Set_Transmit_Data (Data : Transmit_Data) is
    begin
      The_Transmit_Data := Data;
    end Set_Transmit_Data;

    procedure Disconnect is
    begin
      Positions_Are_Known := False;
      The_Temperature := Unknown_Temperature;
      The_Actual_State := Disconnected;
      Has_New_State := True;
      The_Expected_Sentinel := Start_Sentinel;
    end Disconnect;

    use type Ada.Real_Time.Time;

  begin -- Control
    accept Start;
    The_Connection := Parameter.Telescope_Connection;
    loop
      select
        accept Transmit (Data : Transmit_Data) do
          case Data.The_Command is
          when Update_Directions =>
            The_Update_Direction_Data := Data;
            Has_Update_Direction_Data := True;
          when Set_Autoguiding =>
            The_Guiding_Data := Data;
            Has_Guiding_Data := True;
            Is_Autoguiding := Data.Parameters.Rate /= 0;
          when Adjust_1 | Adjust_2 =>
            if not Has_Guiding_Data then
              The_Guiding_Data := Data;
              Has_Guiding_Data := True;
            end if;
          when others =>
            Set_Transmit_Data (Data);
            Has_New_Transmit_Data := True;
          end case;
          The_Delay_Time := 0.0;
        end Transmit;
      or
        when Has_New_State => accept Get_State (The_State : out Device.State) do
          The_State := The_Actual_State;
          Has_New_State := False;
        end Get_State;
      or
        accept Get_Positions_Known (Positions_Known : out Boolean) do
          Positions_Known := Positions_Are_Known;
        end Get_Positions_Known;
      or
        accept Get_Step_Data (The_Data : out Step_Information) do
          The_Data := The_Actual_Data;
        end Get_Step_Data;
      or
        accept Get_Temperature (The_Value : out Deci_Kelvin) do
          The_Value := The_Temperature;
        end Get_Temperature;
      or
        when Has_New_Start_Time => accept Get_Start_Time (The_Time : out Time.Ut) do
          The_Time := The_Start_Time;
          Has_New_Start_Time := False;
        end Get_Start_Time;
      or
        accept Get_Synch_State (The_State : out Device.Time_Synch_State) do
          The_State := The_Time_Synch_State;
        end Get_Synch_State;
      or
        accept Finalize;
        exit;
      or
        delay The_Delay_Time;
        The_Delay_Time := Default_Delay_Time;
        if Has_New_Transmit_Data then
          Has_New_Transmit_Data := False;
        elsif Has_Update_Direction_Data then
          Has_Update_Direction_Data := False;
          The_Transmit_Data := The_Update_Direction_Data;
        elsif Has_Guiding_Data then
          Has_Guiding_Data := False;
          The_Transmit_Data := The_Guiding_Data;
        elsif The_Time_Synch_State = Ready then
          Set_Transmit_Data ((The_Command => Synchronize,
                              Actual_Time => Time.Nearest_Universal (Base => Time.One_Minute),
                              others      => <>));
        else
          Set_Transmit_Data ((The_Command => Get_Data, others => <>));
        end if;
        The_Transmit_Data.The_Sentinel := The_Expected_Sentinel;
        Latest_Transmit_Time := Ada.Real_Time.Clock + Maximum_Timeout;
        Transmitter: loop
          Log.Write ("transmit " & The_Transmit_Data.The_Command'img &
                     " - sentinel => " & Unsigned.Hex_Image_Of (Unsigned.Longword(The_Transmit_Data.The_Sentinel)));
          begin
            Send (The_Transmit_Data, The_Address => The_Connection.Address, Used_Socket => The_Connection.Socket);
          exception
          when others =>
            null;
          end;
          begin
            The_Telescope_Data := Datagram_From (Used_Socket => The_Connection.Socket);
            if Log.Is_Enabled then
              Log.Write ("received: actual state => " & The_Telescope_Data.The_Device_State'img &
                         "; sentinel => " & Unsigned.Hex_Image_Of(Unsigned.Longword(The_Telescope_Data.Next_Sentinel)));
              Log.Write ("          position 1 =>" & The_Telescope_Data.Actual_Data.Positions.M1'img &
                         "; position 2 =>" & The_Telescope_Data.Actual_Data.Positions.M2'img &
                         "; offset 1 =>" & The_Telescope_Data.Actual_Data.Offsets.M1'img &
                         "; offset 2 =>" & The_Telescope_Data.Actual_Data.Offsets.M2'img);
              Log.Write ("          left => " & The_Telescope_Data.Left'img &
                         "; right => " & The_Telescope_Data.Right'img &
                         "; up => " & The_Telescope_Data.Up'img &
                         "; down => " & The_Telescope_Data.Down'img);
              Log.Write ("          synch => " & The_Telescope_Data.The_Synch_State'img &
                         "; Clocks =>" & The_Telescope_Data.Clocks_Per_Minute'img &
                         "; time =>" & The_Telescope_Data.The_Time'img &
                         "; temperature =>" & The_Telescope_Data.Temperature'img);
            end if;
            if The_Telescope_Data.The_Error_State /= The_Error_State then
              The_Error_State := The_Telescope_Data.The_Error_State;
              Log.Error (The_Telescope_Data.The_Error_State'img);
            end if;
            if The_Telescope_Data.The_Sentinel = The_Expected_Sentinel and then
              Hardware_Version_Of (The_Telescope_Data.The_Version) = The_Hardware_Version
            then
              The_Expected_Sentinel := The_Telescope_Data.Next_Sentinel;
              declare
                Maximum_Delay_Time : constant Duration
                  := Duration(The_Telescope_Data.Next_Maximum_Delay) / Duration(One_Second);
              begin
                if The_Delay_Time > Maximum_Delay_Time then
                  The_Delay_Time := Maximum_Delay_Time;
                end if;
              end;
              if The_Actual_State /= The_Telescope_Data.The_Device_State then
                Has_New_State := True;
                The_Actual_State := The_Telescope_Data.The_Device_State;
                Log.Write ("$$$ NEW STATE:" & The_Actual_State'img);
                if The_Actual_State = Synchronised then
                  The_Update_Time := The_Telescope_Data.The_Time;
                  The_Start_Time := The_Update_Time - Time_Delta;
                  Log.Write ("$$$ START TIME:" & Time.Image_Of (The_Start_Time));
                  Has_New_Start_Time := True;
                end if;
              else
                if The_Actual_State = Synchronised then
                  if The_Update_Time <= The_Telescope_Data.The_Time then
                    Log.Write ("$$$ UPDATE TIME:" & Time.Image_Of (The_Update_Time));
                    The_Update_Time := The_Update_Time + Time_Delta;
                    Has_New_State := True;
                  end if;
                end if;
              end if;
              case The_Actual_State is
              when Disconnected | Startup | Initialized =>
                Positions_Are_Known := False;
                The_Actual_Command := Device.No_Command;
              when Moving | Stopped | Fault =>
                if The_Actual_Data /= The_Telescope_Data.Actual_Data then
                  The_Actual_Data := The_Telescope_Data.Actual_Data;
                  Has_New_State := True;
                end if;
                Positions_Are_Known := True;
              when Synchronised =>
                The_Actual_Data := The_Telescope_Data.Actual_Data;
                Positions_Are_Known := True;
              end case;
              The_Time_Synch_State := The_Telescope_Data.The_Synch_State;
              if not Is_Autoguiding and then Positions_Are_Known then
                declare
                  The_Command : Device.Command;
                  use all type Device.Command;
                begin
                  if The_Actual_Command = No_Command then
                    if (not The_Telescope_Data.Right) and
                       (    The_Telescope_Data.Left) and
                       (    The_Telescope_Data.Up) and
                       (not The_Telescope_Data.Down)
                    then
                      The_Command := Increase;
                    elsif (not The_Telescope_Data.Right) and
                          (    The_Telescope_Data.Left) and
                          (not The_Telescope_Data.Up) and
                          (    The_Telescope_Data.Down)
                    then
                      The_Command := Decrease;
                    elsif (    The_Telescope_Data.Right) and
                          (not The_Telescope_Data.Left) and
                          (    The_Telescope_Data.Up) and
                          (not The_Telescope_Data.Down)
                    then
                      The_Command := Stop;
                    elsif (    The_Telescope_Data.Right) and
                          (not The_Telescope_Data.Left) and
                          (not The_Telescope_Data.Up) and
                          (    The_Telescope_Data.Down)
                    then
                      The_Command := Enter;
                    elsif (not The_Telescope_Data.Right) and
                          (    The_Telescope_Data.Left) and
                          (not The_Telescope_Data.Up) and
                          (not The_Telescope_Data.Down)
                    then
                      The_Command := Move_Left;
                    elsif (    The_Telescope_Data.Right) and
                          (not The_Telescope_Data.Left) and
                          (not The_Telescope_Data.Up) and
                          (not The_Telescope_Data.Down)
                    then
                      The_Command := Move_Right;
                    elsif (not The_Telescope_Data.Right) and
                          (not The_Telescope_Data.Left) and
                          (    The_Telescope_Data.Up) and
                          (not The_Telescope_Data.Down)
                    then
                      The_Command := Move_Up;
                    elsif (not The_Telescope_Data.Right) and
                          (not The_Telescope_Data.Left) and
                          (not The_Telescope_Data.Up) and
                          (    The_Telescope_Data.Down)
                    then
                      The_Command := Move_Down;
                    else
                      The_Command := The_Actual_Command;
                    end if;
                  else
                    if (not The_Telescope_Data.Right) and
                       (not The_Telescope_Data.Left) and
                       (not The_Telescope_Data.Up) and
                       (not The_Telescope_Data.Down)
                    then
                      The_Command := No_Command;
                    else
                      The_Command := The_Actual_Command;
                    end if;
                  end if;
                  if The_Actual_Command /= The_Command then
                    The_Actual_Command := The_Command;
                    User.Input.Put (The_Actual_Command, From => User.Input.Handbox);
                  end if;
                end;
              end if;
              The_Temperature := The_Telescope_Data.Temperature;
              exit Transmitter;
            end if;
          exception
          when Network.Timeout =>
            Log.Write ("$$$ RECEIVE TIMEOUT");
          when Network.Receive_Error =>
            Log.Write ("$$$ RECEIVE ERROR");
            Disconnect;
            exit Transmitter;
          end;
          if Ada.Real_Time.Clock > Latest_Transmit_Time then
            Disconnect;
            exit Transmitter;
          end if;
        end loop Transmitter;
      end select;
    end loop;
    Network.Udp.Close (The_Connection.Socket);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Control;

  The_Control : access Control;


  function Image_Of (The_Version : Protocol_Version) return String is
  begin
    return Strings.Trimmed (The_Version.Major_Id'img) & '.' &
           Strings.Trimmed (The_Version.Minor_Id'img) & '.' &
           Strings.Trimmed (The_Version.Revision'img);
  end Image_Of;


  procedure Connect_Device is
    Connection : constant Parameter.Connection := Parameter.Telescope_Connection;
  begin
    begin
      Send (Transmit_Data'(The_Command  => Get_Data,
                           The_Sentinel => Start_Sentinel,
                           others => <>),
            The_Address => Connection.Address,
            Used_Socket => Connection.Socket);
      declare
        The_Telescope_Data : constant Telescope_Data := Datagram_From (Used_Socket => Connection.Socket);
      begin
        if The_Telescope_Data.The_Sentinel = Start_Sentinel then
          Log.Write ("Connected SkyTracker protocol version " & Image_Of (The_Telescope_Data.The_Version));
          The_Hardware_Version := Hardware_Version_Of (The_Telescope_Data.The_Version);
          case The_Hardware_Version is
          when Stepper_Version_0 | Stepper_Version_1 =>
            return;
          when others =>
            Error.Raise_With ("firmware update required in stepper hardware.");
          end case;
        end if;
      end;
    exception
    when others =>
      null;
    end;
    Error.Raise_With ("Can't connect the telescope");
  end Connect_Device;


  function Stepper_Version return Hardware_Version is
  begin
    if The_Hardware_Version = Undefined then
      raise Program_Error;
    end if;
    return The_Hardware_Version;
  end Stepper_Version;


  procedure Start is
  begin
    Log.Write ("start");
    The_Control := new Control;
    The_Control.Start;
  end Start;


  procedure Transmit (Data : Transmit_Data) is
  begin
    The_Control.Transmit (Data);
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Transmit;


  The_Autoguiding_Rate : Device.Autoguiding_Rate;

  procedure Define_Autoguiding is
  begin
    Log.Write ("define autoguiding - rate =>" & The_Autoguiding_Rate'img & "%");
    Transmit ((The_Command => Set_Autoguiding,
               Parameters  => (Rate                   => The_Autoguiding_Rate,
                               Steps_Per_Revolution_1 => Nspr_Of (Device.D1),
                               Steps_Per_Revolution_2 => Nspr_Of (Device.D2)),
               others      => <>));
  end Define_Autoguiding;


  procedure Set_Initial_Count (C0_1 : Natural;
                               C0_2 : Natural) is
  begin
    Log.Write ("set initial count: C0_1 =>" & C0_1'img & "; C0_2 =>" & C0_2'img);
    Transmit ((The_Command      => Initialize,
               Start_Count_1    => C0_1,
               Start_Count_2    => C0_2,
               others           => <>));
    Define_Autoguiding;
  end Set_Initial_Count;


  procedure Set_Autoguiding_Rate (The_Rate : Device.Autoguiding_Rate) is
  begin
    The_Autoguiding_Rate := The_Rate;
  end Set_Autoguiding_Rate;


  function Actual_Stepper_State return Device.State is
    The_State : Device.State;
  begin
    The_Control.Get_State (The_State);
    Log.Write ("actual stepper state => " & The_State'img);
    return The_State;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Actual_Stepper_State;


  function Time_Synch_State return Device.Time_Synch_State is
    The_State : Device.Time_Synch_State;
  begin
    The_Control.Get_Synch_State (The_State);
    return The_State;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Time_Synch_State;


  function Step_Position_Known return Boolean is
    Has_Positions : Boolean;
  begin
    The_Control.Get_Positions_Known (Has_Positions);
    Log.Write ("step position known => " & Has_Positions'img);
    return Has_Positions;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Step_Position_Known;


  function Actual_Step_Data return Step_Information is
    The_Step_Data : Step_Information;
  begin
    Log.Write ("actual step data");
    The_Control.Get_Step_Data (The_Step_Data);
    return The_Step_Data;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Actual_Step_Data;


  function Actual_Temperature return Celsius is
    The_Value : Deci_Kelvin;
  begin
    The_Control.Get_Temperature (The_Value);
    return Celsius(Float(The_Value) / 10.0 + Float(Celsius'first));
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Actual_Temperature;


  procedure Set_Step_Positions (The_Positions : Step_Positions) is
  begin
    Log.Write ("set step position: P1 =>" & The_Positions.M1'img & "; P2 =>" & The_Positions.M2'img);
    Transmit ((The_Command => Define_Directions,
               Positions   => The_Positions,
               others      => <>));
  end Set_Step_Positions;


  procedure Update_Step_Positions (Offsets : Step_Positions) is
  begin
    Log.Write ("update step position: O1 =>" & Offsets.M1'img & "; O2 =>" & Offsets.M2'img);
    Transmit ((The_Command => Update_Directions,
               Offsets     => Offsets,
               others      => <>));
  end Update_Step_Positions;


  procedure Synchronize_Start_Time (The_Time : out Time.Ut) is
    The_Actual_Time : Time.Ut;
  begin
    Log.Write ("synchronize time");
    The_Actual_Time := Time.Universal;
    Log.Write ("  - actual time => " & Time.Image_Of (The_Actual_Time));
    Transmit ((The_Command => Synchronize,
               Actual_Time  => The_Actual_Time,
               others      => <>));
    The_Control.Get_Start_Time (The_Time);
    Log.Write ("  - start time => " & Time.Image_Of (The_Time));
  end Synchronize_Start_Time;


  procedure Transfer_Actions (M1 : Action_List := No_Actions;
                              M2 : Action_List := No_Actions) is
  begin
    Log.Write ("transfer actions");
    Transmit ((The_Command      => Update,
               Actions_Length_1 => M1'length,
               Actions_Length_2 => M2'length,
               Action_List_1    => M1,
               Action_List_2    => M2,
               others           => <>));
  end Transfer_Actions;


  procedure Adjust (The_Drive         : Device.Drive;
                    Offset_Per_Action : Step_Count) is
  begin
    Log.Write ("adjust " & The_Drive'img & " - Offset:" & Offset_Per_Action'img);
    case The_Drive is
    when Device.D1 =>
      Transmit ((The_Command      => Adjust_1,
                 Steps_Per_Action => Offset_Per_Action,
                 others           => <>));
    when Device.D2 =>
      Transmit ((The_Command      => Adjust_2,
                 Steps_Per_Action => Offset_Per_Action,
                 others           => <>));
    end case;
  end Adjust;


  procedure Stop_All is
  begin
    Log.Write ("stop all");
    Transmit ((The_Command => Stop,
               others      => <>));
    Define_Autoguiding;
  end Stop_All;


  procedure Finish is
  begin
    Log.Write ("finish");
    The_Control.Finalize;
  end Finish;

end Motor.Io.Protocol.Udp;
