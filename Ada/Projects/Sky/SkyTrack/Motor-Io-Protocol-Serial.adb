-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Ada.Unchecked_Conversion;
with Error;
with Parameter;
with Serial_Io;
with Unsigned;
with System;
with Traces;

package body Motor.Io.Protocol.Serial is

  package Log is new Traces ("Serial");

  use type Time.Ut;

  type Command is (Unused_Next_Action_1, Unused_Next_Action_2,
                   Set_Initial_Count_1, Set_Initial_Count_2,
                   Set_Position_1, Set_Position_2,
                   Update_Position_1, Update_Position_2,
                   Adjust_1, Adjust_2,
                   Get_Data, Unused_Get_Data,
                   Synchronize);

  type Channel is access Serial_Io.Channel;

  The_Serial_Port : Serial_Io.Port;
  The_Channel     : Channel;

  Startup_Id      : constant := 8#0111#;
  Positioned_Id   : constant := 8#0222#;
  Positioning_Id  : constant := 8#0333#;
  Update_Id       : constant := 8#0044#;
  New_Position_1  : constant := 8#0155#;
  New_Position_2  : constant := 8#0266#;
  Error_Id        : constant := 8#0077#;


  procedure Connect_Device is
    The_Event : Unsigned.Byte;
  begin
    Log.Write ("Device_Is_Available");
    The_Serial_Port := Parameter.Telescope_Connection.Port;
    The_Channel := new Serial_Io.Channel (The_Serial_Port);
    Serial_Io.Set (The_Baudrate => 19200, On => The_Channel.all);
    Serial_Io.Set (The_Timeout => 1.0, On => The_Channel.all);
    Serial_Io.Send (Command'pos(Get_Data), The_Channel.all);
    Serial_Io.Receive (The_Event, The_Channel.all);
    Serial_Io.Flush (The_Channel.all);
    case The_Event is
    when Startup_Id =>
      Log.Write ("Device => Startup");
    when Positioned_Id | Positioning_Id | Update_Id | New_Position_1 | New_Position_2 =>
      Log.Write ("Device => Started");
    when Error_Id =>
      Log.Write ("Device => Error");
    when others =>
      Error.Raise_With ("Unknown Device on " & The_Serial_Port'img);
    end case;
  exception
  when Serial_Io.Timeout =>
    Error.Raise_With ("Can't connect to the telescope on port " & The_Serial_Port'img);
  when others =>
    Error.Raise_With ("No access to " & The_Serial_Port'img);
  end Connect_Device;


  task type Control with Priority => System.Max_Priority is

    entry Start;

    entry Set_State (The_State : Device.State);

    entry Set_Step_Data (The_Data : Step_Information);

    entry Get_State (The_State : out Device.State);

    entry Get_Positions_Known (Positions_Known : out Boolean);

    entry Get_Step_Data (The_Data : out Step_Information);

    entry Send (Id : Command);

    entry Send (Item : Unsigned.Byte_String);

    entry Finalize;

  end Control;


  use all type Device.Drive;
  use all type Device.State;

  The_Control : access Control;

  task body Control is

    task Receiver with Priority => System.Max_Priority is

      entry Enable;

      entry Finish;

    end Receiver;

    package RT renames Ada.Real_Time;

    Is_Terminating : Boolean := False;

    task body Receiver is

      procedure Read (The_Position : out Integer;
                      The_Offset   : out Integer) is
        function Convert is new Ada.Unchecked_Conversion (Unsigned.Longword, Integer);
        The_Position_String : Unsigned.Byte_String (1..8);
      begin
        Serial_Io.Receive (The_Position_String, The_Channel.all);
        The_Position := Convert (Unsigned.Longword_Of_Big_Endian (The_Position_String(1..4)));
        The_Offset := Convert (Unsigned.Longword_Of_Big_Endian (The_Position_String(5..8)));
      end Read;

      The_Event : Unsigned.Byte;

      The_Data       : Step_Information;
      Has_Position_1 : Boolean := False;
      Has_Position_2 : Boolean := False;
      The_Error_Code : Unsigned.Byte := 0;

      procedure Disconnect is
      begin
        if not Is_Terminating then
          Has_Position_1 := False;
          Has_Position_2 := False;
          The_Control.Set_State (Disconnected);
        end if;
      end Disconnect;

      The_Count      : Integer := 0;
      The_Start_Time : RT.Time;

      use type RT.Time_Span;

    begin -- Receiver
      accept Enable;
      while not Is_Terminating loop
        begin
          Serial_Io.Receive (The_Event, The_Channel.all);
          case The_Event is
          when Startup_Id =>
            The_Control.Set_State (Startup);
            Has_Position_1 := False;
            Has_Position_2 := False;
          when Positioned_Id =>
            The_Count := 0;
            The_Control.Set_State (Stopped);
          when Positioning_Id =>
            The_Control.Set_State (Moving);
          when Update_Id =>
            The_Control.Set_State (Synchronised);
            if Log.Is_Enabled then
              if The_Count = 0 then
                The_Start_Time := RT.Clock;
              else
                Log.Write ("TIME_SPAN" & The_Count'img
                                       & " ->" & RT.To_Duration ((RT.Clock - The_Start_Time) / The_Count)'img);
              end if;
              The_Count := The_Count + 1;
            end if;
          when New_Position_1 =>
            Read (The_Data.Positions.M1, The_Data.Offsets.M1);
            Has_Position_1 := True;
            if Has_Position_2 then
              The_Control.Set_Step_Data (The_Data);
            end if;
          when New_Position_2 =>
            Read (The_Data.Positions.M2, The_Data.Offsets.M2);
            Has_Position_2 := True;
            if Has_Position_1 then
              The_Control.Set_Step_Data (The_Data);
            end if;
          when Error_Id =>
            Serial_Io.Receive (The_Error_Code, The_Channel.all);
            Log.Error ("error code =>" & The_Error_Code'img);
            The_Control.Set_State (Fault);
          when others =>
            Log.Error ("unknown event =>" & The_Event'img);
            Disconnect;
          end case;
        exception
        when Serial_Io.Aborted =>
          Disconnect;
        when Serial_Io.Timeout =>
          Disconnect;
          Log.Warning ("receiver timeout");
        when Occurrence: others =>
          Log.Termination (Occurrence);
          Disconnect;
        end;
      end loop;
      accept Finish;
    exception when Item: others =>
      Log.Termination (Item);
    end Receiver;

    The_Device_State     : Device.State := Disconnected;
    Is_Initialized       : Boolean := False;
    Has_New_State        : Boolean := False;
    Step_Positions_Known : Boolean := False;
    The_Step_Data        : Step_Information;
    The_Next_Send_Time   : RT.Time;

    Send_Interval : constant Duration := 0.2;

    use type RT.Time;

  begin -- Control
    accept Start;
    Receiver.Enable;
    The_Next_Send_Time := RT.Clock;
    loop
      select
        accept Set_State (The_State : Device.State) do
          The_Device_State := The_State;
          case The_State is
          when Disconnected =>
            Is_Initialized := False;
            Step_Positions_Known := False;
          when Startup =>
            if Is_Initialized then
              The_Device_State := Initialized;
            end if;
            Step_Positions_Known := False;
          when others =>
            null;
          end case;
          Has_New_State :=True;
        end Set_State;
      or
        accept Set_Step_Data (The_Data : Step_Information) do
          The_Step_Data := The_Data;
          Step_Positions_Known := True;
        end Set_Step_Data;
      or
        when Has_New_State => accept Get_State (The_State : out Device.State) do
          Has_New_State :=False;
          The_State := The_Device_State;
        end Get_State;
      or
        accept Get_Positions_Known (Positions_Known : out Boolean) do
          Positions_Known := Step_Positions_Known;
        end Get_Positions_Known;
      or
        accept Get_Step_Data (The_Data : out Step_Information) do
          The_Data := The_Step_Data;
        end Get_Step_Data;
      or
        accept Send (Item : Unsigned.Byte_String) do
          Serial_Io.Send (Item, The_Channel.all);
          The_Next_Send_Time := RT.Clock + RT.To_Time_Span(Send_Interval);
          if Natural(Item(Item'first)) = Command'pos(Set_Initial_Count_2) then
            Is_Initialized := True;
          end if;
        end Send;
      or
        accept Send (Id : Command) do
          Serial_Io.Send (Command'pos(Id), The_Channel.all);
          The_Next_Send_Time := RT.Clock + RT.To_Time_Span(Send_Interval);
        end Send;
      or
        accept Finalize;
        Is_Terminating := True;
        Serial_Io.Free (The_Serial_Port);
        exit;
      or
        when not Is_Terminating => delay until The_Next_Send_Time;
        Serial_Io.Send (Command'pos(Get_Data), The_Channel.all);
        The_Next_Send_Time := The_Next_Send_Time + RT.To_Time_Span(Send_Interval);
      end select;
    end loop;
    Receiver.Finish;
  exception when Item: others =>
    Log.Termination (Item);
  end Control;


  procedure Start is
  begin
    The_Control := new Control;
    The_Control.Start;
  end Start;


  function Actual_Stepper_State return Device.State is
    The_State : Device.State;
  begin
    The_Control.Get_State (The_State);
    return The_State;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Actual_Stepper_State;


  function Step_Position_Known return Boolean is
    Positions_Known : Boolean;
  begin
    The_Control.Get_Positions_Known (Positions_Known);
    return Positions_Known;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Step_Position_Known;


  function Actual_Step_Data return Step_Information is
    The_Data : Step_Information;
  begin
    The_Control.Get_Step_Data (The_Data);
    return The_Data;
  exception
  when Tasking_Error =>
    raise Driver_Closing;
  end Actual_Step_Data;


  Terminator : constant Unsigned.Byte := 16#1F#;
  Escape     : constant Unsigned.Byte := 16#F8#;
  Correction : constant Unsigned.Byte := 16#18#;

  The_Data  : Unsigned.Byte_String (1 .. 1024);
  The_Index : Natural := The_Data'first;


  procedure Append_Byte (Item : Unsigned.Byte) is
    The_Byte : Unsigned.Byte := Item;
    use type Unsigned.Byte;
  begin
    if (The_Byte = Terminator) or (The_Byte = Escape) then
      The_Data(The_Index) := Escape;
      The_Index := The_Index + 1;
      The_Byte := The_Byte - Correction;
    end if;
    The_Data(The_Index) := The_Byte;
    The_Index := The_Index + 1;
  end Append_Byte;


  type Four_Bytes is array (1..4) of Unsigned.Byte;

  function Convert is new Ada.Unchecked_Conversion (Integer, Four_Bytes);

  procedure Append (Item : Integer) is
    Bytes : constant Four_Bytes := Convert(Item);
  begin
    Append_Byte (Bytes(4));
    Append_Byte (Bytes(3));
    Append_Byte (Bytes(2));
    Append_Byte (Bytes(1));
  end Append;


  procedure Send (Id   : Command;
                  Item : Integer) is
  begin
    The_Data(The_Data'first) := Command'pos(Id);
    The_Index := The_Data'first + 1;
    Append (Item);
    The_Data(The_Index) := Terminator;
    The_Control.Send (The_Data(The_Data'first .. The_Index));
  end Send;


  procedure Send (Id : Command) is
  begin
    The_Control.Send (Id);
  end Send;


  procedure Set_Initial_Count (C0_1 : Natural;
                               C0_2 : Natural) is
  begin
    Send (Set_Initial_Count_1, C0_1);
    Send (Set_Initial_Count_2, C0_2);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Set_Initial_Count;


  procedure Set_Step_Positions (P : Step_Positions) is
  begin
    Send (Set_Position_1, P.M1);
    Send (Set_Position_2, P.M2);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Set_Step_Positions;


  procedure Update_Step_Offsets (Offsets : Step_Positions) is
  begin
    null; -- !!! not implemented
  end Update_Step_Offsets;


  procedure Update_Step_Positions (Offsets : Step_Positions)  is
  begin
    Send (Update_Position_1, Offsets.M1);
    Send (Update_Position_2, Offsets.M2);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Update_Step_Positions;


  procedure Synchronize_Start_Time (The_Time : out Time.Ut) is
  begin
    The_Time := Time.Synchronized_Universal (Base => Time_Delta);
    delay (abs Duration(The_Time - Time.Universal));
    Send (Synchronize);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Synchronize_Start_Time;


  procedure Transfer_Actions (M1 : Action_List := No_Actions;
                              M2 : Action_List := No_Actions) is

    procedure Send (A  : Action;
                    To : Device.Drive) is
    begin
      if A.S = 0 and A.N = 0 and A.K = Keep_Speed then
        Log.Error ("transfer actions => S = N = 0");
        return;
      end if;
      The_Index := The_Data'first;
      Append_Byte (Device.Drive'pos(To));
      Append_Byte (Acceleration_Kind'pos(A.K));
      Append_Byte (Step_Direction'pos(A.D));
      Append (A.S);
      Append (A.C);
      Append (A.N);
      The_Data(The_Index) := Terminator;
      The_Control.Send (The_Data(The_Data'first .. The_Index));
    end Send;

    M1_Index : Natural := M1'first;
    M2_Index : Natural := M2'first;

  begin -- Transfer
    The_Index := The_Data'first;
    loop
      if M1_Index <= M1'last then
        Send (M1(M1_Index), D1);
        M1_Index := M1_Index + 1;
      else
        exit when M2_Index > M2'last;
      end if;
      if M2_Index <= M2'last then
        Send (M2(M2_Index), D2);
        M2_Index := M2_Index + 1;
      else
        exit when M1_Index > M1'last;
      end if;
    end loop;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Transfer_Actions;


  procedure Adjust (The_Drive         : Device.Drive;
                    Offset_Per_Action : Step_Count) is
  begin
    case The_Drive is
    when Device.D1 =>
      Send (Adjust_1, Offset_Per_Action);
    when Device.D2 =>
      Send (Adjust_2, Offset_Per_Action);
    end case;
  end Adjust;


  procedure Stop_All is
  begin
    The_Control.Send ([Terminator]);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Stop_All;


  procedure Finish is
  begin
    The_Control.Finalize;
  end Finish;

end Motor.Io.Protocol.Serial;
