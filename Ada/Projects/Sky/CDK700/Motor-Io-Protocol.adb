-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Motor.Io.Protocol.Serial;
with Motor.Io.Protocol.Simulation;
with Motor.Io.Protocol.Udp;
with Parameter;
with Traces;

package body Motor.Io.Protocol is

  package Log is new Traces ("Protocol");

  use all type Device.Drive;
  use all type Parameter.Connection_Kind;

  The_Connection_Kind : Parameter.Connection_Kind;

  procedure Do_Connect is
  begin
    The_Connection_Kind := Parameter.Telescope_Connection.Kind;
    case The_Connection_Kind is
    when Is_Simulated =>
      null;
    when Is_Udp =>
      Udp.Connect_Device;
    when Is_Serial =>
      Serial.Connect_Device;
    end case;
  end Do_Connect;


  function Connected_Device_Version return Hardware_Version is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      return Unknown;
    when Is_Simulated =>
      return Unknown;
    when Is_Udp =>
      return Udp.Stepper_Version;
    end case;
  end Connected_Device_Version;


  procedure Do_Open_Communication is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Start;
    when Is_Simulated =>
      Simulation.Start;
    when Is_Udp =>
      Udp.Start;
    end case;
  end Do_Open_Communication;


  procedure Initialize (C0_1 : Natural;
                        C0_2 : Natural) is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Set_Initial_Count (C0_1, C0_2);
    when Is_Simulated =>
      Simulation.Set_Initial_Count (C0_1, C0_2);
    when Is_Udp =>
      Udp.Set_Initial_Count (C0_1, C0_2);
    end case;
  end Initialize;


  procedure Do_Set_Autoguiding_Rate (The_Rate : Device.Autoguiding_Rate) is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      null;
    when Is_Simulated =>
      null;
    when Is_Udp =>
      Udp.Set_Autoguiding_Rate (The_Rate);
    end case;
  end Do_Set_Autoguiding_Rate;


  function Actual_Device_State return Device.State is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      return Serial.Actual_Stepper_State;
    when Is_Simulated =>
      return Simulation.Actual_Stepper_State;
    when Is_Udp =>
      return Udp.Actual_Stepper_State;
    end case;
  end Actual_Device_State;


  function Actual_Device_Synch_State return Device.Time_Synch_State is
  begin
    case The_Connection_Kind is
    when Is_Serial | Is_Simulated =>
      return Device.Idle;
    when Is_Udp =>
      return Udp.Time_Synch_State;
    end case;
  end Actual_Device_Synch_State;


  procedure Define_Positions (The_Positions : Step_Positions) is
  begin
    case The_Connection_Kind is
    when Is_Simulated =>
      Simulation.Set_Step_Positions (The_Positions);
    when Is_Serial =>
      Serial.Set_Step_Positions (The_Positions);
    when Is_Udp =>
      Udp.Set_Step_Positions (The_Positions);
    end case;
  end Define_Positions;


  procedure Update_Positions (Offsets : Step_Positions) is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Update_Step_Positions (Offsets);
    when Is_Simulated =>
      Simulation.Update_Step_Positions (Offsets);
    when Is_Udp =>
      Udp.Update_Step_Positions (Offsets);
    end case;
  end Update_Positions;


  function Position_Known return Boolean is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      return Serial.Step_Position_Known;
    when Is_Simulated =>
      return Simulation.Step_Position_Known;
    when Is_Udp =>
      return Udp.Step_Position_Known;
    end case;
  end Position_Known;


  function Stepper_Data return Step_Information is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      return Serial.Actual_Step_Data;
    when Is_Simulated =>
      return Simulation.Actual_Step_Data;
    when Is_Udp =>
      return Udp.Actual_Step_Data;
    end case;
  end Stepper_Data;


  function Hardware_Board_Temperature return Celsius is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      return Unknown_Board_Temperature;
    when Is_Simulated =>
      return Unknown_Board_Temperature;
    when Is_Udp =>
      return Udp.Actual_Temperature;
    end case;
  end Hardware_Board_Temperature;


  procedure Do_Synchronize_Time (The_Time : out Time.Ut) is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Synchronize_Start_Time (The_Time);
    when Is_Simulated =>
      Simulation.Synchronize_Start_Time (The_Time);
    when Is_Udp =>
      Udp.Synchronize_Start_Time (The_Time);
    end case;
  end Do_Synchronize_Time;


  procedure Log_Action (A         : Action;
                        The_Motor : Device.Drive) is
  begin
    Log.Write ("====================");
    Log.Write ("++ " & The_Motor'img);
    Log.Write ("--------------------");
    Log.Write ("++ K => " & A.K'img);
    Log.Write ("++ D => " & A.D'img);
    Log.Write ("++ C =>" & A.C'img);
    Log.Write ("++ S =>" & A.S'img);
    Log.Write ("++ N =>" & A.N'img);
    Log.Write ("====================");
  end Log_Action;


  procedure Transfer (M1 : Action_List := No_Actions;
                      M2 : Action_List := No_Actions) is
  begin
    if Log.Is_Enabled then
      for A of M1 loop
        Log_Action (A, D1);
      end loop;
      for A of M2 loop
        Log_Action (A, D2);
      end loop;
    end if;
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Transfer_Actions (M1, M2);
    when Is_Simulated =>
      Simulation.Transfer_Actions (M1, M2);
    when Is_Udp =>
      Udp.Transfer_Actions (M1, M2);
    end case;
  end Transfer;


  procedure Do_Adjust (The_Drive        : Device.Drive;
                       Steps_Per_Update : Step_Count) is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Adjust (The_Drive, Steps_Per_Update);
    when Is_Simulated =>
      Simulation.Adjust (The_Drive, Steps_Per_Update);
    when Is_Udp =>
      Udp.Adjust (The_Drive, Steps_Per_Update);
    end case;
  end Do_Adjust;


  procedure Do_Stop is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Stop_All;
    when Is_Simulated =>
      Simulation.Stop_All;
    when Is_Udp =>
      Udp.Stop_All;
    end case;
  end Do_Stop;


  procedure Do_Close_Communication is
  begin
    case The_Connection_Kind is
    when Is_Serial =>
      Serial.Finish;
    when Is_Simulated =>
      Simulation.Finish;
    when Is_Udp =>
      Udp.Finish;
    end case;
  end Do_Close_Communication;

end Motor.Io.Protocol;
