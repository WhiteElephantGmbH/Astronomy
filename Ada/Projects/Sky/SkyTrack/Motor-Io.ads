-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

private package Motor.Io is

  procedure Define (Parameters_1 :     Parameters;
                    Parameters_2 :     Parameters;
                    The_Epsilon  : out Values);

  procedure Connect;

  function Device_Version return Hardware_Version;

  procedure Open;

  procedure Startup_Initialization;

  procedure Set_Autoguiding (The_Rate : Device.Autoguiding_Rate);

  function Actual_State return Device.State;

  function Actual_Synch_State return Device.Time_Synch_State;

  procedure Set_Positions (The_Positions : Values);

  procedure Update_Offsets (Offsets : Values);

  procedure Update_Positions (Offsets : Alignment.Offsets);

  function Position_Is_Known return Boolean;

  function Actual_Data return Information;

  function Actual_Board_Temperature return Celsius;

  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Value);

  procedure Move (The_Distance : Values);

  procedure Synchronize_Time (The_Time : out Time.Ut);

  procedure Update (Profile : Update_Profile);

  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Value);
  procedure Halt;

  procedure Close;

private

  function Freq return Natural;

  function Nspr_Of (The_Drive : Device.Drive) return Device.Step_Number;

  type Step_Direction is (Backward, Forward, Undefined);

  type Acceleration_Kind is (Accelerate, Decelerate, Keep_Speed);

  Actions_Per_Update : constant := 8;

  Actions_Per_Second : constant := Actions_Per_Update * Time_Divider;

  Maximum_Unsigned : constant := 2**31 - 1;

  Maximum_N : constant := Maximum_Unsigned / 4 - Maximum_Unsigned / 128;

  subtype N_Type is Natural range 0 .. Maximum_N;

  type Action is record
    N : Natural := 0; -- start step counter or rest clocks
    C : Natural := 0; -- start clock counter
    S : Natural := 0; -- number of steps
    K : Acceleration_Kind := Acceleration_Kind'first;
    D : Step_Direction    := Step_Direction'first;
  end record
  with
    Alignment => 4;
  for Action use record
    N  at  0 range 0..31;
    C  at  4 range 0..31;
    S  at  8 range 0..31;
    K  at 12 range 0..7;
    D  at 13 range 0..7;
  end record;

  No_Action : constant Action := (others => <>);

  subtype Action_List_Length is Natural range 0 .. 16;

  subtype Action_Range is Positive range 1 .. Action_List_Length'last;

  type Action_List is array (Action_Range range <>) of Action;

  No_Actions : constant Action_List(1..0) := (others => No_Action);

  subtype Step_Position is Integer;

  subtype Step_Count is Integer;

  type Step_Positions is record
    M1 : Step_Position := 0;
    M2 : Step_Position := 0;
  end record
  with
    Alignment => 4,
    Size      => 2 * 32;
  for Step_Positions use record
    M1 at  0 range 0..31;
    M2 at  4 range 0..31;
  end record;

  type Step_Information is record
    Positions : Step_Positions := (others => <>);
    Offsets   : Step_Positions := (others => <>);
  end record;

end Motor.Io;
