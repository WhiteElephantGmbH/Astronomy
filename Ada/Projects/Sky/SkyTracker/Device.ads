-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Angle;
with Earth;
with PWI;
with Space;
with Time;

package Device is

  type Command is (No_Command,
                   Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   Increase_Speed,
                   Decrease_Speed,
                   Increase_Time,
                   Decrease_Time,
                   Enter,
                   Stop,
                   Set_Guiding_Rate,
                   Set_Centering_Rate,
                   Set_Finding_Rate,
                   Set_Slewing_Rate);

  use type PWI.Encoder_Degrees;

  subtype Encoder_Degrees is PWI.Encoder_Degrees delta 0.000_000_1 range -999.999_999_9 .. 999.999_999_9;

  function Image_Of (Item : Encoder_Degrees) return String;

  subtype Microns is PWI.Microns;

  type Encoder_Limits is record
    Azm_Lower_Goto : Encoder_Degrees;
    Azm_Upper_Goto : Encoder_Degrees;
    Alt_Lower_Goto : Encoder_Degrees;
    Alt_Upper_Goto : Encoder_Degrees;
  end record;

  function Limits return Encoder_Limits;

  package Fans is

    type State is (Off, On);

    Initial_State : constant State := Off;

    procedure Turn (To : State);

    procedure Turn_On_Or_Off;

    type State_Handler_Access is access procedure (The_State : State);

  end Fans;

  package Mount is

    type Drive is (D1, D2);

    type Speed is array (Drive) of Angle.Signed;

    type State is (Unknown,
                   Disconnected,
                   Connected,
                   Enabled,
                   Homing,
                   Synchronised,
                   Stopped,
                   Approaching,
                   Tracking);

    type Encoder_Data is record
      Azm : Encoder_Degrees;
      Alt : Encoder_Degrees;
    end record;

    type Information is record
      J2000_Direction  : Space.Direction;
      Actual_Direction : Space.Direction;
      Local_Direction  : Earth.Direction;
      Encoder          : Encoder_Data;
    end record;

    type State_Handler_Access is access procedure (The_State : State);

    function Actual_Encoder return Encoder_Data;

    function Actual_Info return Information;

    procedure Connect;

    procedure Disconnect;

    procedure Enable;

    procedure Disable;

    procedure Find_Home (Completion_Time : out Time.Ut);

    procedure Set_Pointing_Model;

    procedure Goto_Target (Direction       :     Space.Direction;
                           With_Speed      :     Speed;
                           Completion_Time : out Time.Ut);

    procedure Goto_Mark (Direction       :     Earth.Direction;
                         Completion_Time : out Time.Ut);

    procedure Jog (Rate : Speed);

    procedure Stop;
    -- stoppes the motors

    function Actual_Direction return Space.Direction;

  end Mount;

  package M3 is

    type Position is (Unknown,
                      Between,
                      Ocular,
                      Camera);

    subtype Place is Position range Ocular .. Camera;

    type Position_Handler_Access is access procedure (The_Position : Position);

    procedure Turn (To : Place);

  end M3;

  package Focuser is

    procedure Connect;

    procedure Disconnect;

    procedure Set (The_Position : Microns);

    procedure Move;

    function Position return Microns;

  end Focuser;

  package Rotator is

    procedure Find_Home;

    procedure Start;

    procedure Stop;

  end Rotator;

  procedure Start (Fans_State_Handler  : Fans.State_Handler_Access;
                   Mount_State_Handler : Mount.State_Handler_Access;
                   M3_Position_Handler : M3.Position_Handler_Access;
                   Pointing_Model      : String);

  procedure Finalize;

end Device;
