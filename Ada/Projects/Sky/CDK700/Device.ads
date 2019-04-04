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
with Space;
with Time;

package Device is

  type Command is (No_Command,
                   Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   Increase,
                   Decrease,
                   Enter,
                   Stop,
                   Set_Guiding_Rate,
                   Set_Centering_Rate,
                   Set_Finding_Rate,
                   Set_Slewing_Rate);

  package Fans is

    procedure Turn_On_Or_Off;

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

    type Information is record
      J2000_Direction  : Space.Direction;
      Actual_Direction : Space.Direction;
      Local_Direction  : Earth.Direction;
    end record;

    type State_Handler_Access is access procedure (The_State : State);

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

    procedure Direct (The_Drive  : Drive;
                      With_Speed : Angle.Signed);
    -- move drive

    procedure Adjust (The_Drive  : Drive;
                      With_Speed : Angle.Signed);
    -- adjust drive

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

  package Rotator is

    type State is (Unknown,
                   Disconnected,
                   Connected,
                   Homing,
                   Started);

    type State_Handler_Access is access procedure (The_State : State);

    procedure Connect;

    procedure Disconnect;

    procedure Find_Home;

    procedure Start;

    procedure Stop;

  end Rotator;

  procedure Start (Mount_State_Handler   : Mount.State_Handler_Access;
                   M3_Position_Handler   : M3.Position_Handler_Access;
                   Rotator_State_Handler : Rotator.State_Handler_Access;
                   Pointing_Model        : String);

  procedure Finalize;

end Device;
