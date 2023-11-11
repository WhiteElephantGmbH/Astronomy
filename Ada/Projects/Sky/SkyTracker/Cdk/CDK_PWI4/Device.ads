-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Earth;
with PWI4;
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

  subtype Degrees is PWI4.Degrees;

  function Image_Of (Item : Degrees) return String renames PWI4.Image_Of;

  subtype Speed is PWI4.Arc_Second; -- per second

  function Image_Of (Item : Speed) return String renames PWI4.Image_Of;

  subtype Microns is PWI4.Microns;

  package Mount is

    type State is (Unknown,
                   Disconnected,
                   Connected,
                   Enabled,
                   Stopped,
                   Approaching,
                   Tracking,
                   Error);

    type Information is record
      J2000_Direction  : Space.Direction;
      Actual_Direction : Space.Direction;
      Local_Direction  : Earth.Direction;
      Az_Axis          : PWI4.Axis_Data;
      Alt_Axis         : PWI4.Axis_Data;
    end record;

    type State_Handler_Access is access procedure (The_State : State);

    function Actual_Info return Information;

    procedure Connect;

    procedure Disconnect;

    procedure Enable;

    procedure Disable;

    procedure Find_Home (Completion_Time : out Time.Ut);

    function At_Home return Boolean;

    procedure Goto_Target (Direction       :     Space.Direction;
                           Completion_Time : out Time.Ut);

    procedure Update_Target (Offset : Space.Direction);

    procedure Set_Rate_Ra (Item : Speed);

    procedure Set_Rate_Dec (Item : Speed);

    procedure Stop_Rate;

    procedure Goto_Mark (Direction       :     Earth.Direction;
                         Completion_Time : out Time.Ut);

    procedure Confirm_Goto;

    procedure Stop;
    -- stoppes the motors

    function Actual_Direction return Space.Direction;

  end Mount;

  package Focuser is

    procedure Enable;

    procedure Disable;

  end Focuser;

  package Rotator is

    procedure Enable;

    procedure Disable;

  end Rotator;

  package M3 is

    type Position is (Unknown,
                      Between,
                      Ocular,
                      Camera);

    subtype Place is Position range Ocular .. Camera;

    type Position_Handler_Access is access procedure (The_Position : Position);

    procedure Turn (To : Place);

  end M3;

  package Fans is

    type State is (Off, On);

    Initial_State : constant State := Off;

    procedure Turn (To : State);

    procedure Turn_On_Or_Off;

    type State_Handler_Access is access procedure (The_State : State);

  end Fans;

  procedure Start (Fans_State_Handler  : Fans.State_Handler_Access;
                   Mount_State_Handler : Mount.State_Handler_Access;
                   M3_Position_Handler : M3.Position_Handler_Access);

  procedure Finalize;

end Device;
