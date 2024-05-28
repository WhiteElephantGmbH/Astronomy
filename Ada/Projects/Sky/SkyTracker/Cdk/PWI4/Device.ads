-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Name;
with Persistent;
with PWI4;
with Site;
with Space;
with Time;

package Device is

  subtype Degrees is PWI4.Degrees;

  function Image_Of (Item : Degrees) return String renames PWI4.Image_Of;

  subtype Speed is PWI4.Arc_Second; -- per second

  function Image_Of (Item : Speed) return String renames PWI4.Image_Of;

  subtype Microns is PWI4.Microns;

  subtype Points is PWI4.Points;

  function Image_Of (Item : Points) return String renames PWI4.Image_Of;

  function Site_Info return Site.Data;

  function Site_Lmst return Time.Value;

  function Error_Info return String renames PWI4.Error_Info;

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
      Model            : PWI4.Model_Data;
    end record;

    type State_Handler_Access is access procedure (The_State : State);

    function Actual_Info return Information;

    function Az_Axis_Minimum return Degrees;

    procedure Connect;

    procedure Disconnect;

    procedure Enable;

    procedure Disable;

    procedure Find_Home (Completion_Time : out Time.Ut);

    function At_Home return Boolean;

    procedure Goto_Target (Direction       :     Space.Direction;
                           Completion_Time : out Time.Ut);

    procedure Update_Target (Offset : Space.Direction);

    procedure Set_Rate_Axis0 (Item : Speed);

    procedure Set_Rate_Axis1 (Item : Speed);

    procedure Set_Rate_Dec (Item : Speed);

    procedure Set_Rate_Ra (Item : Speed);

    procedure Set_Rate_Path (Item : Speed);

    procedure Set_Rate_Transverse (Item : Speed);

    procedure Stop_Rate;

    procedure Set_Moving (Alt_Speed : Speed;
                          Az_Speed  : Speed);

    procedure Spiral_Offset_Center;

    procedure Spiral_Offset_Next;

    procedure Spiral_Offset_Previous;

    procedure Reset_Moving_Target;

    procedure Goto_Mark (Direction       :     Earth.Direction;
                         Completion_Time : out Time.Ut);

    procedure Confirm_Goto;

    procedure Follow_Tle (Id : Name.Id);

    procedure Set_Az_Axis_Wrap (Range_Min : Degrees);

    procedure Add_To_Model (Direction : Space.Direction);

    procedure Stop;

  end Mount;

  package Focuser is

    package Persistent_Position is new Persistent (Microns, Name => "Focuser");

    Persistent_Position_Data : Persistent_Position.Data;

    Stored_Position : Microns renames Persistent_Position_Data.Storage;

    type State is (Unknown, Disconnected, Connected, Moving);

    type State_Handler_Access is access procedure (The_State : State);

    function Exists return Boolean;

    function Actual_Position return Microns;

    procedure Connect;

    procedure Disconnect;

    procedure Find_Home;

    procedure Go_To (The_Position : Microns);

    procedure Stop;

  end Focuser;

  package Rotator is

    procedure Find_Home;

    function Moving return Boolean;

    function Slewing return Boolean;

    function Mech_Position return Degrees;

    function Field_Angle return Degrees;

    procedure Goto_Field (The_Angle : Degrees);

    procedure Goto_Mech (The_Position : Degrees);

    procedure Go_To (The_Offset : Degrees);

    procedure Stop;

  end Rotator;

  package M3 is

    type Position is (Unknown,
                      Between,
                      Ocular,
                      Camera);

    subtype Place is Position range Ocular .. Camera;

    type Position_Handler_Access is access procedure (The_Position : Position);

    function Exists return Boolean;

    procedure Rotate;

    procedure Turn_To_Occular;

  end M3;

  package Fans is

    type State is (Off, On);

    procedure Turn (To : State);

    procedure Turn_On_Or_Off;

  end Fans;

  procedure Start (Mount_State_Handler   : Mount.State_Handler_Access;
                   Focuser_State_Handler : Focuser.State_Handler_Access;
                   M3_Position_Handler   : M3.Position_Handler_Access);

  procedure Finalize;

end Device;
