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

with Angle;
with Device;
with Time;

package Motor is

  type Hardware_Version is (Stepper_Version_0, Stepper_Version_1, Stepper_Version_2, Unknown, Undefined);

  type Celsius is delta 10.0**(-1) range -273.2 .. 3276.7;

  Time_Divider : constant := 2;

  Time_Delta : constant Duration := Time.One_Second / Time_Divider;

  type State is (Disconnected, Fault, Startup, Ready, Parked,
                 Parking, Directing, Positioned, Positioning, Update, Terminated);

  type Position is private;

  type Position_Data is record
    Positions : Position;
    Offsets   : Position;
    Inverted  : Boolean := False;
  end record;

  type Speed is private;

  Resting : constant Speed;

  type State_Handler_Access is access procedure (The_State : State);

  procedure Define (First_Acceleration   : Angle.Value;
                    Second_Acceleration  : Angle.Value;
                    Maximum_Speed        : Angle.Value;
                    Park_Position        : Position;
                    Pole_Height          : Angle.Value;
                    Meridian_Flip_Offset : Angle.Value);
  -- set parameters

  procedure Connect_Communication;

  function Version return Hardware_Version;
  -- PRECONDITION: after Connect_Communication

  procedure Open_Communication (State_Handler : State_Handler_Access);

  procedure Initialize;

  procedure Set (Autoguiding_Rate : Device.Autoguiding_Rate);

  procedure Define_Positions;

  procedure Synch_Park_Position;
  -- synchronises the park position

  procedure Synch_Position (To : Position);
  -- synchronises to a position

  function Time_When_Parked return Time.Ut;
  -- move to park position

  function Time_When_Positioned (To : Position) return Time.Ut;
  -- move to position

  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed);
  -- move drive

  procedure Synchronize (Tbegin : out Time.Ut);
  -- synchronize start time

  procedure Change_Time (T : Time.Ut);
  -- change the start time for calculations

  procedure Update;
  -- update next profile

  procedure Update_Offsets (To : Position);
  -- update offsets without changing the motor position

  procedure Update_Positions;
  -- update the alignment position and clears the offsets

  procedure Synchronize_Positions;
  -- synchronize the positions (clears the offsets).

  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed);
  -- adjust drive

  procedure Stop;
  -- stoppes the motors

  procedure Allow_Inversion;
  -- new target allows position inversion

  At_Limit : exception;

  function Time_When_At (The_Position : Position) return Time.Ut;
  -- calculates the time when positioned

  function Arriving_Time (At_Position : Position;
                          With_Speed  : Speed) return Time.Ut;
  -- returns the calculated time when at position with speed

  function Approach_Target_At (The_Position : Position;
                               With_Speed   : Speed := Resting) return Boolean;
  -- returns true if approaching Target

  function Following (To          : Position;
                      Final_Speed : Speed;
                      At_Time     : Time.Ut) return Boolean;
  -- returns true if following the target to the specified position

  function Synch_State return Device.Time_Synch_State;

  function Positions return Position_Data;

  function Is_Inverse return Boolean;

  procedure Close_Communication;

  function Position_Of (First  : Angle.Value;
                        Second : Angle.Value) return Position with Inline;

  function Is_Defined (The_Position : Position) return Boolean with Inline;

  function First_Of (The_Position : Position) return Angle.Value with Inline;

  function Second_Of (The_Position : Position) return Angle.Value with Inline;

  procedure Set_Undefined (The_Position : in out Position) with Inline;

  function "-" (Left, Right : Position) return Position;

  function "-" (Left, Right : Position) return Speed;

  function Board_Temperature_Is_Known return Boolean;

  function Board_Temperature return Celsius;

private

  Unknown_Board_Temperature : constant Celsius := Celsius'first;

  subtype Drive is Device.Drive;

  type Position is record
    First      : Angle.Value := Angle.Zero; -- RA or AZ axis
    Second     : Angle.Value := Angle.Zero; -- DEC or ALT axis
    Is_Defined : Boolean := False;
  end record;

  type Speed is record
    First  : Angle.Signed;
    Second : Angle.Signed;
  end record;

  Resting : constant Speed := (others => 0);

  subtype Value is Angle.Degrees; -- position in ° , accelleration °/s², speed °/s or time in s

  type Values is array (Drive) of Value;

  type Information is record
    Positions : Values;
    Offsets   : Values;
  end record;

  function "=" (Left, Right : Values) return Boolean;

  function Is_Zero (Item : Values) return Boolean;

  type Parameters is record
    Vm : Value; -- maximum speed
    Am : Value; -- maximum accelleration
    D  : Drive;
  end record;

  type Start_Profile is record
    Tb : Value; -- time at begin
    V1 : Value; -- maximum speed motor 1
    V2 : Value; -- maximum speed motor 2
    A1 : Value; -- maximum accelleration motor 1
    A2 : Value; -- maximum accelleration motor 2
    L1 : Value; -- first lower limit
    U1 : Value; -- first upper limit
    L2 : Value; -- second lower limit
    U2 : Value; -- second upper limit
  end record;

  type Delta_Profile is record
    Sb  : Value   := 0.0; -- distance at begin (if Sb <> previous Se then Set position)
    Vb  : Value   := 0.0; -- speed at begin
    V   : Value   := 0.0; -- actual speed
    A   : Value   := 0.0; -- actual accelleration
    Se  : Value   := 0.0; -- distance at end
    Ve  : Value   := 0.0; -- speed at end
    Dtm : Time.Ut := 0.0; -- maximum speed reached
    Dtd : Time.Ut := 0.0; -- deaccelleration begin
    Dtf : Time.Ut := 0.0; -- following begin (deaccelleration end)
  end record;

  type Update_Profile is array (Device.Drive) of Delta_Profile;

  Driver_Closing : exception;

  procedure Get (T       :        Time.Ut;
                 S       :    out Value;
                 V       :    out Value;
                 T_Begin : in out Time.Ut;
                 P       : in out Delta_Profile);

end Motor;
