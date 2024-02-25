-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;
with Earth;
with Network;
with Refraction;
with Space;
with Time;

private with Traces;

package Ten_Micron is

  type State is (Tracking,
                 Stopped,
                 Parking,
                 Unparking,
                 Homing,
                 Parked,
                 Slewing,
                 Positioned,    -- not moving (tracking off)
                 Inhibited,     -- low temperator (motors pre-heating)
                 Outside,       -- not within tracking limits
                 Following,     -- following a precalculated trajectory
                 Inconsistent,  -- user intervention needed
                 Unknown,       -- unknown mount status
                 Failure,       -- error
                 Preparing,     -- slewing to start of the transit
                 Waiting,       -- waiting for the satellite, stopped at the start of the transit
                 Catching,      -- slewing to catch the satellite
                 Ended,         -- the transit has ended
                 Capturing,     -- Camera is capturing a picture
                 Solving,       -- picture solving
                 Disconnected); -- no connection to mount

  subtype Error_State is State range Inconsistent .. Failure;

  subtype Transit_State is State range Preparing .. Ended;

  type Target_Kind is (Axis_Position, Near_Earth_Object, Other_Targets);

  type Time_Offset is delta 0.0001 range -9.9999 .. 9.9999 with Small => 0.0001;

  Undefined_Pier : constant Character := ' ';

  type Information is record
    Status       : State := Disconnected;
    Date_Time    : Time.Ut := Time.Unknown;
    Direction    : Space.Direction;
    Position     : Space.Direction;
    Pier_Side    : Character := Undefined_Pier;
    Moving_Speed : Angle.Value := Angle.Zero;
  end record;

  type Polar_Error is delta 0.01 range 0.0 .. 99.9999;

  type Axis_Angle is delta 0.01 range 0.0 .. 999.99;

  type Orthogonality is delta 0.0001 range -99.9999 .. 99.9999;

  type Knob_Turns is delta 0.01 range -99.99 .. 99.99;

  type Arc_Seconds is delta 0.1 range 0.0 .. 99999.9;

  No_Data : constant := 0.0;

  type Alignment_Data is record
    Ra_Axis_Direction   : Earth.Direction;
    Polar_Align_Error   : Polar_Error   := No_Data;
    Ra_Axis_Angle       : Axis_Angle    := No_Data;
    Orthogonality_Error : Orthogonality := No_Data;
    Az_Knob_Turns_Left  : Knob_Turns    := No_Data;
    Alt_Knob_Turns_Down : Knob_Turns    := No_Data;
    Modeling_Terms      : Natural       := 0;
    Rms_Error           : Arc_Seconds   := No_Data;
  end record;

  type Command is (Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   Move_Left_End,
                   Move_Right_End,
                   Move_Up_End,
                   Move_Down_End,
                   Move_End,
                   Increase_Moving_Rate,
                   Decrease_Moving_Rate);

  function Is_Expert_Mode return Boolean;

  procedure Startup;

  procedure Define (The_Air_Pressure : Refraction.Hectopascal);

  procedure Define (The_Temperature : Refraction.Celsius);

  procedure Set_Julian_Date (Item : Time.JD);

  procedure Set_Time_Offset (Item : Duration);

  procedure Slew_To (Location : Space.Direction;
                     Target   : Target_Kind := Other_Targets);

  procedure Synch_To (Location : Space.Direction);

  procedure Start_Capturing;

  procedure Start_Solving;

  procedure End_Solving;

  procedure Load_Tle (Name : String);

  procedure Park;

  procedure Stop;

  procedure Unpark;

  procedure Execute (The_Command : Command);

  function Gps_Is_Synchronized return Boolean;

  function Julian_Date return Time.JD;

  function Get return Information;

  procedure Start_Alignment;

  procedure Add_Alignment_Point (Mount   :     Space.Direction;
                                 Picture :     Space.Direction;
                                 Side    :     Character;
                                 Lmst    :     Time.Value;
                                 Points  : out Natural);

  function End_Alignment return Boolean;

  function Has_New_Alignment_Info return Boolean;

  function Alignment_Info return Alignment_Data;

  function Updated (Time_Change : Time_Offset) return Boolean;

  function Image_Of (Value : Time_Offset) return String;

  procedure Disconnect;

private

  Id : constant String := "10micron";

  package Log is new Traces (Id);

  Is_In_Expert_Mode  : Boolean;
  The_Server_Address : Network.Ip_Address;
  The_Server_Port    : Network.Port_Number;

end Ten_Micron;
