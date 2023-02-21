-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Earth;
with Network;
with Refraction;
with Space;
with Time;

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
                 Solving,       -- picture solving
                 Disconnected); -- no connection to mount

  type Target_Kind is (Axis_Position, Other_Targets);

  type Information is record
    Status    : State := Disconnected;
    Direction : Space.Direction;
    Position  : Space.Direction;
    Pier_Side : Character;
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

  procedure Startup (Server_Address : Network.Ip_Address;
                     Server_Port    : Network.Port_Number);

  procedure Define (The_Air_Pressure : Refraction.Hectopascal);

  procedure Define (The_Temperature : Refraction.Celsius);

  procedure Slew_To (Location : Space.Direction;
                     Target   : Target_Kind := Other_Targets);

  procedure Synch_To (Location : Space.Direction);

  procedure Start_Solving;

  procedure End_Solving;

  procedure Park;

  procedure Stop;

  procedure Unpark;

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

  procedure Disconnect;

end Ten_Micron;
