-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network;
with Refraction;
with Space;

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

  procedure Disconnect;

end Ten_Micron;
