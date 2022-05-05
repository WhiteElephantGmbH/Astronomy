-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network;
with Space;

package Ten_Micron is

  type State is (Tracking,
                 Stopped,
                 Parking,
                 Unparking,
                 Homing,
                 Parked,
                 Slewing,
                 Halted,        -- not moving (tracking off)
                 Inhibited,     -- low temperator (motors pre-heating)
                 Outside,       -- not within tracking limits
                 Following,     -- following a precalculated trajectory
                 Inconsistent,  -- user intervention needed
                 Unknown,       -- unknown mount status
                 Failure,       -- error
                 Disconnected); -- no connection to mount

  type Information is record
    Status    : State := Disconnected;
    Direction : Space.Direction;
  end record;

  procedure Startup (Server_Address : Network.Ip_Address;
                     Server_Port    : Network.Port_Number);

  procedure Slew_To (Location : Space.Direction);
  
  procedure Park;

  procedure Stop;

  procedure Unpark;

  function Get return Information;

  procedure Disconnect;

end Ten_Micron;
