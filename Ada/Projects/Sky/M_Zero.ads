-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network;
with Space;

package M_Zero is

  type State is (Error, Disconnected, Connected, Initialized, Moving, Approaching, Tracking);

  type Information is record
    Status    : State;
    Direction : Space.Direction;
  end record;

  type Moving_Rate is (Centering, Guiding, Finding, Slewing);

  type Directions is (Up, Down, Left, Right);

  function Error_Message return String;

  procedure Connect (Server_Address : Network.Ip_Address);

  procedure Initialize;

  procedure Set_Rate (Rate : Moving_Rate);

  procedure Start_Move (Direction : Directions);

  procedure Stop_Move (Direction : Directions);

  procedure Stop_Move;

  procedure Slew_To (Location : Space.Direction);

  procedure Synch_To (Location : Space.Direction);

  function Get return Information;

  procedure Disconnect;

end M_Zero;
