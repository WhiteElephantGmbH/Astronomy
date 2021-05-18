-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Network;
with Space;

package M_Zero is

  type State is (Error, Disconnected, Connected, Initialized, Moving, Approaching, Tracking, Solving);

  type Information is record
    Status    : State;
    Direction : Space.Direction;
  end record;

  type Moving_Rate is (Guiding, Centering, Finding, Slewing);

  type Moving_Direction is (Up, Down, Left, Right);

  procedure Set_Error (Message : String);

  function Error_Message return String;

  procedure Connect (Server_Address : Network.Ip_Address;
                     Server_Port    : Network.Port_Number);

  procedure Initialize;

  procedure Set_Rate (Rate : Moving_Rate);

  procedure Start_Moving (Direction : Moving_Direction);

  procedure Stop_Moving (Direction : Moving_Direction);

  procedure Stop_Moving;

  procedure Slew_To (Location : Space.Direction);

  procedure Synch_To (Location : Space.Direction);

  procedure Start_Solving;

  procedure End_Solving;

  function Get return Information;

  procedure Disconnect;

end M_Zero;
