-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.Mount is

  subtype Speed is Arc_Second;

  subtype Axis_Rate is Degrees; -- per second

  type State is (Disconnected, -- not Connected
                 Connected,    -- Connected and not (Azm_Enabled and Alt_Enabled)
                 Enabled,      -- Azm_Enabled and Alt_Enabled and not Homed
                 Stopped,      -- Homed and not Slewing and not Tracking
                 Approaching,  -- Slewing or Slewing and Tracking
                 Tracking,     -- Tracking and not Slewing
                 Error);

  type Information is record
    Status    : State;
    Ra        : Hours;
    Dec       : Degrees;
    Ra_J2000  : Hours;
    Dec_J2000 : Degrees;
    Az        : Degrees;
    Alt       : Degrees;
    Az_Axis   : Axis_Data;
    Alt_Axis  : Axis_Data;
  end record;

  function Info return Information;

  procedure Connect;

  procedure Disconnect;

  procedure Enable;

  procedure Disable;

  procedure Find_Home;

  procedure Goto_Ra_Dec (Ra         : Hours;
                         Dec        : Degrees;
                         From_J2000 : Boolean := False);

  procedure Goto_Alt_Az (Alt : Degrees;
                         Az  : Degrees);

  procedure Confirm_Goto;

  procedure Stop;

end PWI4.Mount;
