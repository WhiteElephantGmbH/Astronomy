-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI.Mount is

  type Hours is delta 0.00000001 range 0.0 .. 24.0;

  type Degrees is delta 0.0000001 range -90.0 .. 90.0;

  type State is (Disconnected, -- not Connected
                 Connected,    -- Connected and not (Azm_Enabled and Alt_Enabled)
                 Enabled,      -- Azm_Enabled and Alt_Enabled and not Encoders_Have_Been_Set and not Is_Finding_Home
                 Homing,       -- Azm_Enabled and Alt_Enabled and not Encoders_Have_Been_Set and Is_Finding_Home
                 Synchronised, -- Encoders_Have_Been_Set and not Pointing_Model_Set
                 Stopped,      -- Encoders_Have_Been_Set and Pointing_Model_Set and not Tracking
                 Approaching,  -- Tracking and not On_Target
                 Tracking);    -- not yet decoded

  type Information is record
    Status   : State;
    Ra       : Hours;
    Dec      : Degrees;
    Ra_2000  : Hours;
    Dec_2000 : Degrees;
  end   record;

  function Info return Information;

  procedure Define_Pointing_Model (Filename : String);

  function Defined_Pointing_Model return String;

  function Status return State;

  procedure Connect;

  procedure Disconnect;

  procedure Enable;

  procedure Disable;

  procedure Find_Home;

  procedure Set_Pointing_Model;

  procedure Move (Ra         : Hours;
                  Dec        : Degrees;
                  From_J2000 : Boolean := False);

  procedure Stop;

end PWI.Mount;
