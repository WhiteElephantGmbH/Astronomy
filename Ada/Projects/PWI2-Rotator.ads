-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI2.Rotator is

  type State is (Disconnected, -- not Connected
                 Connected,    -- Connected and not homing and not derotating
                 Homing,       -- Is_Finding_Home = True
                 Started);     -- AltAz derotate enabled

  function Status return State;

  procedure Find_Home (On : Port);

  procedure Start;

  procedure Stop;

end PWI2.Rotator;
