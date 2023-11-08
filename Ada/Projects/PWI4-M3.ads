-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.M3 is

  procedure Turn (To : Port);

  function Exists return Boolean;

  function Actual_Port return M3_Port;

end PWI4.M3;
