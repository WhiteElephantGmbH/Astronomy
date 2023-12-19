-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.Focuser is

  function Exists return Boolean;

  function Connected return Boolean;

  function Moving return Boolean;

  function Actual_Position return Microns;

  procedure Connect;

  procedure Disconnect;

  procedure Go_To (Position : Microns);

  procedure Stop;

end PWI4.Focuser;
