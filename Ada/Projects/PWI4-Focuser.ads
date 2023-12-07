-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.Focuser is

  function Exists return Boolean;

  procedure Connect;
  
  procedure Disconnect;

  procedure Enable;
  
  procedure Disable;

  procedure Go_To (Position : Microns);

  procedure Stop;

end PWI4.Focuser;
