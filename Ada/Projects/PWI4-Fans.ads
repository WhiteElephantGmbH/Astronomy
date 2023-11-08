-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.Fans is

  function Exists return Boolean;

  procedure Turn_On;

  procedure Turn_Off;

  function Turned_On return Boolean;

end PWI4.Fans;
