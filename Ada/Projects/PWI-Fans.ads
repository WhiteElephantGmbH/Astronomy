-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI.Fans is

  procedure Turn_On;

  procedure Turn_Off;

  function Turned_On return Boolean;

end PWI.Fans;
