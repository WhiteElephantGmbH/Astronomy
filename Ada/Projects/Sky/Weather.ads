-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Refraction;

package Weather is

  function Requested return Boolean;

  function Air_Pressure return Refraction.Hectopascal;

  function Temperature return Refraction.Celsius;

end Weather;
