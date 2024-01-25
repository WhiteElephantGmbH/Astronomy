-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.Rotator is

  function Moving return Boolean;

  function Slewing return Boolean;

  function Field_Angle return Degrees;

  function Mech_Position return Degrees;

  procedure Find_Home;

  procedure Goto_Mech (Position : Degrees);

  procedure Goto_Field (Position : Degrees);

  procedure Goto_Offset (Distance : Degrees);

  procedure Stop;

end PWI4.Rotator;
