-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Coordinate;

generic
  with procedure Error (Message : String);
package Data is

  type Magnitude is delta 0.01 range -99.0 .. 99.0;

  procedure Read;

  procedure Reset;

  function Next return Boolean;

  function HR_Id return Natural;

  function HR_Image return String;

  function Name return String;

  function Location return Coordinate.Polar;

  function Point return Coordinate.Cartesian;

  function Mag return Magnitude;

  function Hr_Of (Loc : Coordinate.Polar) return Natural;

  function Image_Of (HR : Natural) return String;

  function Dec_Of (HR : Natural) return Coordinate.Declination;

  function Point_Of (HR : Natural) return Coordinate.Cartesian;

  function Name_Of (HR : Natural) return String;

  function Name_Of (Loc : Coordinate.Polar) return String;

end Data;
