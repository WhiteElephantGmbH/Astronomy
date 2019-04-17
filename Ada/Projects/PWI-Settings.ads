-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;

package PWI.Settings is

  File_Not_Found    : exception;
  Missing_Longitude : exception;
  Missing_Latitude  : exception;
  Missing_Elevation : exception;

  procedure Read (Filename : String);

  function Latitude return Angle.Value;

  function Longitude return Angle.Value;

  function Elevation return Integer; -- in meters

end PWI.Settings;
