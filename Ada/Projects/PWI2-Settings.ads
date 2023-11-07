-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI2.Settings is

  File_Not_Found : exception;

  Missing_Longitude : exception;
  Missing_Latitude  : exception;
  Missing_Elevation : exception;

  Missing_Lower_Azm_Goto_Limit : exception;
  Missing_Upper_Azm_Goto_Limit : exception;
  Missing_Lower_Alt_Goto_Limit : exception;
  Missing_Upper_Alt_Goto_Limit : exception;

  procedure Read (Filename : String);

  function Lower_Azm_Goto_Limit return Encoder_Degrees;

  function Upper_Azm_Goto_Limit return Encoder_Degrees;

  function Lower_Alt_Goto_Limit return Encoder_Degrees;

  function Upper_Alt_Goto_Limit return Encoder_Degrees;

end PWI2.Settings;
