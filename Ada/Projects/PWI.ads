-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI is

  Command_Failed : exception;
  No_Server      : exception;

  procedure Get_System;

private
  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "");

end PWI;
