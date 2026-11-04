-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Target is

  procedure Promote (Filename : String);
  
  function Run (Executable : String;
                Parameters : String) return Boolean;

end Target;

