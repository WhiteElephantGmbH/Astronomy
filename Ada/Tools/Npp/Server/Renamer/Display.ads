-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Display is

  procedure Show (Item  : String);

  procedure Rename (From, To : String);

  procedure Error (Message : String);

  procedure Show_Result;

end Display;
