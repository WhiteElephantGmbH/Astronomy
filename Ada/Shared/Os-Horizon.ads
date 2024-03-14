-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Text;

package Os.Horizon is

  subtype Arguments is Text.Strings;

  function Result_Of_Get_With (Item : Arguments) return String;

end Os.Horizon;
