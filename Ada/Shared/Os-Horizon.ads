-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Strings;

package Os.Horizon is

  subtype Arguments is Strings.Item;

  function Result_Of_Get_With (Item : Arguments) return String;

end Os.Horizon;
