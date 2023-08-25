-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

generic
  Package_Name : String;
package Key is

  function Name return String is (Package_Name);

  function New_Item return String;

end Key;
