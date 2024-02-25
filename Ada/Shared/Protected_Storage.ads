-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

generic
  type Element is private;
package Protected_Storage is

  procedure Set (Item : Element);

  function Data return Element;

end Protected_Storage;
