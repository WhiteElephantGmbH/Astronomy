-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Memory;

package Ada_95.Texts is

  type Handle is private;

  function New_String (Item : String) return Handle with Inline;

  function Image_Of (Item : Handle) return String with Inline;

  function Operator_Of (Item : Handle) return String with Inline;

  function Length_Of (Item : Handle) return Natural with Inline;

private
  type Handle is access String;
  for Handle'storage_pool use Memory.Pool.all;
end Ada_95.Texts;
