-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Memory;

package Ada_95.Number is

  type Handle is private;

  procedure Add (Item           : String;
                 Is_Real_Number : Boolean) with Inline;

  function Image_Of (The_Handle : Handle) return String with Inline;

  function Length_Of (The_Handle : Handle) return Natural with Inline;

private
  type Handle is access String;
  for Handle'storage_pool use Memory.Pool.all;
end Ada_95.Number;
