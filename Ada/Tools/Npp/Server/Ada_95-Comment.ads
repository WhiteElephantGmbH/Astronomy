-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Memory;

package Ada_95.Comment is

  type Handle is private;

  function Is_Null (Item : Handle) return Boolean with Inline;

  procedure Clear (The_Item : in out Handle) with Inline;

  procedure Add (Item : String) with Inline;

  function Is_Special (The_Handle : Handle) return Boolean;

  function Text_Of (The_Handle : Handle) return String with Inline;

  function Image_Of (The_Handle : Handle) return String with Inline;

  function Length_Of (The_Handle : Handle) return Natural with Inline;

private
  type Handle is access String;
  for Handle'storage_pool use Memory.Pool.all;
end Ada_95.Comment;
