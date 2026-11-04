-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Lexical;
with Memory;

package Ada_95.Word is

  Max_Length : constant := 64;

  type Case_Style is (Lower_Case, Upper_Case, Capitalized, Mixed_Case, Undefined_Case);

  type Item (Length : Natural) is record
    Image : String (1 .. Length);
    Key   : Lexical.Name_Key;
    Style : Case_Style;
  end record;

  type Handle is access Item;
  for Handle'storage_pool use Memory.Global_Pool.all;


  procedure Add (Image : String);

  function Handle_Of (Image : String) return Handle;

  procedure Finalize;

end Ada_95.Word;
