-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token;

package body Ada_95.Error is

  function Handle_Of (Item : String) return Handle is
  begin
    return new String'(Item);
  end Handle_Of;


  procedure Add (Message  : Kind;
                 Item     : String;
                 Position : Column_Range) is
  begin
    Token.Append_Error (Message, Handle_Of(Item), Position);
  end Add;


  function Image_Of (Item : Handle) return String is
  begin
    return Item.all;
  end Image_Of;


  function Length_Of (Item : Handle) return Natural is
  begin
    return Item'length;
  end Length_Of;

end Ada_95.Error;
