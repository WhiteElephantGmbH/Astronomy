-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token;

package body Ada_95.Number is

  procedure Add (Item           : String;
                 Is_Real_Number : Boolean) is
  begin
    Token.Append_Literal (Handle'(new String'(Item)), Is_Real_Number);
  end Add;


  function Image_Of (The_Handle : Handle) return String is
  begin
    return The_Handle.all;
  end Image_Of;


  function Length_Of (The_Handle : Handle) return Natural is
  begin
    return The_Handle'length;
  end Length_Of;

end Ada_95.Number;
