-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token;

package body Ada_95.Characters is

  procedure Add (Item : Character) is
  begin
    Token.Append_Literal (Item);
  end Add;

end Ada_95.Characters;
