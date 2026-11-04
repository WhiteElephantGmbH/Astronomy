-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Lexical;

package Ada_95.Symbol is

  subtype Delimiter is Lexical.Delimiter;

  procedure Add (Item : Delimiter) with Inline;

  function Image_Of (Value : Delimiter) return String with Inline;

  function Length_Of (Value : Delimiter) return Natural with Inline;

end Ada_95.Symbol;
