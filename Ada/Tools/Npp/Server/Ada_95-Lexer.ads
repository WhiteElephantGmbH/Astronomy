-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Source;
with Ada_95.Token;

package Ada_95.Lexer is

  procedure Start;

  function Tokens_For (File : in out Source.Object'class) return Token.Sequence;

  procedure Finalize;

end Ada_95.Lexer;
