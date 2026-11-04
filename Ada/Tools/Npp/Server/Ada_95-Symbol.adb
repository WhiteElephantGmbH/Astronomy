-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token;

package body Ada_95.Symbol is

  procedure Add (Item : Delimiter) is
  begin
    Token.Append_Symbol (Item);
  end Add;


  function Image_Of (Value : Delimiter) return String is
  begin
    case Value is
    when Lexical.Semicolon         => return ";";
    when Lexical.Period            => return ".";
    when Lexical.Comma             => return ",";
    when Lexical.Assignment        => return ":=";
    when Lexical.Association       => return "=>";
    when Lexical.Equal             => return "=";
    when Lexical.Not_Equal         => return "/=";
    when Lexical.Greater           => return ">";
    when Lexical.Greater_Or_Equal  => return ">=";
    when Lexical.Less              => return "<";
    when Lexical.Less_Or_Equal     => return "<=";
    when Lexical.Ampersand         => return "&";
    when Lexical.Plus              => return "+";
    when Lexical.Minus             => return "-";
    when Lexical.Asterisk          => return "*";
    when Lexical.Slash             => return "/";
    when Lexical.Left_Bracket      => return "[";
    when Lexical.Right_Bracket     => return "]";
    when Lexical.Left_Parenthesis  => return "(";
    when Lexical.Right_Parenthesis => return ")";
    when Lexical.Exponentiation    => return "**";
    when Lexical.Colon             => return ":";
    when Lexical.Apostrophe        => return "'";
    when Lexical.Vertical_Line     => return "|";
    when Lexical.Quote             => return """";
    when Lexical.Range_Delimiter   => return "..";
    when Lexical.Unconstrained     => return "<>";
    when Lexical.Start_Label       => return "<<";
    when Lexical.End_Label         => return ">>";
    when Lexical.Target_Name       => return "@";
    when Lexical.Special_Id        => return "~";
    end case;
  end Image_Of;


  function Length_Of (Value : Delimiter) return Natural is
  begin
    return Image_Of (Value)'length;
  end Length_Of;

end Ada_95.Symbol;
