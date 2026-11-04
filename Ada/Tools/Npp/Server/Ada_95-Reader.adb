-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Lexer;
with Ada_95.Source.File;
with Container.List;
with Log;

package body Ada_95.Reader is

  package Elements is new Container.List (Element);

  The_Library : Elements.Item;


  procedure Start is
  begin
    Lexer.Start;
  end Start;


  procedure Add (Filename : String) is
    The_Element : Element;
  begin
    declare
      The_File : Source.File.Item := Source.File.New_With (Filename);
    begin
      The_Element.Tokens := Lexer.Tokens_For (The_File);
      The_Library.Append (The_Element);
    end;
  exception
  when Occurrence: others =>
    Log.Write ("Library.Add", Occurrence);
  end Add;


  procedure Iterator is
    procedure Iterate is new Elements.Iterators (Visit);
  begin
    Iterate (The_Library);
  end Iterator;


  procedure Finalize is
  begin
    Lexer.Finalize;
    Elements.Clear (The_Library);
  end Finalize;

end Ada_95.Reader;
