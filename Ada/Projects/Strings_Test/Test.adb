-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Assertions;
with Ada.Text_IO;
with Strings;
with Exceptions;

package body Test is

  package IO renames Ada.Text_IO;

  Test_Ok : Boolean := True;


  function Equal (A, R : String) return String is
  begin
    if A = R then
      return " -> Ok";
    else
      Test_Ok := False;
      return " -> Error";
    end if;
  end Equal;


  procedure Execute is
    A : constant String := "Hello World";
    B : constant Strings.Item := ["Hello", "World"];
    C : constant String := B.To_Data (Separator => " ");
    D : Strings.Item := [];
  begin
    IO.Put_Line ("String Test");
    IO.Put_Line ("A -> " & A'image);
    IO.Put_Line ("");
    IO.Put_Line ("B -> " & B'image);
    for E of B loop
      IO.Put_Line ("-> " & E'image);
    end loop;
    IO.Put_Line ("");
    IO.Put_Line ("C = A -> " & C'image & Equal (A, C));
    IO.Put_Line ("");
    IO.Put_Line ("Empty => " & D'image & Equal (D'image, "[]"));
    for Unused of D loop
      raise Program_Error;
    end loop;
    D.Append ("One");
    D.Append ("Two");
    D.Append ("Tree");
    IO.Put_Line ("D -> " & D'image);
    for E of reverse D when E /= "Two" loop
      IO.Put_Line ("-> " & E);
    end loop;
    IO.Put_Line ("D.First -> " & D.First & Equal (D.First, "One"));
    IO.Put_Line ("D.Last -> " & D.Last & Equal (D.Last, "Tree"));
    IO.Put_Line ("");
    declare
      Table : constant Strings.Item := ["A", "BB", "CCC", "DDDD", "EEEEE", "FFFFF"];
      Part  : Strings.Item;
      List  : Strings.List;
    begin
      Part := Table.Part(2, 4);
      List := Part.To_List;
      IO.Put_Line ("List: " & List'image);
      for S of List loop
       IO.Put_Line ("-> " & S);
      end loop;
      IO.Put_Line ("");
      List.Delete_First;
      Part := [List.First_Element, List.Last_Element];
      declare
        Image  : constant String := "  " & Part'image & "  ";
        Tokens : Strings.Item;
      begin
        IO.Put_Line ("Part: " & Image);
        Tokens := Strings.Item_Of (Image, Separator => ' ', Symbols => "[,]", Purge => True);
        for T of Tokens loop
         IO.Put_Line ("-> " & T);
        end loop;
        IO.Put_Line ("Tokens: " & Tokens.To_Data & Equal (Tokens.To_Data, "[""CCC"",""DDDD""]"));
      end;
    end;
    IO.Put_Line ("");
    declare
      Parameter : constant String := ",,,";
      Arguments : constant Strings.Item := Strings.Item_Of (Parameter, Separator => ',');
    begin
      IO.Put_Line ("Arguments for " & Parameter);
      for A of Arguments loop
        IO.Put_Line ("-> " & A);
      end loop;
      IO.Put_Line ("Parameter: " & Arguments'image & Equal (Arguments'image, "["""", """", """", """"]"));
    end;
    IO.Put_Line ("");
    begin
      declare
        Big_String : constant String (1..20000) := [others => '*'];
        S : Strings.Item := [Big_String];
      begin
        S.Append (Big_String);
      end;
      IO.Put_Line ("Precontition check failed");
      Test_Ok := False;
    exception
    when Ada.Assertions.Assertion_Error =>
      IO.Put_Line ("Precontition checked");
    end;
    IO.Put_Line ("");
    IO.Put_Line ("Test Complete : " & Test_Ok'image);
  exception
  when Occurrence: others =>
    IO.Put_Line (Exceptions.Information_Of (Occurrence));
  end Execute;

end Test;
