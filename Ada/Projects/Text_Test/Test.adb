-- *********************************************************************************************************************
-- *                        (c) 2024 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                         *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Assertions;
with Ada.Text_IO;
with Exceptions;
with Log;
with Text;

package body Test is

  package IO renames Ada.Text_IO;

  Test_Ok : Boolean := True;


  procedure Check (Ok : Boolean) is
  begin
    if not Ok then
      Test_Ok := False;
    end if;
  end Check;


  function Equal (Left, Right : String) return String is
  begin
    if Left = Right then
      return " -> Ok";
    else
      Check (False);
      return " -> Error";
    end if;
  end Equal;


  Titel_Part_1 : constant String := "S T R I N G   ";
  Titel_Part_2 : constant String := "T E S T";

  Titel_List : constant Text.List := [Titel_Part_1, Titel_Part_2];

  Titel : constant String := Titel_List.To_Data;


  procedure Execute is
    use type Text.String;
    A : Text.String := ["o #name#"];
    B : constant Text.Strings := ["Hello", "World"];
    C : constant String := B.To_Data (Separator => " ");
    D : Text.Strings := [];
  begin
    IO.Put_Line (Titel);
    Check (Titel = Titel_Part_1 & Titel_Part_2);
    IO.Put_Line ("");

    A.Prepend ("Hell");
    A.Replace ("#name#", By => "World");
    IO.Put_Line ("A -> " & A'image);
    IO.Put_Line ("");
    IO.Put_Line ("B -> " & B'image);
    for E of B loop
      IO.Put_Line ("-> " & E'image);
    end loop;
    IO.Put_Line ("");
    IO.Put_Line ("C = A -> " & C'image & Equal (A.S, C));
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
      Table  : constant Text.Strings := ["A", "CCC", "BB", "DDDD", "EEEEE", "FFFFF"];
      Part   : Text.Strings;
      List   : Text.List;
      Vector : Text.Vector;
    begin
      Part := Table.Part(2, 4);
      List := Part.To_List;
      IO.Put_Line ("List: " & List'image);
      Text.Sort (List);
      IO.Put_Line ("Sorted List: " & List'image);
      for S of List loop
       IO.Put_Line ("-> " & S);
      end loop;
      Vector := List.To_Vector;
      IO.Put_Line ("Vector: " & Vector'image);
      List.Delete_First;
      Part := [List.First_Element, List.Last_Element];
      declare
        Image  : constant String := "  " & Part'image & "  ";
        Tokens : Text.Strings;
      begin
        IO.Put_Line ("Part: " & Image);
        Tokens := Text.Strings_Of (Image, Separator => ' ', Symbols => "[,]");
        for T of Tokens loop
         IO.Put_Line ("-> " & T);
        end loop;
        IO.Put_Line ("Tokens: " & Tokens.To_Data & Equal (Tokens.To_Data, "[""CCC"",""DDDD""]"));
      end;
    end;
    IO.Put_Line ("");

    declare
      Parameter : constant String := ",,,";
      Arguments : constant Text.Strings := Text.Strings_Of (Parameter, Separator => ',', Purge => False);
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
        S : Text.Strings := [Big_String];
      begin
        S.Append (Big_String);
      end;
      IO.Put_Line ("Precontition check failed");
      Check (False);
    exception
    when Ada.Assertions.Assertion_Error =>
      IO.Put_Line ("Precontition checked");
    end;
    IO.Put_Line ("");

    declare
      S : Text.String := [];
      L : Positive;
      B : Positive;
      P : constant String := "Pattern";
      U : constant String := Text.Uppercase_Of (P);
    begin
      IO.Put_Line ("Append to huge string");
      Log.Write ("Huge string test started");
      for Dummy in 1 .. 10 loop
        for Unused in 1 .. 100_000_000 loop
          S.Append ("*");
        end loop;
        IO.Put ('.');
      end loop;
      IO.New_Line;
      Log.Write ("Huge string test complete");
      IO.Put_Line ("Complete - Capacity:" & S.Capacity'image);
      IO.Put_Line ("Update huge string");
      S.Update (From => 987_654_315, By => P);
      S.Update (From => 987_654_316, By => P);
      S.Update (From => 987_654_320, By => P);
      S.Update (From => 987_654_321, By => P);
      IO.Put_Line ("Lowercase huge string");
      S.Make_Lowercase;
      IO.Put_Line ("Lowercase complete");
      IO.Put_Line ("Uppercase huge string");
      S.Make_Uppercase;
      IO.Put_Line ("Uppercase complete");
      IO.Put_Line ("Prepend spaces");
      S.Prepend ("                      ");
      IO.Put_Line ("Prepend complete");
      IO.Put_Line ("Append spaces");
      S.Append ("                      ");
      IO.Put_Line ("Append complete");
      IO.Put_Line ("Trim");
      S.Trim;
      IO.Put_Line ("Trim complete");
      IO.Put_Line ("Find Location");
      L := S.Index_Of (U);
      B := S.Index_Of (U, From => 1_000_000_000, The_Direction => Text.Backward);
      IO.Put_Line ("Location of " & U'image & " is" & L'image & Equal (L'image, B'image));
      IO.Put_Line ("Slice" & Equal (S.Slice (From => L, To => L + P'length - 1), U));
    end;

    declare
      Tests     : Text.String := ["TexXxx"];
      Completed : constant Text.String := ["Completed "];
      X : constant Text.String := [['s'], ['t']];
      Y : Text.String := [];
    begin
      Y := X;
      Tests(3) := Y.First;
      Tests(Tests.Index_Of ('X')) := Y.Last;
      Tests.Delete_Last;
      Tests(Tests.Count) := 's';
      Tests.Append (' ');
      IO.Put_Line ("Strings: " & Tests'image & Equal (Tests'image, """Tests """));
      IO.Put_Line ("");

      for The_Character of Tests loop
        IO.Put (The_Character);
      end loop;
      IO.Put_Line (Completed & (if Test_Ok then "successfully" else "with error"));
    end;
  exception
  when Occurrence: others =>
    IO.Put_Line (Exceptions.Information_Of (Occurrence));
  end Execute;

end Test;
