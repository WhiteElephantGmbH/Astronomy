-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Discrete_Set;
with Exceptions;

package body Test is

  package IO renames Ada.Text_IO;

  Test_Ok : Boolean := True;


  type Element is range 0 .. 255;

  package Cardinal is new Discrete_Set (Element);

  subtype Set is Cardinal.Set;

  use type Set;

  function Check_Set (A, R : Set) return String is
  begin
    if A = R then
      return " -> Ok";
    else
      Test_Ok := False;
      return " -> Error";
    end if;
  end Check_Set;


  type Color is (Blue, Green, Red);

  package Colors is new Discrete_Set (Color);

  subtype RGB is Colors.Set;

  use type RGB;

  function Check_RGB (A, R : RGB) return String is
  begin
    if A = R then
      return " -> Ok";
    else
      Test_Ok := False;
      return " -> Error";
    end if;
  end Check_RGB;


  procedure Execute is
    A : Set := [0, 3, 17, 43, 64, 68, 127];
    B : Set := [0, 9, 12, 17] or 64 ** 68;
    C : RGB := [Blue];
  begin
    IO.Put_Line ("Set Test");

    IO.Put_Line (" ->" & A'image);
    for Item of A loop
      IO.Put_Line ("   ->" & Item'image);
    end loop;
    Cardinal.Include (A, 42);
    Cardinal.Exclude (A, 43);
    A := @ or 23 ** 25;
    B := @ - 66;
    B := @ + 17;
    IO.Put_Line ("A -> " & A'image                    & Check_Set (A, [0, 3, 17, 23, 24, 25, 42, 64, 68, 127]));
    IO.Put_Line ("B -> " & B'image                    & Check_Set (B, [0, 9, 12, 17, 64, 65, 67, 68]));
    IO.Put_Line ("A - B -> " & Set'(A - B)'image      & Check_Set (A - B, [3, 23, 24, 25, 42, 127]));
    IO.Put_Line ("A and B -> " & Set'(A and B)'image  & Check_Set (A and B, [0, 17, 64, 68]));
    IO.Put_Line ("A  or B -> " & Set'(A  or B)'image);
    IO.Put_Line ("A xor B -> " & Set'(A xor B)'image & Check_Set (A xor B, [3, 9, 12, 23, 24, 25, 42, 65, 67, 127]));
    IO.Put_Line ("All Set -> " & Set'(not Set'[])'image);
    IO.Put_Line ("");

    Colors.Include (C, Green);
    IO.Put_Line ("Colours  -> " & C'image                    & Check_RGB (C, [Blue, Green]));
    IO.Put_Line ("Colours + Red -> " & RGB'(C + Red)'image   & Check_RGB (C + Red, [Blue, Green, Red]));
    IO.Put_Line ("Colours - Blue -> " & RGB'(C - Blue)'image & Check_RGB (C - Blue, [Green]));
    IO.Put_Line ("Red < Colours -> " & Boolean'(Red < C)'image);
    IO.Put_Line ("Blue < Colours -> " & Boolean'(Blue < C)'image);
    IO.Put_Line ("[Blue, Green] < Colours -> " & Boolean'([Blue, Green] < C)'image);
    IO.Put_Line ("[] < Colours -> " & Boolean'([] < C)'image);
    IO.Put_Line ("");

    IO.Put_Line ("Test Complete : " & Test_Ok'image);
  exception
  when Occurrence: others =>
    IO.Put_Line (Exceptions.Information_Of (Occurrence));
  end Execute;

end Test;
