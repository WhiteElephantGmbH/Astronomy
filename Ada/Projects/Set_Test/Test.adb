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

  Word_Size : constant := 128;

  type Word is mod 2 ** Word_Size;

  type Element is range 0 .. Word_Size - 1;

  package Cardinal is new Discrete_Set (Element, Word);

  subtype Set is Cardinal.Set;

  type Color is (Blue, Green, Red);

  type Tree_Bits is mod 2 ** (Color'pos(Color'last) + 1);

  package Colors is new Discrete_Set (Color, Tree_Bits);

  subtype RGB is Colors.Set;

  procedure Execute is
    use type Set;
    use type RGB;
    A : Set := [0, 3, 17, 43, 64, 68, 127];
    B : Set := [0, 9, 12, 17] or 64 ** 68;
    C : RGB := [Blue];
  begin
    IO.Put_Line ("Set Test");
    IO.Put_Line (" ->" & A'image & " -" & Word'(Cardinal.From (A))'image);
    for Item of A loop
      IO.Put_Line ("   ->" & Item'image);
    end loop;
    Cardinal.Include (A, 42);
    Cardinal.Exclude (A, 43);
    Cardinal.Exclude (B, 17);
    IO.Put_Line ("A ->" & A'image);
    IO.Put_Line ("B ->" & B'image);
    IO.Put_Line ("A - B ->" & Set'(A - B)'image);
    IO.Put_Line ("A and B ->" & Set'(A and B)'image);
    IO.Put_Line ("A  or B ->" & Set'(A  or B)'image);
    IO.Put_Line ("A xor B ->" & Set'(A xor B)'image);
    IO.Put_Line ("All Set ->" & Set'(not Set'[])'image);
    Colors.Include (C, Green);
    IO.Put_Line ("Colours  ->" & C'image);
    IO.Put_Line ("C + Red ->" & RGB'(C + Red)'image);
    IO.Put_Line ("C - Blue ->" & RGB'(C - Blue)'image);
    IO.Put_Line ("Red < C -> " & Boolean'(Red < C)'image);
    IO.Put_Line ("Blue < C -> " & Boolean'(Blue < C)'image);
    IO.Put_Line ("[Blue, Green] < C -> " & Boolean'([Blue, Green] < C)'image);
    IO.Put_Line ("[] < C -> " & Boolean'([] < C)'image);
  exception
  when Occurrence: others =>
    IO.Put_Line (Exceptions.Information_Of (Occurrence));
  end Execute;

end Test;
