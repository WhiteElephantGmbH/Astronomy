-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

package body Random is

-- Random Algorithm: Additive congruential method (D.Knuth)

  Maximum_Value : constant := 16#FFFF#;

  type Value_Kind is range 0 .. Maximum_Value;

  Max_Count : constant := 54;

  subtype Index is Natural range 0 .. Max_Count;

  type Value_Array is array (Index) of Value_Kind;

  Old_Array, The_Array                       : Value_Array;
  Old_Index_1, Old_Index_2, Index_1, Index_2 : Index;
  Old_Seed                                   : Value_Kind;


  function Next return Natural is
  begin
    The_Array(Index_2) := (The_Array(Index_2) + The_Array(Index_1)) mod 16#10000#;
    if Index_2 = 0 then
      Index_2 := Max_Count;
    else
      Index_2 := Index_2 - 1;
    end if;
    if Index_1 = 0 then
      Index_1 := Max_Count;
    else
      Index_1 := Index_1 - 1;
    end if;
    return Natural(The_Array(Index_2));
  end Next;


  procedure Initialize (The_Seed : Natural) is
    Dummy : Natural;
  begin
    if Value_Kind(The_Seed) /= Old_Seed then
      Index_1 := 24;
      Index_2 := 0;
      for The_Index in Index'range loop
        The_Array(The_Index) := 0;
      end loop;
      The_Array(0) := (31415 + Value_Kind(The_Seed)) mod 16#10000#;
      if The_Array(0) = 0 then
        The_Array(0) := 31415;
      end if;
      for The_Index in 0 .. 1999 loop
        pragma Unreferenced (The_Index);
        Dummy := Next;
      end loop;
      Old_Index_1 := Index_1;
      Old_Index_2 := Index_2;
      Old_Array := The_Array;
      Old_Seed := Value_Kind(The_Seed);
    else
      Index_1 := Old_Index_1;
      Index_2 := Old_Index_2;
      The_Array := Old_Array;
    end if;
  end Initialize;


  function Value (The_Maximum : Natural) return Natural is
  begin
    if Value_Kind(The_Maximum) = Maximum_Value then
      return Next;
    else
      return (Next * (The_Maximum + 1)) / 16#10000#;
    end if;
  end Value;

begin
  Old_Seed := Maximum_Value;
end Random;

