-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Astro;
with Error;
with Persistent_Definite_Doubly_Linked_Lists;
with Traces;

package body Matrix is

  package Log is new Traces ("Matrix");

  Minimum_Data_Lists_Length : constant := 5;

  Angle_Index_Bits : constant := 9;

  Index_Count : constant := 2 ** Angle_Index_Bits;

  type Angle_Index is mod Index_Count;

  use type Angle.Unsigned;

  Angle_Increment : constant Angle.Unsigned := Angle.Unsigned(2 ** (32 - Angle_Index_Bits));
  Angle_Mask      : constant Angle.Unsigned := not (Angle_Increment - 1);

  type Az_Data is array (Angle_Index) of Offsets;

  package Corrections is new Definite_Doubly_Linked_Lists (Az_Data);

  package Persistent_Corrections is new Persistent_Definite_Doubly_Linked_Lists ("Matrix", Az_Data, Corrections);

  The_Persistent_Data : Persistent_Corrections.Data;

  The_Correction_List : Corrections.Item renames The_Persistent_Data.List;

  type Correction_Information is array (Angle_Index, Angle_Index) of Offsets;

  type Correction_Offsets is array (Boolean) of Correction_Information;

  The_Corrections : Correction_Offsets;


  function Calculated_Offsets_Available return Boolean is
    Alt_Index  : Angle_Index := Angle_Index'first;
    Is_Inverse : Boolean := False;
  begin
    if The_Correction_List.Count /= (2 * Index_Count) then
      return False;
    end if;
    for Az_Offsets of The_Correction_List loop
      for Az_Index in Angle_Index loop
        The_Corrections(Is_Inverse)(Alt_Index, Az_Index) := Az_Offsets(Az_Index);
      end loop;
      Alt_Index := Alt_Index + 1;
      if Alt_Index = Angle_Index'first then
        Is_Inverse := True;
      end if;
    end loop;
    Log.Write ("offset data ready");
    return True;
  end Calculated_Offsets_Available;


  Offsets_Are_Available : Boolean := Calculated_Offsets_Available;
  The_Actual_Offset     : Earth.Direction := Earth.Zero_Direction;


  function Is_Available return Boolean is
  begin
    return Offsets_Are_Available;
  end Is_Available;


  procedure Clear is
  begin
    Log.Write ("Clear");
    The_Correction_List.Clear;
    Offsets_Are_Available := False;
    The_Actual_Offset := Earth.Zero_Direction;
  end Clear;


  procedure Clear (The_Information : in out Data_Lists) is
  begin
    for The_Data_List of The_Information loop
      Matrix.List.Clear (The_Data_List);
    end loop;
  end Clear;


  function Is_Empty (The_Information : Data_Lists) return Boolean is
  begin
    for The_Data_List of The_Information loop
      if not The_Data_List.Is_Empty then
        return False;
      end if;
    end loop;
    return True;
  end Is_Empty;


  function Is_Ready (The_Information : Data_Lists) return Boolean is
  begin
    if Is_Empty (The_Information) then
      return False;
    end if;
    for The_Data_List of The_Information loop
      if not The_Data_List.Is_Empty and then Natural(The_Data_List.Length) < Minimum_Data_Lists_Length then
        return False;
      end if;
    end loop;
    return True;
  end Is_Ready;


  procedure Calculate (The_Information : in out Data_Lists) is

    type Data_Array is array (List.Index range <>) of Data;


    procedure Build_Matrix (The_Data : Data_Array) is

      subtype Index is Natural range The_Data'range;

      type Index_Array is array (Index) of Index;

      function Index_Array_Of_Sorted_Az return Index_Array is

        package Index_List is new Definite_Doubly_Linked_Lists (Index);

        function Az_Is_Less_Than (Left, Right : Index) return Boolean is
        begin
          return The_Data(Left).Az < The_Data(Right).Az;
        end Az_Is_Less_Than;

        package Az_Sorting is new Index_List.Generic_Sorting (Az_Is_Less_Than);

        The_Index_List : Index_List.Item;

        use type Index_List.Item;

      begin -- Index_Array_Of_Sorted_Az
        for The_Index in Index loop
          The_Index_List := The_Index_List + The_Index;
        end loop;
        Az_Sorting.Sort (The_Index_List);
        return Index_Array(Index_List.Elements(The_Index_List));
      end Index_Array_Of_Sorted_Az;

      Index_Of_Sorted_Az : constant Index_Array := Index_Array_Of_Sorted_Az;

      Epsilon : constant := 0.5;


      package Modular is

        type Index_Range is private;

        function "+" (Item : Index) return Index_Range with Inline;

        function "+" (Item : Index_Range) return Index with Inline;

        function "+" (Left  : Index_Range;
                      Right : Positive) return Index_Range with Inline;

        function "-" (Left  : Index_Range;
                      Right : Positive) return Index_Range with Inline;

      private
        type Index_Range is new Index;
      end Modular;


      package body Modular is

        function "+" (Item : Index) return Index_Range is
        begin
          return Index_Range(Item);
        end "+";

        function "+" (Item : Index_Range) return Index is
        begin
          return Index(Item);
        end "+";

        function "+" (Left  : Index_Range;
                      Right : Positive) return Index_Range is
        begin
          if (Natural(Left) + Right) <= Natural(Index'last) then
            return Left + Index_Range(Right);
          else
            return Left - Index_Range(Natural(Index'last - Index'first + 1) - Right);
          end if;
        end "+";

        function "-" (Left  : Index_Range;
                      Right : Positive) return Index_Range is
        begin
          if Integer(Left) - Right >= Natural(Index'first) then
            return Left - Index_Range(Right);
          else
            return Left + Index_Range(Natural(Index'last - Index'first + 1) - Right);
          end if;
        end "-";

      end Modular;


      function Distance_Between (Alt_1 : Angle.Unsigned;
                                 Az_1  : Angle.Unsigned;
                                 Alt_2 : Angle.Unsigned;
                                 Az_2  : Angle.Unsigned) return Angle.Degrees is
        A1 : constant Angle.Degrees := +Az_1;
        B1 : constant Angle.Degrees := +Alt_1;
        A2 : constant Angle.Degrees := +Az_2;
        B2 : constant Angle.Degrees := +Alt_2;
        use Astro;
        use MATLIB;
      begin
        return 2.0 * ASN (SQRT ((1.0 - CS(A1 - A2) * CS (B1) * CS (B2) - SN (B1) * SN (B2)) / 2.0));
      end Distance_Between;


      type Neighbour_Range is range 1..Minimum_Data_Lists_Length;

      type Neighbours is array (Neighbour_Range) of Natural;

      type Distances is array (Neighbour_Range) of Angle.Degrees;

      type Neighbour_Offsets is array (Neighbour_Range) of Angle.Signed;


      Next_Alt_Index : Index;
      Next_Az_Index  : Index;

      procedure Find_Neighbours (Alt, Az             :     Angle.Unsigned;
                                 The_Neighbours      : out Neighbours;
                                 The_Distances       : out Distances) is

        Is_Neighbour : array (Index) of Boolean := (others => False);

        function Check_If_Neigbour (The_Index : Index) return Boolean is
        begin
          if Is_Neighbour (The_Index) then
            return True;
          end if;
          declare
            The_Distance : constant Angle.Degrees := Distance_Between (Alt_1 => The_Data(The_Index).Alt,
                                                                       Az_1  => The_Data(The_Index).Az,
                                                                       Alt_2 => Alt,
                                                                       Az_2  => Az);
            use type Angle.Degrees;
          begin
            for N in Neighbour_Range loop
              if The_Distance < The_Distances(N) then
                for I in reverse N + 1 .. Neighbour_Range'last loop
                  The_Distances(I) := The_Distances(I - 1);
                  The_Neighbours(I) := The_Neighbours(I - 1);
                end loop;
                The_Distances(N) := The_Distance;
                The_Neighbours(N) := The_Index;
                Is_Neighbour(The_Index) := True;
                return True;
              end if;
            end loop;
          end;
          return True;
        end Check_If_Neigbour;

        Next_Index : Modular.Index_Range;
        The_Count  : Natural;

        use type Modular.Index_Range;

      begin -- Find_Neighbours
        The_Distances := (others => Angle.Degrees'last);
        Next_Index := +Next_Alt_Index;
        The_Count := 0;
        while The_Count < Natural(Neighbour_Range'last) loop
          if Check_If_Neigbour (+Next_Index) then
            The_Count := The_Count + 1;
          end if;
          Next_Index := Next_Index - 1;
        end loop;
        Next_Index := +Next_Alt_Index;
        The_Count := 0;
        while The_Count < Natural(Neighbour_Range'last) loop
          Next_Index := Next_Index + 1;
          if Check_If_Neigbour (+Next_Index) then
            The_Count := The_Count + 1;
          end if;
        end loop;

        Next_Index := +Next_Az_Index;
        The_Count := 0;
        while The_Count < Natural(Neighbour_Range'last) loop
          if Check_If_Neigbour (Index_Of_Sorted_Az(+Next_Index)) then
            The_Count := The_Count + 1;
          end if;
          Next_Index := Next_Index - 1;
        end loop;
        Next_Index := +Next_Az_Index;
        The_Count := 0;
        while The_Count < Natural(Neighbour_Range'last) loop
          Next_Index := Next_Index + 1;
          if Check_If_Neigbour (Index_Of_Sorted_Az(+Next_Index)) then
            The_Count := The_Count + 1;
          end if;
        end loop;
      end Find_Neighbours;

      use type Corrections.Item;

    begin --Build_Matrix
      if The_Data'length = 0 then
        for Unused_Index in Angle_Index loop
          The_Correction_List := The_Correction_List + Az_Data'(others => Offsets'(others => <>));
        end loop;
        Log.Write ("no data for build");
        return;
      elsif The_Data'length < Neighbour_Range'last then
        Error.Raise_With ("Alignment - less then" & Neighbour_Range'last'img & " data records");
      end if;
      Log.Write ("build");
      Next_Alt_Index := Index'first;
      for Alt_Angle_Index in Angle_Index loop
        declare
          Alt_Angle   : constant Angle.Unsigned := Angle.Unsigned(Alt_Angle_Index) * Angle_Increment;
          The_Az_Data : Az_Data;
        begin
          if Next_Alt_Index < Index'last then
            if Alt_Angle >= The_Data(Next_Alt_Index + 1).Alt then
              Next_Alt_Index := Next_Alt_Index + 1;
            end if;
          end if;
          Next_Az_Index  := Index'first;
          for Az_Angle_Index in Angle_Index loop
            declare
              Offset    : Offsets renames The_Az_Data(Az_Angle_Index);
              Az_Angle  : constant Angle.Unsigned := Angle.Unsigned(Az_Angle_Index) * Angle_Increment;
            begin
              if Next_Az_Index < Index'last then
                if Az_Angle >= The_Data(Index_Of_Sorted_Az (Next_Az_Index + 1)).Az then
                  Next_Az_Index := Next_Az_Index + 1;
                end if;
              end if;
              declare
                The_Neighbours : Neighbours;
                The_Distances  : Distances;

                function Offset_Of (The_Offsets : Neighbour_Offsets) return Angle.Signed is
                  use type Astro.REAL;
                  S, D : Astro.REAL := 0.0;
                begin
                  for N in Neighbour_Range loop
                    if The_Distances(N) < Epsilon then
                      return The_Offsets(N);
                    end if;
                    declare
                      V : constant Astro.REAL := Astro.REAL(The_Offsets(N));
                      F : constant Astro.REAL := 1.0 / The_Distances(N);
                    begin
                      S := S + V * F;
                      D := D + F;
                    end;
                  end loop;
                  return Angle.Signed(S / D);
                end Offset_Of;

              begin
                Find_Neighbours (Alt_Angle, Az_Angle, The_Neighbours, The_Distances);
                declare
                  The_Offsets : Neighbour_Offsets;
                begin
                  for N in Neighbour_Range loop
                    The_Offsets(N) := The_Data(The_Neighbours(N)).Offset.Alt;
                  end loop;
                  Offset.Alt := Offset_Of (The_Offsets);
                  for N in Neighbour_Range loop
                    The_Offsets(N) := The_Data(The_Neighbours(N)).Offset.Az;
                  end loop;
                  Offset.Az := Offset_Of (The_Offsets);
                end;
              end;
            end;
          end loop;
          The_Correction_List := The_Correction_List + The_Az_Data;
        end;
      end loop;
    end Build_Matrix;


    function Alt_Is_Less_Than (Left, Right : Data) return Boolean is
    begin
      return Left.Alt < Right.Alt;
    end Alt_Is_Less_Than;

    package Alt_Sorting is new List.Generic_Sorting (Alt_Is_Less_Than);

  begin -- Calculate
    for Index in The_Information'range loop
      Alt_Sorting.Sort (The_Information(Index));
      Build_Matrix (Data_Array(List.Elements(The_Information(Index))));
    end loop;
    Offsets_Are_Available := Calculated_Offsets_Available;
  exception
  when Error.Occurred =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Calculate;


  procedure Correct (Alt, Az    : in out Angle.Value;
                     Is_Inverse :        Boolean) is

    The_Data : Correction_Information renames The_Corrections (Is_Inverse);

  begin
    if not Offsets_Are_Available then
      return;
    end if;
    declare
      use type Angle.Value;

      Alt_Angle   : constant Angle.Unsigned := +Alt;
      Alt_Angle_1 : constant Angle.Unsigned := Alt_Angle and Angle_Mask;
      Alt_Angle_2 : constant Angle.Unsigned := Alt_Angle_1 + Angle_Increment;
      Alt_Index_1 : constant Angle_Index    := Angle_Index(Alt_Angle_1 / Angle_Increment);
      Alt_Index_2 : constant Angle_Index    := Angle_Index(Alt_Angle_2 / Angle_Increment);

      Az_Angle   : constant Angle.Unsigned := +(Az + Angle.Semi_Circle);
      Az_Angle_1 : constant Angle.Unsigned := Az_Angle and Angle_Mask;
      Az_Angle_2 : constant Angle.Unsigned := Az_Angle_1 + Angle_Increment;
      Az_Index_1 : constant Angle_Index    := Angle_Index(Az_Angle_1 / Angle_Increment);
      Az_Index_2 : constant Angle_Index    := Angle_Index(Az_Angle_2 / Angle_Increment);

      Alt_Offset : Angle.Signed;
      Az_Offset  : Angle.Signed;
      use type Angle.Signed;
    begin
      Alt_Offset := Angle.Interpolation_Of (A  => Alt_Angle,
                                            A1 => Alt_Angle_1,
                                            A2 => Alt_Angle_2,
                                            V1 => The_Data(Alt_Index_1, Az_Index_1).Alt,
                                            V2 => The_Data(Alt_Index_2, Az_Index_2).Alt);
      Az_Offset := Angle.Interpolation_Of (A  => Az_Angle,
                                           A1 => Az_Angle_1,
                                           A2 => Az_Angle_2,
                                           V1 => The_Data(Alt_Index_1, Az_Index_1).Az,
                                           V2 => The_Data(Alt_Index_2, Az_Index_2).Az);
      Alt := Alt + Alt_Offset;
      Az := Az + Az_Offset;
      The_Actual_Offset := Earth.Direction_Of (Az => +Az_Offset, Alt => +Alt_Offset, Inv => Is_Inverse);
    end;
  end Correct;


  function Actual_Offset return Earth.Direction is
  begin
    return The_Actual_Offset;
  end Actual_Offset;

end Matrix;
