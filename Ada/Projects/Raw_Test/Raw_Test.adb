-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

pragma Build (Description => "Raw Test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Raw;
with Exceptions;

procedure Raw_Test is

  package IO renames Ada.Text_IO;

  use type Raw.Pixel;

begin
  IO.Put_Line ("Raw Test");
  IO.Put_Line ("========");
  declare
    Green_Grid : constant Raw.Green_Grid := Raw.Grid_Of ("First_Picture.CR2", 1024);

    procedure Show_Green_Grid is
    begin
      for Row in Green_Grid'range(1) loop
        declare
          Row_Image : constant String := "  " & Row'image;
        begin
          IO.Put (Row_Image(Row_Image'last - 3 .. Row_Image'last) & ":");
          for Column in Green_Grid'range(2) loop
            declare
              Column_Image : constant String := "     " & Green_Grid(Row, Column)'image;
            begin
              IO.Put (Column_Image(Column_Image'last - 5 .. Column_Image'last));
            end;
          end loop;
          IO.New_Line;
        end;
      end loop;
    end Show_Green_Grid;


    function Evaluated_Half_Flux return Raw.Pixel is
      Black_Level : constant := 100;
      The_Sum     : Long_Long_Integer := 0;
      The_Count   : Natural := 0;
    begin
      for Value of Green_Grid loop
        if Value > Black_Level then
          The_Count := @ + 1;
          The_Sum := @ + Long_Long_Integer(Value);
        end if;
      end loop;
      if The_Count = 0 then
        return 0;
      end if;
      return Raw.Pixel (The_Sum / Long_Long_Integer(The_Count));
    end Evaluated_Half_Flux;

    Half_Flux : constant Raw.Pixel := Evaluated_Half_Flux;

    Max_Missings : constant := 10;

    type Column_Sequence is record
      At_Row : Raw.Rows    := Raw.Rows'first;
      First  : Raw.Columns := Raw.Columns'first;
      Count  : Natural     := 0;
    end record;

    type Column_Sequences is array (Positive range <>) of Column_Sequence;

    function Evaluated_Column_Sequences return Column_Sequences is
      The_Sequence  : Column_Sequence;
      The_Sequences : Column_Sequences(1..2);
    begin
      for Row in Green_Grid'range(1) loop
        declare
          The_Actual : Column_Sequence;
          The_Count  : Natural := Max_Missings;
        begin
          The_Actual.At_Row := Row;
          for Column in Green_Grid'range(2) loop
            declare
              Value : constant Raw.Pixel := Green_Grid(Row, Column);
            begin
              if Value > Half_Flux then
                if The_Actual.Count = 0 then
                  The_Actual.First := Column;
                end if;
                The_Actual.Count := @ + 1;
              elsif The_Count = 0 then
                The_Count := Max_Missings;
                if The_Actual.Count > 0 then
                  if The_Actual.Count > The_Sequence.Count then
                    The_Sequence := The_Actual;
                  end if;
                end if;
                The_Actual.Count := 0;
              else
                The_Count := @ - 1;
                The_Actual.Count := @ + 1;
              end if;
            end;
          end loop;
        end;
        if The_Sequences(2).Count < The_Sequence.Count then
          if The_Sequences(2).Count > The_Sequences(1).Count then
            The_Sequences(1) := The_Sequences(2);
          end if;
          The_Sequences(2) := The_Sequence;
        elsif The_Sequences(1).Count < The_Sequence.Count then
          The_Sequences(1) := The_Sequences(2);
          The_Sequences(2) := The_Sequence;
        end if;
        The_Sequence.Count := 0;
      end loop;
      return The_Sequences;
    end Evaluated_Column_Sequences;

    Max_Column_Sequences : constant Column_Sequences := Evaluated_Column_Sequences;

    type Row_Sequence is record
      At_Column : Raw.Columns := Raw.Columns'first;
      First     : Raw.Rows    := Raw.Rows'first;
      Count     : Natural     := 0;
    end record;

    type Row_Sequences is array (Positive range <>) of Row_Sequence;

    function Evaluated_Row_Sequences return Row_Sequences is
      The_Sequence  : Row_Sequence;
      The_Sequences : Row_Sequences(1..2);
    begin
      for Column in Green_Grid'range(2) loop
        declare
          The_Actual : Row_Sequence;
          The_Count  : Natural := Max_Missings;
        begin
          The_Actual.At_Column := Column;
          for Row in Green_Grid'range(1) loop
            declare
              Value : constant Raw.Pixel := Green_Grid(Row, Column);
            begin
              if Value > Half_Flux then
                if The_Actual.Count = 0 then
                  The_Actual.First := Row;
                end if;
                The_Actual.Count := @ + 1;
              elsif The_Count = 0 then
                The_Count := Max_Missings;
                if The_Actual.Count > 0 then
                  if The_Actual.Count > The_Sequence.Count then
                    The_Sequence := The_Actual;
                  end if;
                end if;
                The_Actual.Count := 0;
              else
                The_Count := @ - 1;
                The_Actual.Count := @ + 1;
              end if;
            end;
          end loop;
        end;
        if The_Sequences(2).Count < The_Sequence.Count then
          if The_Sequences(2).Count > The_Sequences(1).Count then
            The_Sequences(1) := The_Sequences(2);
          end if;
          The_Sequences(2) := The_Sequence;
        elsif The_Sequences(1).Count < The_Sequence.Count then
          The_Sequences(1) := The_Sequences(2);
          The_Sequences(2) := The_Sequence;
        end if;
        The_Sequence.Count := 0;
      end loop;
      return The_Sequences;
    end Evaluated_Row_Sequences;

    Max_Row_Sequences : constant Row_Sequences := Evaluated_Row_Sequences;

  begin
    IO.New_Line;
    IO.Put_Line ("Green Grid:");
    if Natural(Green_Grid'last(1)) <= 20 then
      Show_Green_Grid;
    end if;
    IO.Put_Line ("Half_Flux:" & Half_Flux'image);
    IO.Put_Line ("Column Sequence:" & Max_Column_Sequences'image);
    IO.Put_Line ("Row Sequence:" & Max_Row_Sequences'image);
  end;
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Exceptions.Information_Of (Item));
end Raw_Test;
