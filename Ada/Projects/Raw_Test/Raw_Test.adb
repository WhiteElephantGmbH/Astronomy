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
with Ada.Numerics.Generic_Elementary_Functions;

procedure Raw_Test is

  package IO renames Ada.Text_IO;
  package NF is new Ada.Numerics.Generic_Elementary_Functions (Float);

  Pi : constant Float := Ada.Numerics.Pi;

  use type Raw.Pixel;

begin
  IO.Put_Line ("Raw Test");
  IO.Put_Line ("========");
  declare
    subtype Huge_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'last;

    Last_Index : constant := 1024;

    Green_Grid : constant Raw.Green_Grid := Raw.Grid_Of ("D:\Temp\Picture.CR2", Last_Index);

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
      Black_Level : constant := 0;
      The_Sum     : Huge_Natural := 0;
      The_Count   : Natural := 0;
    begin
      for Value of Green_Grid loop
        if Value > Black_Level then
          The_Count := @ + 1;
          The_Sum := @ + Huge_Natural(Value);
        end if;
      end loop;
      if The_Count = 0 then
        return 0;
      end if;
      return Raw.Pixel (The_Sum / Huge_Natural(The_Count));
    end Evaluated_Half_Flux;

    Half_Flux : constant Raw.Pixel := Evaluated_Half_Flux;

    type Right_Angle is record
      Edge : Raw.Position;
      Ends : Raw.Position;
      Size : Natural := 0;
    end record;

    function Right_Angle_At (Row    : Raw.Rows;
                             Column : Raw.Columns) return Right_Angle is
      The_Right_Angle : Right_Angle;
    begin
      The_Right_Angle.Edge := (Row    => Row,
                               Column => Column);
      for The_Column in Column .. Green_Grid'last(2) loop
        if Green_Grid(Row, The_Column) > Half_Flux then
          The_Right_Angle.Ends.Column := The_Column;
          The_Right_Angle.Size := @ + 1;
        else
          exit;
        end if;
      end loop;
      for The_Row in Row .. Green_Grid'last(1) loop
        if Green_Grid(The_Row, Column) > Half_Flux then
          The_Right_Angle.Ends.Row := The_Row;
          The_Right_Angle.Size := @ + 1;
        else
          exit;
        end if;
      end loop;
      return The_Right_Angle;
    end Right_Angle_At;

    function Evaluated_Max_Rigth_Angle return Right_Angle is
      The_Right_Angle : Right_Angle;
      Max_Right_Angle : Right_Angle;
    begin
      for Column in Green_Grid'range(2) loop
        for Row in Green_Grid'range(1) loop
          The_Right_Angle := Right_Angle_At (Row, Column);
          if The_Right_Angle.Size > Max_Right_Angle.Size then
            Max_Right_Angle := The_Right_Angle;
          end if;
        end loop;
      end loop;
      return Max_Right_Angle;
    end Evaluated_Max_Rigth_Angle;

    use type Raw.Rows;
    use type Raw.Columns;

    RA            : constant Right_Angle := Evaluated_Max_Rigth_Angle;
    Row_Offset    : constant Raw.Rows := (RA.Ends.Row - RA.Edge.Row) / 2;
    Column_Offset : constant Raw.Columns := (RA.Ends.Column - RA.Edge.Column) / 2;

    function Is_In_RA (Row    : Raw.Rows;
                       Column : Raw.Columns) return Boolean is
    begin
      return Row    > RA.Edge.Row    and Row    < RA.Ends.Row and
             Column > RA.Edge.Column and Column < RA.Ends.Column;
     end Is_In_RA;

    function Evaluated_Half_Flux_Diameter (Center : out Raw.Position) return Natural is
      First_Column : Raw.Columns := RA.Edge.Column;
      Last_Column  : Raw.Columns := RA.Ends.Column;
      First_Row    : Raw.Rows := RA.Edge.Row;
      Last_Row     : Raw.Rows := RA.Ends.Row;
      Column_Sum   : Huge_Natural := 0;
      Row_Sum      : Huge_Natural := 0;
      The_Count    : Huge_Natural := 0;
    begin
      if First_Column > Column_Offset then
        First_Column := @ - Column_Offset;
      else
        First_Column := Raw.Columns'first;
      end if;
      if Last_Column < Green_Grid'last(2) - Column_Offset then
        Last_Column := @ + Column_Offset;
      else
        Last_Column := Green_Grid'last(2);
      end if;
      if First_Row > Row_Offset then
        First_Row := @ - Row_Offset;
      else
        First_Row := Raw.Rows'first;
      end if;
      if Last_Row < Green_Grid'last(1) - Row_Offset then
        Last_Row := @ + Row_Offset;
      else
        Last_Row := Green_Grid'last(1);
      end if;
      for The_Row in First_Row .. Last_Row loop
        for The_Column in First_Column .. Last_Column loop
          if Green_Grid(The_Row, The_Column) > Half_Flux or else Is_In_RA (The_Row, The_Column) then
            Column_Sum := @ + Huge_Natural(The_Column);
            Row_Sum := @ + Huge_Natural(The_Row);
            The_Count := @ + 1;
          end if;
        end loop;
      end loop;
      Center := (Column => Raw.Columns (Column_Sum / The_Count),
                 Row    => Raw.Rows (Row_Sum / The_Count));
      return Natural (2.0 * NF.Sqrt (Float(The_Count) / Pi));
    end Evaluated_Half_Flux_Diameter;

    The_Center : Raw.Position;

    Half_Flux_Diameter : constant Natural := Evaluated_Half_Flux_Diameter (The_Center);

  begin
    IO.New_Line;
    if Natural(Green_Grid'last(1)) <= 20 then
      IO.Put_Line ("Green Grid:");
      Show_Green_Grid;
    end if;
    IO.Put_Line ("Half Flux:" & Half_Flux'image);
    IO.Put_Line ("Center Position:" & The_Center'image);
    IO.Put_Line ("Half Flux Diameter:" & Half_Flux_Diameter'image);
  end;

exception
when Item: others =>
  Ada.Text_IO.Put_Line (Exceptions.Information_Of (Item));
end Raw_Test;
