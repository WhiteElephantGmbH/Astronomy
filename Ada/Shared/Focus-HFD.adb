-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Numerics.Generic_Elementary_Functions;

package body Focus.HFD is

  package NF is new Ada.Numerics.Generic_Elementary_Functions (Float);

  Pi : constant Float := Ada.Numerics.Pi;

  use type Camera.Pixel;


  procedure Evaluate (Grid : Camera.Raw_Grid) is
    use type Camera.Raw_Grid;
    use type Camera.Rows;
    use type Camera.Columns;
  begin
    if Grid = [] then
      Set_Error ("No Grid - " & Camera.Error_Message);
      return;
    end if;
    declare
      subtype Huge_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'last;

      function Evaluated_Half_Flux return Camera.Pixel is
        Black_Level : constant := 0;
        The_Sum     : Huge_Natural := 0;
        The_Count   : Natural := 0;
      begin
        for Value of Grid loop
          if Value > Black_Level then
            The_Count := @ + 1;
            The_Sum := @ + Huge_Natural(Value);
          end if;
        end loop;
        if The_Count = 0 then
          return 0;
        end if;
        return Camera.Pixel (The_Sum / Huge_Natural(The_Count));
      end Evaluated_Half_Flux;

      Half_Flux : constant Camera.Pixel := Evaluated_Half_Flux;

      type Position is record
        Row    : Camera.Rows;
        Column : Camera.Columns;
      end record;

      type Right_Angle is record
        Edge : Position;
        Ends : Position;
        Size : Natural := 0;
      end record;

      function Right_Angle_At (Row    : Camera.Rows;
                               Column : Camera.Columns) return Right_Angle is
        The_Right_Angle : Right_Angle;
      begin
        The_Right_Angle.Edge := (Row    => Row,
                                 Column => Column);
        for The_Column in Column .. Grid'last(2) loop
          if Grid(Row, The_Column) > Half_Flux then
            The_Right_Angle.Ends.Column := The_Column;
            The_Right_Angle.Size := @ + 1;
          else
            exit;
          end if;
        end loop;
        for The_Row in Row .. Grid'last(1) loop
          if Grid(The_Row, Column) > Half_Flux then
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
        for Column in Grid'range(2) loop
          for Row in Grid'range(1) loop
            The_Right_Angle := Right_Angle_At (Row, Column);
            if The_Right_Angle.Size > Max_Right_Angle.Size then
              Max_Right_Angle := The_Right_Angle;
            end if;
          end loop;
        end loop;
        Log.Write ("Max Right Angle Size:" & Max_Right_Angle.Size'image);
        return Max_Right_Angle;
      end Evaluated_Max_Rigth_Angle;

      RA : constant Right_Angle := Evaluated_Max_Rigth_Angle;

      function Evaluated_Offset return Position is
      begin
        return (Row    => (Camera.Rows'first + RA.Ends.Row - RA.Edge.Row) / 2,
                Column => (Camera.Columns'first + RA.Ends.Column - RA.Edge.Column) / 2);
      exception
      when others =>
        return (Camera.Rows'first, Camera.Columns'first);
      end Evaluated_Offset;

      Offset : constant Position := Evaluated_Offset;

      function Is_In_RA (Row    : Camera.Rows;
                         Column : Camera.Columns) return Boolean is
      begin
        return Row    > RA.Edge.Row    and Row    < RA.Ends.Row and
               Column > RA.Edge.Column and Column < RA.Ends.Column;
       end Is_In_RA;

      function Evaluated_Half_Flux_Diameter (Center : out Position) return Diameter is
        First_Column : Camera.Columns := RA.Edge.Column;
        Last_Column  : Camera.Columns := RA.Ends.Column;
        First_Row    : Camera.Rows := RA.Edge.Row;
        Last_Row     : Camera.Rows := RA.Ends.Row;
        Column_Sum   : Huge_Natural := 0;
        Row_Sum      : Huge_Natural := 0;
        The_Count    : Huge_Natural := 0;
      begin
        if First_Column > Offset.Column then
          First_Column := (@ - Offset.Column);
        else
          First_Column := Camera.Columns'first;
        end if;
        if Last_Column < Grid'last(2) - Offset.Column then
          Last_Column := @ + Offset.Column;
        else
          Last_Column := Grid'last(2);
        end if;
        if First_Row > Offset.Row then
          First_Row := @ - Offset.Row;
        else
          First_Row := Camera.Rows'first;
        end if;
        if Last_Row < Grid'last(1) - Offset.Row then
          Last_Row := @ + Offset.Row;
        else
          Last_Row := Grid'last(1);
        end if;
        for The_Row in First_Row .. Last_Row loop
          for The_Column in First_Column .. Last_Column loop
            if Grid(The_Row, The_Column) > Half_Flux or else Is_In_RA (The_Row, The_Column) then
              Column_Sum := @ + Huge_Natural(The_Column);
              Row_Sum := @ + Huge_Natural(The_Row);
              The_Count := @ + 1;
            end if;
          end loop;
        end loop;
        Center := (Column => Camera.Columns (Column_Sum / The_Count),
                   Row    => Camera.Rows (Row_Sum / The_Count));
        return Diameter (2.0 * NF.Sqrt (Float(The_Count) / Pi));
      end Evaluated_Half_Flux_Diameter;

      The_Center : Position;

      Half_Flux_Diameter : constant Diameter := Evaluated_Half_Flux_Diameter (The_Center);

    begin
      Log.Write ("Half Flux:" & Half_Flux'image);
      Focus_Data.Set (Half_Flux);
      Log.Write ("Half Flux Diameter:" & Half_Flux_Diameter'image);
      Log.Write ("Center - Row:" & The_Center.Row'image & " - Column:" & The_Center.Column'image);
      Focus_Data.Set (Half_Flux_Diameter);
    end;
  exception
  when Occurrence: others =>
    Focus_Data.Set_Fatal (Occurrence);
  end Evaluate;

end Focus.HFD;
