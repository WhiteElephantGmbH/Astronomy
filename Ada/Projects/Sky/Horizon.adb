-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Alpha;
with Angle;
with Earth;
with Error;
with Sky_Line;
with Stellarium;
with Traces;

package body Horizon is

  package Log is new Traces ("Horizon");


  procedure Generate is

    Filename : constant String := Stellarium.Landscape_Filename;

    Rotation : constant Angle.Degrees := Stellarium.Landscape_Rotation;

    Altitude_Limit : constant Angle.Degrees := 2.0;

    procedure Build_Sky_Line is

      Alpha_Limits : constant Alpha.Limits := Alpha.Limits_Of (Filename);

      use type Alpha.Column;
      use type Alpha.Row;
      use type Angle.Degrees;

      Height : constant Alpha.Row := Alpha.Last_Row + 1;
      Width  : constant Alpha.Column := Alpha_Limits'last + 1;

      Column_Offset : constant Alpha.Column
        := (Width / 4 + Alpha.Column((Rotation + 360.0) * Angle.Degrees(Width)) / 360) mod Width;

      function Column_Angle (Column : Alpha.Column) return Angle.Degrees is
      begin
        return Angle.Degrees(((Column + Column_Offset) mod Width) * 360) / Angle.Degrees(Width);
      end Column_Angle;

      function Row_Angle (Row : Alpha.Row) return Angle.Degrees is
      begin
        return Angle.Degrees(Row * 90) / Angle.Degrees(Height);
      end Row_Angle;

      procedure Process_Row_Of (Column : Alpha.Column)is
        The_Altitude : Angle.Degrees := Row_Angle (Alpha_Limits(Column).Lower);
        Top_Altitude : Angle.Degrees := Row_Angle (Alpha_Limits(Column).Upper);
        use type Angle.Value;
      begin
        if The_Altitude < Altitude_Limit then
          The_Altitude := Altitude_Limit;
        end if;
        if Top_Altitude < Altitude_Limit then
          Top_Altitude := Altitude_Limit;
        end if;
        if Top_Altitude = The_Altitude then
          Top_Altitude := +Sky_Line.No_Top_Altitude;
        end if;
        Sky_Line.Append (Direction => Earth.Direction_Of (Az  => +Column_Angle (Column),
                                                          Alt => +The_Altitude),
                         Top_Alt => +Top_Altitude);
      end Process_Row_Of;

    begin -- Build_Sky_Line
      Sky_Line.Create;
      for Column in Width - Column_Offset .. Width - 1 loop
        Process_Row_Of (Column);
      end loop;
      for Column in 0 .. Width - Column_Offset - 1 loop
        Process_Row_Of (Column);
      end loop;
      Sky_Line.Close;
    end Build_Sky_Line;

    type Rotation_Angle is delta 0.001 range -360.0 .. 360.0;

  begin -- Generate
    Sky_Line.Clear;
    Log.Write ("Generate from file " & Filename & " (Rotation =" & Rotation_Angle(Rotation)'img & ")");
    Build_Sky_Line;
    Log.Write ("Generation complete");
  exception
  when Alpha.File_Not_Found =>
    Log.Warning ("No horizon generated");
  when Alpha.Unknown_File =>
    Error.Raise_With ("Filetype not supported for " & Filename);
  when Occurence: others =>
    Log.Termination (Occurence);
    Error.Raise_With ("Generation failed");
  end Generate;

end Horizon;
