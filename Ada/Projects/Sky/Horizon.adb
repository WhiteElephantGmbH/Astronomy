-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;
with Earth;
with Error;
with Png;
with Sky_Line;
with Stellarium;
with Traces;

package body Horizon is

  package Log is new Traces ("Horizon");


  procedure Generate is

    Filename : constant String := Stellarium.Landscape_Filename;
    The_File : Png.File;

    procedure Build_Sky_Line is

      Width       : constant Png.Dimension        := Png.Width (The_File);
      Height      : constant Png.Dimension        := Png.Height (The_File);
      Bit_Depth   : constant Png.Depth            := Png.Bit_Depth (The_File);
      Colour_Type : constant Png.Colour_Type_Code := Png.Colour_Type (The_File);

      Maximum_Alpha : Natural;

      function Column_Angle (Column : Natural) return Angle.Degrees is
        use type Angle.Degrees;
      begin
        return Angle.Degrees(((Column + Width / 4) mod Width) * 360) / Angle.Degrees(Width);
      end Column_Angle;

      function Row_Angle (Row : Natural) return Angle.Degrees is
        use type Angle.Degrees;
      begin
        return Angle.Degrees((Height / 2 - Row) * 90) / Angle.Degrees(Height / 2);
      end Row_Angle;

      procedure Process_Row_Of (Column : Png.Coordinate)is
        The_Row : Png.Coordinate := Height / 2;
      begin
        for Row in reverse 0 .. Height / 2 loop
          declare
            A : constant Natural := Png.Alpha_Value (The_File, R => Row, C => Column);
          begin
            if A /= Maximum_Alpha then
              The_Row := Row;
              exit;
            end if;
          end;
        end loop;
        declare
          The_Altitude : Angle.Degrees := Row_Angle (The_Row);
          use type Angle.Degrees;
          use type Angle.Value;
        begin
          if The_Altitude < 2.0 then
            The_Altitude := 2.0;
          end if;
          Sky_Line.Append (Earth.Direction_Of (Az  => +Column_Angle (Column),
                                               Alt => +The_Altitude));
        end;
      end Process_Row_Of;

    begin -- Build_Sky_Line
      Log.Write ("Generate from file: " & Filename);
      Log.Write ("  Width       :"  & Width'img);
      Log.Write ("  Height      :"  & Height'img);
      Log.Write ("  Bit_Depth   : " & Bit_Depth'img);
      Log.Write ("  Colour_Type : " & Colour_Type'img);
      if not (Colour_Type in Png.Four | Png.Six) then
        Log.Error ("No alpha channel in " & Filename);
        return;
      end if;
      case Bit_Depth is
      when Png.Eight =>
        Maximum_Alpha := 2 ** 8 - 1;
      when Png.Sixteen =>
        Maximum_Alpha := 2 ** 16 - 1;
      when others =>
        Log.Error ("Bit depth not allowed");
        return;
      end case;
      for Column in 3 * Width / 4 .. Width - 1 loop
        Process_Row_Of (Column);
      end loop;
      for Column in 0 .. 3 * Width / 4 - 1 loop
        Process_Row_Of (Column);
      end loop;
    end Build_Sky_Line;

  begin -- Generate
    Sky_Line.Clear;
    begin
      Png.Open (The_File, Filename);
    exception
    when others =>
      Log.Error ("Failed to open file: " & Filename);
      return;
    end;
    Sky_Line.Create;
    Build_Sky_Line;
    Png.Close(The_File);
    Sky_Line.Close;
    Log.Write ("Generation complete");
  exception
  when Occurence: others =>
    Png.Close(The_File);
    Log.Termination (Occurence);
    Error.Raise_With ("Generation failed");
  end Generate;

end Horizon;
