-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Command_Line;
with Ada.Text_IO;
with Angle;
with Exceptions;
with Stellarium;
with Png;

procedure Png_Alpha is

  pragma Linker_Options ("-mconsole");

  Syntax_Error : constant String := "Syntax: png_alpha";

  procedure Put_Line (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put_Line;

begin -- Png_Alpha

  if Ada.Command_Line.Argument_Count /= 0 then
    Put_Line (Syntax_Error);
    return;
  end if;

  declare
    Filename : constant String  := Stellarium.Landscape_Filename;
    The_File : Png.File;
  begin
    begin
      Png.Open (The_File, Filename);
    exception
    when Occurence : others =>
      Put_Line ("Failed to open file: " & Filename);
      Put_Line (Exceptions.Information_Of (Occurence));
      return;
    end;

    declare
      W : constant Png.Dimension        := Png.Width (The_File);
      H : constant Png.Dimension        := Png.Height (The_File);
      D : constant Png.Depth            := Png.Bit_Depth (The_File);
      T : constant Png.Colour_Type_Code := Png.Colour_Type (The_File);

      function Column_Angle (Column : Natural) return Angle.Degrees is
        use type Angle.Degrees;
      begin
        return Angle.Degrees(((Column + W/4) mod W) * 360) / Angle.Degrees(W);
      end Column_Angle;

      function Row_Angle (Row : Natural) return Angle.Degrees is
        use type Angle.Degrees;
      begin
        return Angle.Degrees((H/2 - Row) * 90) / Angle.Degrees(H/2);
      end Row_Angle;

      procedure Process_Row_Of (Column : Png.Coordinate)is
        The_Row : Png.Coordinate := H/2;
        use type Angle.Value;
      begin
        for Row in reverse 0 .. H/2 loop
          declare
            A : constant Natural := Png.Alpha_Value (The_File, R => Row, C => Column);
          begin
            if A /= 255 then
              The_Row := Row;
              exit;
            end if;
          end;
        end loop;
        declare
          The_Altitude : Angle.Degrees := Row_Angle (The_Row);
          use type Angle.Degrees;
        begin
          if The_Altitude < 2.0 then
            The_Altitude := 2.0;
          end if;
          Put_Line (Angle.Image_Of (+Column_Angle (Column)) & ", " & Angle.Image_Of (+The_Altitude));
        end;
      end Process_Row_Of;

    begin
      Put_Line ("Image information from file: " & Filename);
      Put_Line ("  Width  :"  & W'img);
      Put_Line ("  Height :"  & H'img);
      Put_Line ("  Depth  : " & D'img);
      Put_Line ("  Type   : " & T'img);
      Put_Line ("Limits");
      for Column in 3*W/4 .. W-1 loop
        Process_Row_Of (Column);
      end loop;
      for Column in 0 .. 3*W/4-1 loop
        Process_Row_Of (Column);
      end loop;
    end;

    Png.Close(The_File);
  end;
exception
when Item: others =>
  Put_Line ("Program terminated by exception.");
  Put_Line (Exceptions.Information_Of (Item));
end Png_Alpha;
