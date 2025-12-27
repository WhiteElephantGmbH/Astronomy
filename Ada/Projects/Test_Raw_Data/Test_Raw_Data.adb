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

pragma Build (Description => "Test Raw Data",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Raw_Data;

procedure Test_Raw_Data is

  package IO renames Ada.Text_IO;

begin
  declare
    Grid : constant Raw_Data.Raw_Grid := Raw_Data.Grid ("Sample.CR2", 2000);
  begin
    IO.Put_Line ("Test Raw Data");
    IO.Put_Line ("=============");

    for Row in Grid'range(1) loop
      for Column in Grid'range(2) loop
        declare
          Pixel : constant Raw_Data.Pixel := Grid(Row, Column);
          use type Raw_Data.Pixel;
        begin
          if Pixel > 10000 then
            IO.Put_Line ("Pixel at row:" & Row'image & ", Column:" & Column'image & " =" & Pixel'image);
          end if;
        end;
      end loop;
    end loop;
  end;
  IO.Put_Line ("End");

exception
  when Raw_Data.File_Not_Found =>
    IO.Put_Line ("File not found.");
  when Raw_Data.Invalid_File =>
    IO.Put_Line ("Invalid CR2 file.");
  when Raw_Data.Not_Found =>
    IO.Put_Line ("RAW data not found.");
  when Raw_Data.Size_Error =>
    IO.Put_Line ("Size Error.");
  when Raw_Data.Unsupported =>
    IO.Put_Line ("Unsupported.");
end Test_Raw_Data;
