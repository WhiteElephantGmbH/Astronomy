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

  Grid : constant Raw_Data.Raw_Grid := Raw_Data.Grid ("Sample.CR2", 10);

begin
  for Row in Grid'range(1) loop
    declare
      Row_Image : constant String := "  " & Row'image;
    begin
      IO.Put (Row_Image(Row_Image'last - 3 .. Row_Image'last) & ":");
      for Column in Grid'range(2) loop
        declare
          Column_Image : constant String := "     " & Grid(Row, Column)'image;
        begin
          IO.Put (Column_Image(Column_Image'last - 5 .. Column_Image'last));
        end;
      end loop;
      IO.New_Line;
    end;
  end loop;

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
