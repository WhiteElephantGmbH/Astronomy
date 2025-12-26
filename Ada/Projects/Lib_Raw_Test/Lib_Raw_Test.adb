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

pragma Build (Description => "Lib Raw Test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Exceptions;
with Lib_Raw;

procedure Lib_Raw_Test is

  package IO renames Ada.Text_IO;

begin
  IO.Put_Line ("Lib Raw Test");
  IO.Put_Line ("============");

  declare
    Grid : constant Lib_Raw.Grid := Lib_Raw.Grid_Of ("Sample.CR2", 2000);
  begin
    for Row in Grid'range(1) loop
      for Column in Grid'range(2) loop
        declare
          Pixel : constant Lib_Raw.Pixel := Grid(Row, Column);
          use type Lib_Raw.Pixel;
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
when Lib_Raw.Raw_Error =>
  IO.Put_Line ("### Raw Error");
when Item: others =>
  IO.Put_Line (Exceptions.Information_Of (Item));
end Lib_Raw_Test;
