-- *********************************************************************************************************************
-- *                       (c) 2020 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

pragma Build (Description => "GID test",
              Version     => (1, 0, 0, 2),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\22.0");

with Alpha;
with Ada.Exceptions;
with Ada.Text_IO;

procedure GID_Test is
begin
  Ada.Text_IO.Put_Line ("GID Test");
  Ada.Text_IO.Put_Line ("========");
  declare
    Limits : constant Alpha.Limits := Alpha.Limits_Of ("Test.Png");
  begin
    for The_Column in Limits'range loop
      Ada.Text_IO.Put_Line (The_Column'img & " :" & Limits(The_Column)'img);
    end loop;
  end;
  Ada.Text_IO.Put_Line ("Last_Row:" & Alpha.Last_Row'img);
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Item));
end GID_Test;
