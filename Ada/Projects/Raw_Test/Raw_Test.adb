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

pragma Build (Description => "GID test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Raw;
with Exceptions;

procedure Raw_Test is
  The_Context : Raw.Context;
begin
  Ada.Text_IO.Put_Line ("Canon Test");
  Ada.Text_IO.Put_Line ("==========");
  Raw.Open_File (The_Context, "First_Picture.CR2");
  Ada.Text_IO.Put_Line ("Height =" & Raw.Image_Height (The_Context)'image);
  Ada.Text_IO.Put_Line ("Width  =" & Raw.Image_Width (The_Context)'image);
  Raw.Close (The_Context);
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Exceptions.Information_Of (Item));
end Raw_Test;
