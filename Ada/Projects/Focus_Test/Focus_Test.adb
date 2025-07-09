-- *********************************************************************************************************************
-- *                       (c) 2020 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
              Version     => (1, 0, 0, 3),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Focus;
with Ada.Exceptions;
with Ada.Text_IO;

procedure Focus_Test is
begin
  Ada.Text_IO.Put_Line ("Focus Test");
  Ada.Text_IO.Put_Line ("==========");
  declare
    Diameter : constant Focus.Diameter := Focus.Half_Flux_Diameter ("Granatstern.jpg");
  begin
    Ada.Text_IO.Put_Line ("HFD Granatstern:" & Diameter'img);
  end;
  declare
    Diameter : constant Focus.Diameter := Focus.Half_Flux_Diameter ("D:\Picture\Albireo.png");
  begin
    Ada.Text_IO.Put_Line ("HFD Albireo:" & Diameter'img);
  end;
  declare
    Diameter : constant Focus.Diameter := Focus.Half_Flux_Diameter ("D:\Picture\Andromeda.png");
  begin
    Ada.Text_IO.Put_Line ("HFD Andromeda:" & Diameter'img);
  end;
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Item));
end Focus_Test;
