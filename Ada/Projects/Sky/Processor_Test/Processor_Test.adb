-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_Astronomy;

pragma Build (Description => "Processor test",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");

with Feeder;
with Ada.Text_IO;
with File;
with Exceptions;
with Time;
with Space;

procedure Processor_Test is

  package IO renames Ada.Text_IO;

  M13 : constant Space.Direction := Space.Direction_Of (Ra_Image  => "16h41m41s",
                                                        Dec_Image => "+36d27'");
begin
  IO.Put_Line ("Processor Test");
  Feeder.New_Sequence (Target => M13);
  for Filename of File.Iterator_For ("D:\Pictures\M13") loop
    IO.Put_Line ("File: " & Filename);
    Feeder.Next_Picture (Filename);
    Time.Wait (10.0);
  end loop;
exception
when Item : others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Processor_Test;
