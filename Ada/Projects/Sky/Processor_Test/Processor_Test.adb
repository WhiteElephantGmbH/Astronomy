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

--Darks_Id : constant String := "83016"; --M13
  Darks_Id : constant String := "71016"; --M27, NGC7331

--M13 : constant Space.Direction := Space.Direction_Of (Ra_Image  => "16h41m41s",
--                                                      Dec_Image => "+36d27'");
--M27 : constant Space.Direction := Space.Direction_Of (Ra_Image  => "19h59m36s",
--                                                      Dec_Image => "+22d43'16""");
  NGC_7331 : constant Space.Direction := Space.Direction_Of (Ra_Image  => "22h37m06s",
                                                             Dec_Image => "+34d25'07""");
begin
  IO.Put_Line ("Processor Test");
  Feeder.New_Darks (Darks_Id, 20);
  for Filename of File.Iterator_For ("D:\Pictures\Darks_" & Darks_Id) loop
    IO.Put_Line ("Dark: " & Filename);
    Feeder.Next_Picture (Filename);
  end loop;
--Feeder.New_Sequence (M13, Darks_Id);
--Feeder.New_Sequence (M27, Darks_Id);
  Feeder.New_Sequence (NGC_7331, Darks_Id);
--for Filename of File.Iterator_For ("D:\Pictures\M13") loop
--for Filename of File.Iterator_For ("D:\Pictures\M27") loop
  for Filename of File.Iterator_For ("D:\Pictures\NGC_7331") loop
    IO.Put_Line ("Light: " & Filename);
    Feeder.Next_Picture (Filename);
    Time.Wait (10.0);
  end loop;
  Feeder.Stop;
exception
when Item : others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Processor_Test;
