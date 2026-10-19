-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

pragma Build (Description => "Stellarium test",
              Version     => (0, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");

with Ada.Text_IO;
with Exceptions;
with Space;
with Stellarium;
with Units;

procedure Stellarium_Test is

  package IO renames Ada.Text_IO;

  Target_Data : Stellarium.Target_Data;

begin
  IO.Put_Line ("Stellarium_Test");
  Stellarium.Set_Focus ("M13", Stellarium.Center);
  Target_Data := Stellarium.Get;
  IO.Put_Line ("Target: " & Target_Data.Target.S);
  IO.Put_Line ("Type: " & Target_Data.Kind.S);
  IO.Put_Line ("Direction: " & Space.Image_Of (Target_Data.Direction));
  IO.Put_Line ("Direction J2000: " & Space.Image_Of (Target_Data.Direction_J2000));
  Stellarium.Set_Camera_Rotation (42.0);
  IO.Put_Line ("Camera Rotation:" & Stellarium.Camera_Rotation'image);
  begin
    Stellarium.Set_Camera (Units.Canon_Eos_6D);
    IO.Put_Line ("Camera Name: " & Stellarium.Camera_Name);
    IO.Put_Line ("Chip Resolution: " & Stellarium.Chip_Resolution'image);
    IO.Put_Line ("Chip Size: " & Stellarium.Chip_Size'image);
    IO.Put_Line ("Chip Binning: " & Stellarium.Chip_Binning'image);
    IO.Put_Line ("Pixel Size:" & Stellarium.Pixel_Size'image);
  exception
  when Stellarium.Not_Found =>
    IO.Put_Line ("Camera not defined in Stellarium");
  end;
  begin
    Stellarium.Set_No_Lens;
  exception
  when Stellarium.Not_Found =>
    IO.Put_Line ("No Lens setting failed");
  end;
  begin
    Stellarium.Set_Telescope (Units.Celestron_C11);
    IO.Put_Line ("Telescope Name: " & Stellarium.Telescope_Name);
    IO.Put_Line ("Focal Length:" & Stellarium.Telescope_Focal_Length'image);
  exception
  when Stellarium.Not_Found =>
    IO.Put_Line ("Telescope not defined in Stellarium");
  end;
exception
when Occurrence: others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Occurrence));
end Stellarium_Test;
