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

pragma Build (Description => "Siril test",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");

with Ada.Text_IO;
with Exceptions;
with Siril;
with Space;
with Units;

procedure Siril_Test is

  package IO renames Ada.Text_IO;

  Source_Directory : constant String := "T:\Pictures\M13";
  Work_Directory   : constant String := "D:\SkyTracker\Picture\Siril";
  Lights_Directory : constant String := Work_Directory & "\lights";

  Celestron_Focal_Length : constant Units.Focal_Length := 2669.6;

  EOS6D_Pixel_Size : constant Units.Pixel_Size := 6.55;

  M13_Direction : constant Space.Direction := Space.Direction_Of (Ra_Image  => "16h41m41s",
                                                                  Dec_Image => "+36d27'");
  M13_Tones : constant Units.Tones := (Shadows    => 0.00016,
                                       Midtones   => 0.00018,
                                       Highlights => 1.0);

  M13_Saturation : constant Units.Saturation := (Amount     => 0.9,
                                                 Multiplier => 1.0);
  procedure Progress (Command  : String;
                      Progress : Siril.Percent) is
    use type Siril.Percent;
  begin
    if Progress = Siril.Start then
      IO.Put (Command & " started");
    elsif Progress = Siril.Complete then
      IO.Put_Line (Ascii.Cr & Command & " Complete");
    else
      IO.Put (Ascii.Cr & Command & Progress'image & "%       ");
    end if;
  end Progress;

begin
  IO.Put_Line ("Siril Test");
  Siril.Open (Progress'unrestricted_access);
  Siril.Change_Directory (Source_Directory);

  Siril.Convert_Light (Destination => Lights_Directory);
  Siril.Change_Directory (Lights_Directory);

  Siril.Calibrate_Light (Dark => Work_Directory & "\darks_stacked.fit",
                         Flat => Work_Directory & "\flats_stacked.fit");
  Siril.Register_Light;
  Siril.Stack_Light;
  Siril.Load_Stacked_Light;

  Siril.Plate_Solve (M13_Direction, Celestron_Focal_Length, EOS6D_Pixel_Size);

  Siril.Remove_Background;
  Siril.Calibrate_Color;
  Siril.Transfer (M13_Tones);
  Siril.Set (M13_Saturation);

  Siril.Change_Directory ("..");
  Siril.Save_Jpeg ("M13");
  Siril.Close;
exception
when Item: others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Siril_Test;
