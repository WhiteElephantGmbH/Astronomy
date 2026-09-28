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

with Text;

package body Siril is

  procedure Open (Progress : Progress_Handler := null) is
  begin
    Siril_Interface.Open (Progress);
    Siril_Interface.Execute ("requires 1.4.4");
  end Open;


  procedure Change_Directory (Name : String) is
  begin
    SI.Execute ("cd " & Name);
  end Change_Directory;


  procedure Convert_Light (Destination : String) is
  begin
    SI.Execute ("convert light -out=" & Destination);
  end Convert_Light;


  procedure Calibrate_Light (Dark : String;
                             Flat : String) is
  begin
    SI.Execute ("calibrate light -dark=" & Dark & " -flat=" & Flat
              & " -cc=dark 3 3 -equalize_cfa -debayer -prefix=pp_");
  end Calibrate_Light;


  procedure Register_Light is
  begin
    SI.Execute ("register pp_light_");
  end Register_Light;


  procedure Stack_Light (Output : String) is
  begin
    SI.Execute ("stack r_pp_light_ mean winsorized 3 3 -norm=addscale -out=" & Output);
  end Stack_Light;


  procedure Load (File_Name : String) is
  begin
    SI.Execute ("load " & File_Name);
  end Load;


  function Ra_Image_Of (Direction : Space.Direction) return String is
    Image    : constant String := Space.Ra_Image_Of (Direction);     -- [d]dhddmdd.dds
    Ra_Image : String := '0' & Image(Image'first .. Image'last - 4); -- 0[d]dhddmdd
  begin
    Ra_Image(Ra_Image'last - 2) := ':'; -- replace m
    Ra_Image(Ra_Image'last - 5) := ':'; -- replace h
    return Ra_Image(Ra_Image'last - 7 .. Ra_Image'last);
  end Ra_Image_Of;


  function Dec_Image_Of (Direction : Space.Direction) return String is
    Image     : constant String    := Text.Ansi_Of_Utf8 (Space.Dec_Image_Of (Direction)); -- s[d]d°dd'dd".d
    Sign      : constant Character := Image(Image'first);                                 -- +|-
    Dec_Image : String := '0' & Image(Image'first + 1 .. Image'last - 3);                 -- 0[d]d°dd'dd
  begin
    Dec_Image(Dec_Image'last - 2) := ':'; -- replace '
    Dec_Image(Dec_Image'last - 5) := ':'; -- replace °
    return Sign & Dec_Image(Dec_Image'last - 7 .. Dec_Image'last);
  end Dec_Image_Of;


  procedure Plate_Solve (Direction    : Space.Direction;
                         Focal_Length : Units.Focal_Length;
                         Pixel_Size   : Units.Pixel_Size) is
    RA  : constant String := Ra_Image_Of (Direction);
    Dec : constant String := Dec_Image_Of (Direction);
    use type Units.Focal_Length;
    use type Units.Pixel_Size;
  begin
    SI.Execute ("platesolve " & RA & " " & Dec & " -focal=" & Focal_Length & " -pixelsize=" & Pixel_Size);
  end Plate_Solve;


  procedure Remove_Background is
  begin
    SI.Execute ("subsky -rbf -samples=20 -tolerance=2.0 -smooth=0.5");
  end Remove_Background;


  procedure Calibrate_Color is
  begin
    SI.Execute ("spcc");
  end Calibrate_Color;


  procedure Transfer (Tones : Units.Tones) is
    use type Units.Tone;
  begin
    SI.Execute ("mtf " & Tones.Shadows & " " & Tones.Midtones & " " & Tones.Highlights);
  end Transfer;


  procedure Set (Saturation : Units.Saturation) is
    use type Units.Saturation_Amount;
    use type Units.Saturation_Multiplier;
  begin
    SI.Execute ("satu " & Saturation.Amount & " " & Saturation.Multiplier);
  end Set;


  procedure Save_Jpeg (File_Name : String) is
  begin
    SI.Execute ("savejpg " & File_Name);
  end Save_Jpeg;


  procedure Close renames Siril_Interface.Close;

end Siril;
