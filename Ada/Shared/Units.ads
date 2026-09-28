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
package Units is

  Focal_Length_Delta : constant := 0.1;
  type Focal_Length is delta Focal_Length_Delta range Focal_Length_Delta .. 9999.9 with Small => Focal_Length_Delta;

  Pixel_Size_Delta : constant := 0.01;
  type Pixel_Size is delta Pixel_Size_Delta range Pixel_Size_Delta .. 99.99 with Small => Pixel_Size_Delta;

  Saturation_Delta : constant := 0.01;
  type Saturation_Amount     is delta Saturation_Delta range 0.0 .. 1.0                with Small => Saturation_Delta;
  type Saturation_Multiplier is delta Saturation_Delta range Saturation_Delta .. 99.99 with Small => Saturation_Delta;

  type Saturation is record
    Amount     : Saturation_Amount;
    Multiplier : Saturation_Multiplier;
  end record;

  Tone_Delta : constant := 0.00001;
  type Tone is delta Tone_Delta range 0.0 .. 1.0 with Small => Tone_Delta;

  type Tones is record
    Shadows    : Tone;
    Midtones   : Tone;
    Highlights : Tone;
  end record;

  function "&" (Left  : String;
                Right : Focal_Length) return String is (Left & Text.Trimmed(Right'image));

  function "&" (Left  : String;
                Right : Pixel_Size) return String is (Left & Text.Trimmed(Right'image));

  function "&" (Left  : String;
                Right : Saturation_Amount) return String is (Left & Text.Trimmed(Right'image));

  function "&" (Left  : String;
                Right : Saturation_Multiplier) return String is (Left & Text.Trimmed(Right'image));

  function "&" (Left  : String;
                Right : Tone) return String is (Left & Text.Trimmed(Right'image));

end Units;
