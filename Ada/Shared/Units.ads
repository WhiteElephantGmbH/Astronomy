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

with Angle, Text;

package Units is

  subtype Darks_Id is String(1..5); -- "83016" => temperature range 8, 30s, ISO 1600
  -- Temperature Range
  --     .. <-15 -> 0
  -- -15 .. <-10 -> 1
  -- -10 .. <- 5 -> 2
  -- - 5 .. <  0 -> 3
  --   0 .. < 25 -> 4
  --   5 .. < 10 -> 5
  --  10 .. < 15 -> 6
  --  15 .. < 20 -> 7
  --  20 .. < 25 -> 8
  --  25 ..      -> 9

  Picture_Number_Count : constant := 4;
  type Picture_Number is range 1 .. 10**Picture_Number_Count - 1;

  F_L_Delta : constant := 0.1;
  type Focal_Length is delta F_L_Delta range F_L_Delta .. 10_000.0 - F_L_Delta with Small => F_L_Delta;

  P_S_Delta : constant := 0.01; -- um
  type Pixel_Size is delta P_S_Delta range P_S_Delta .. 100.0 - P_S_Delta with Small => P_S_Delta;

  type Pixel_Count is range 1 .. 10**5 - 1;

  type Chip_Resolution is record
    X : Pixel_Count;
    Y : Pixel_Count;
  end record;

  subtype Pixel_Binning is Pixel_Count range 1 .. 16;

  type Chip_Binning is record
    X : Pixel_Binning;
    Y : Pixel_Binning;
  end record;

  C_S_Delta : constant := 0.1; -- mm
  type Chip_Size is delta C_S_Delta range C_S_Delta .. 100.0 - C_S_Delta with Small => C_S_Delta;

  type Chip_Dimension is record
    X : Chip_Size;
    Y : Chip_Size;
  end record;
  
  subtype Field_Of_View is Angle.Degrees range 0.1 .. 180.0;

  L_M_Delta : constant := 0.01;
  type Lens_Multiplier is delta L_M_Delta range L_M_Delta .. 10.0 - L_M_Delta with Small => L_M_Delta;

  S_M_Delta : constant := 0.01;
  type Saturation_Amount     is delta S_M_Delta range 0.0 .. 1.0 with Small => S_M_Delta;
  type Saturation_Multiplier is delta S_M_Delta range S_M_Delta .. 100.0 - S_M_Delta with Small => S_M_Delta;

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
