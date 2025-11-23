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

package Raw is

  Pixel_Size : constant := 14;

  type Pixel is new Natural range 0 .. 2 ** Pixel_Size - 1 with Size => Pixel_Size;

  subtype Square_Size is Positive;

  --  Green grid: Square_Size rows, each with Square_Size / 2 green pixels.
  type Green_Grid is array (Positive range <>, Positive range <>) of Pixel;

  --  Compute the central square crop of the RAW Bayer mosaic and return only
  --  the green samples in a 2D grid.

  function Grid_Of (File_Name : String;
                    Size      : Square_Size) return Green_Grid
  with Pre => Size mod 2 = 0;

  --  Semantics:
  --    * Opens File_Name as a LibRaw context.
  --    * Calls libraw_unpack() to read the RAW mosaic.
  --    * Gets raw_width/raw_height and takes the central Square_Size x Square_Size region (Square_Size must be even).
  --    * Assumes an RGGB Bayer pattern (as on Canon EOS 6D).
  --    * For each of the Square_Size rows, collects all green samples into a row of length Square_Size / 2.

  Raw_Error : exception;
  -- when:
  --   * LibRaw errors (open/unpack),
  --   * Square_Size odd,
  --   * Square_Size larger than RAW dimensions,
  --   * NULL raw pointer.

end Raw;
