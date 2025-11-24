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

  Pixel_Size : constant := 8;

  type Pixel is new Natural range 0 .. 2 ** Pixel_Size - 1
    with Size => Pixel_Size;

  subtype Square_Size is Positive;

  --  Green grid: Square_Size rows, each with Square_Size pixels.
  --
  --  Semantics (using dcraw-style processed image via LibRaw C API):
  --    * We call libraw_open_file + libraw_unpack + libraw_dcraw_process.
  --    * Then libraw_dcraw_make_mem_image() to get an RGB (or similar)
  --      8-bit image buffer in memory.
  --    * We take the central square crop of size Size x Size
  --      (Size must be even and <= min(width, height)).
  --    * For each pixel in that crop we extract the green channel value.
  --    * The result is a Size x Size grid of green intensities.
  --
  --  Result indices:
  --    Result (R, C) with
  --      R in 1 .. Size
  --      C in 1 .. Size
  type Green_Grid is array (Positive range <>, Positive range <>) of Pixel;

  --  Compute the central square crop of the processed image and return
  --  the green channel samples in a 2D grid: Size rows, Size columns.
  --
  --  Pre:
  --    * Size is even.
  --
  --  Raises Raw_Error when:
  --    * libraw_init returns NULL,
  --    * LibRaw open/unpack/process/make_mem_image calls fail,
  --    * Size exceeds processed image dimensions,
  --    * processed image has unsupported format (bits != 16, colors < 1),
  --    * NULL data pointer.
  function Grid_Of (File_Name : String;
                    Size      : Square_Size) return Green_Grid
    with Pre => Size mod 2 = 0;

  Raw_Error : exception;

end Raw;
