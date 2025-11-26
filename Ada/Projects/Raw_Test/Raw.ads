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

  Pixel_Size : constant := 16; -- allowed 8 or 16

  -- Maximums for Canon EOS 6D
  Min_With_Or_Height : constant := 3648;
  Max_With_Or_Height : constant := 5472;

  type Pixel is new Natural range 0 .. 2 ** Pixel_Size - 1 with Size => Pixel_Size;

  type Square_Size is new Natural range 2 .. Min_With_Or_Height with Dynamic_Predicate => Square_Size mod 2 = 0;

  --  Green grid: Square_Size rows, each with Square_Size pixels.
  --
  --  Semantics (using dcraw-style processed image via LibRaw C API):
  --    * We call libraw_open_file + libraw_unpack + libraw_dcraw_process.
  --    * Then libraw_dcraw_make_mem_image() to get an RGB (or similar) image buffer in memory.
  --    * We force LibRaw to disable auto-bright, and set a linear gamma (1.0).
  --    * We take the central square crop of size Size x Size
  --    * For each pixel in that crop we extract the green channel value.
  type Columns is range 1 .. Max_With_Or_Height;
  type Rows    is range 1 .. Max_With_Or_Height;

  type Position is record
    Column : Columns;
    Row    : Rows;
  end record;

  type Row_Range is record
    First : Rows;
    Last  : Rows;
  end record;

  type Green_Grid is array (Rows range <>, Columns range <>) of Pixel;

  --  Compute the central square crop of the processed image and return
  --  the green channel samples in a 2D grid: Size rows, Size columns.
  --
  --  Raises Raw_Error when:
  --    * libraw_init returns NULL,
  --    * LibRaw open/unpack/process/make_mem_image calls fail,
  --    * Size exceeds processed image dimensions,
  --    * processed image has unsupported format (bits /= 16, colors < 1),
  --    * NULL data pointer.
  function Grid_Of (File_Name : String;
                    Size      : Square_Size) return Green_Grid;

  Raw_Error : exception;

end Raw;
