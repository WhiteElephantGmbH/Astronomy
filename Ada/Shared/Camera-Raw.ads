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

private package Camera.Raw is

  procedure Prepare_Grid (File_Name : String;
                          Size      : Square_Size);
  -- Computes the central square crop of the processed image and returns green channel pixels in a 2D grid
  -- Raises Raw_Error when:
  --   libraw_init returns NULL,
  --   LibRaw open/unpack/process/make_mem_image calls fail,
  --   Size exceeds processed image dimensions,
  --   processed image has unsupported format (bits /= 16, colors < 1),
  --   NULL data pointer.
  Raw_Error : exception;

  procedure Stop_Preparing;

  function Grid return Green_Grid;

end Camera.Raw;
