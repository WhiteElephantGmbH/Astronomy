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

package Raw_Data is

  type Columns is range 1 .. 10000;
  type Rows    is range 1 .. 10000;

  Pixel_Bits : constant := 16;

  type Pixel is new Natural range 0 .. 2 ** Pixel_Bits - 1
    with Size => Pixel_Bits;

  type Raw_Grid is array (Rows range <>, Columns range <>) of Pixel;

  type Square_Size is new Natural range 2 .. 2000
    with Dynamic_Predicate => Square_Size mod 2 = 0;

  function Grid
    (Filename : String;       -- <name>.CR2
     Size     : Square_Size) return Raw_Grid;

  File_Not_Found : exception;
  Invalid_File   : exception;
  Not_Found      : exception;
  Unsupported    : exception;
  Size_Error     : exception;

end Raw_Data;
