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

with Interfaces;

package Raw_Data is

  File_Not_Found : exception;
  Invalid_File   : exception;
  Not_Found      : exception;

  subtype Byte is Interfaces.Unsigned_8;

  type Byte_Array is array (Positive range <>) of Byte;
  type Byte_Array_Access is access all Byte_Array;

  procedure Read (Filename : String);

  function Raw_Width return Natural;
  function Raw_Height return Natural;
  function Raw_Bits return Natural;
  function Raw_Compression return Natural;

  function Raw_Buffer return Byte_Array_Access;
  function Raw_Buffer_Bytes return Natural;

end Raw_Data;
