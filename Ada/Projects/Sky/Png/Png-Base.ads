-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                                                                                                                   *
-- *    A big part of this software was created by                                                                     *
-- *    Dr Stephen J. Sangwine sangwine@users.sourceforge.net                                                          *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Streams;
package Png.Base is

  -- PNG files start with an 8-byte signature (ISO/W3C standard § 5.2).

  Signature : constant Ada.Streams.Stream_Element_Array(1 .. 8) :=
    (16#89#, 16#50#, 16#4E#, 16#47#, 16#0D#, 16#0A#, 16#1A#, 16#0A#);

  -------------------------------------------------------------------------

  -- A PNG file can contain various chunks, each with a 4-byte chunk type
  -- code. The chunk type codes are defined here numerically, so that they
  -- are static, but they are verified at the end of the package body so
  -- that errors in the coding of the numerical values are trapped. If any
  -- new codes are added the verification code must also be added. PNG type
  -- codes are defined in terms of ISO 8859-1 Latin-1 character codes as
  -- are the codes for the Ada2012 package Standard listed in LRM Section A.1
  -- from which the following table is derived:
  --
  --      x = 0123456789ABCDEF
  -- 16#4x#    ABCDEFGHIJKLMNO
  -- 16#5x#   PQRSTUVWXYZ
  -- 16#6x#    abcdefghijklmno
  -- 16#7x#   pqrstuvwxyz

  IHDR : constant := 16#49484452#;
  PLTE : constant := 16#504C5445#;
  IDAT : constant := 16#49444154#;
  IEND : constant := 16#49454E44#;

  TRNS : constant := 16#74524E53#;
  GAMA : constant := 16#67414D41#;
  CHRM : constant := 16#6348524D#;
  SRGB : constant := 16#73524742#;
  ICCP : constant := 16#69434350#;
  TEXT : constant := 16#74455874#;
  ZTXT : constant := 16#7A545874#;
  ITXT : constant := 16#69545874#;

  BKGD : constant := 16#624B4744#;
  PHYS : constant := 16#70485973#;
  SBIT : constant := 16#73424954#;
  SPLT : constant := 16#73504C54#;
  HIST : constant := 16#68495354#;
  TIME : constant := 16#74494D45#;

  --------------------------------

  -- The IEND chunk in any PNG file contains the same 12 bytes, since the
  -- data length is zero, and therefore the CRC always has the same value.
  -- The 12 bytes here are the entire content of the IEND chunk. They are
  -- represented as a Stream_Element_Array so that the 12 bytes can be
  -- directly written to a PNG file.

  IEND_Chunk : constant Ada.Streams.Stream_Element_Array(1 .. 12) :=
                        (16#00#, 16#00#, 16#00#, 16#00#,  -- Size = 0.
                         16#49#, 16#45#, 16#4E#, 16#44#,  -- IEND.
                         -- No data bytes.
                         16#AE#, 16#42#, 16#60#, 16#82#); -- CRC.

  -------------------------------------------------------------------------

  -- Look up tables for converting bit depths and colour type codes to
  -- numeric values.

  Bit_Depth_Table : constant array (Depth) of Ada.Streams.Stream_Element := (1, 2, 4, 8, 16);

private

  -- There are certain assumptions made in the design of PNG_IO. These are
  -- verified here, because this base package is used by PNG_IO itself and
  -- also by utility programs bundled with the package that do not 'with'
  -- the main PNG_IO package.

  -- PNG_IO assumes that Natural has a large enough range to be able to
  -- pass 32-bit values such as image dimensions from PNG files. PNG
  -- four-byte unsigned integers are limited to the range 0 to 2**31 - 1,
  -- so we verify that Natural can support this range.
  -- (LRM 3.5.4(21) requires the minimum range for Integer to be 16-bits,
  --  so it is not guaranteed that Natural has a large enough range.)

  pragma Assert(Natural'last >= 2**31 - 1);

  -- The type Stream_Element is defined in Ada.Streams as
  -- "mod implementation defined". The most likely possibility is a byte,
  -- i.e. mod 2**8, on all modern systems, but it is not guaranteed that
  -- this will be so. If Stream_Element is not a byte, PNG_IO will not
  -- compile and work without modification (neither will Zlib_Ada, so this
  -- would be a fairly serious problem).

  pragma Assert (Ada.Streams.Stream_Element'size    =    8);
  pragma Assert (Ada.Streams.Stream_Element'modulus = 2**8);

end Png.Base;
