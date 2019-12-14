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

package Png is

  type File is limited private;

  procedure Open (F : in out File; Filename : in String);
  -- The Open procedure reads the entire file and stores the image
  -- information and pixel data in an internal file descriptor for
  -- access using the various interrogation functions provided below.

  procedure Close (F : in out File);
  -- The Close procedure deallocates the file descriptor and all the
  -- image information therein.


  subtype Dimension is Positive range 1 .. (2**31)-1;
  -- The image sizes.

  function Width (F : File) return Dimension;

  function Height (F : File) return Dimension;


  type Depth is (One, Two, Four, Eight, Sixteen);
  -- The number of bits per sample.
  -- A PNG file can contain five types of image with various bit depths allowed
  -- for each type. Two enumerated types indicate the type of image in a file,
  -- and the number of bits per sample.

  function Bit_Depth (F : File) return Depth;


  type Colour_Type_Code is (Zero, Two, Three, Four, Six); -- The PNG Colour Type.

  function Colour_Type(F : File) return Colour_Type_Code;


  subtype Coordinate is Natural range 0 .. (2**31)-2; -- Image indices.
  -- Note on image indexing: this package numbers rows and columns from the
  -- top and left of the image. Row 0 is the topmost row of the image, column 0
  -- is the leftmost column. This is conventional for graphics and is often the
  -- convention for image processing.

  function Alpha_Value (F    : File;
                        R, C : Coordinate) return Natural with Inline;
  -- Function to access alpha values of an open file.
  -- The high-order bits are zero-filled. Since the user can
  -- find out the actual bit depth in the PNG file, these values can be
  -- converted to a user type with fewer bits if required.

  -- Because these function must be called for every pixel in an image
  -- (therefore many times for a given image) they are implemented for
  -- efficiency and perform almost no error checking.  It is the
  -- responsibility of the user to verify that the PNG file contains the
  -- correct type of data. Since this package is intended as a low-level
  -- foundation for a user-level package this should not be a problem.

  Call_Error      : exception; -- Indicates incorrect use of the procedures/functions (sequence error).
  Signature_Error : exception; -- Indicates (on open) that the PNG file had an incorrect signature.
  Format_Error    : exception; -- Indicates (on open) that the file contained incorrect data.
  CRC_Error       : exception; -- Indicates (on open) an error in a cyclic redundancy check (CRC).
  End_Error       : exception; -- unexpected end of file reached.
  Status_Error    : exception; -- attempt to close a non open file.

private

  type File_Descriptor;
  type File is access File_Descriptor;

end Png;
