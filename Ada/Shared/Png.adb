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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

with Png.Adam7;
with Png.Base;

with ZLib;

package body Png is

  -- 12 August 2006 : the way the version number is declared was changed in order
  -- to make PNG_IO comply with the Ravenscar profile (at least under Gnat/gcc -
  -- since there are some implementation dependencies in this profile). Thanks
  -- to Samuel Tardieu for pointing out that PNG_IO was almost Ravenscar compliant
  -- and that only a small change would be needed to make it so.
  -- TODO Check the Ada 2012 LRM for the requirements and make sure PNG_IO complies.

  -------------------------------------------------------------------

  NUL : Character renames Ada.Characters.Latin_1.NUL;

  --------------------------------------------------------------------------

  use type Ada.Streams.Stream_Element;
  use type Ada.Streams.Stream_Element_Count;
  use type Ada.Streams.Stream_Element_Array;

  ------------------------------------------------------------------------

  -- By placing the full declaration for Chunk_Descriptor here, it is
  -- visible to children of PNG_IO. Thus PNG_IO can be extended to
  -- handle other ancillary chunk types (including user defined chunks
  -- as well as less-often used standard chunks) by adding child packages.

  type Chunk_List_Element;

  type Chunk_List is access Chunk_List_Element;

  type Chunk(Size : Ada.Streams.Stream_Element_Count) is
    record
      Data : Ada.Streams.Stream_Element_Array(1 .. Size);
    end record;

  type Chunk_List_Element(Size : Ada.Streams.Stream_Element_Count) is
    record
      Chnk : Chunk(Size);
      Link : Chunk_List;
    end record;

  -- We define the encodings for the sRGB rendering intent so that no
  -- explicit conversion is necessary between the binary representation in
  -- a PNG file, and the internal representation used in this package.

  type Rendering_Intent is (Perceptual, Relative_Colorimetric,
                            Saturation, Absolute_Colorimetric);

  for Rendering_Intent use (Perceptual            => 0,
                            Relative_Colorimetric => 1,
                            Saturation            => 2,
                            Absolute_Colorimetric => 3);

  function Valid_Zlib_Header (CMG, FLG : in Ada.Streams.Stream_Element) return Boolean is
    -- This function checks the data in the first two bytes of a Zlib
    -- data stream, supplied as the two parameters. (ISO standard, section 10.1.)
    -- We check the least significant 4 bits of CMG (the first byte)
    -- and that the overall 16-bit value is a multiple of 31 as required
    -- by RFC1950 (the Zlib Compressed Data Format Specification).
    use type Interfaces.Unsigned_16;
  begin
    return ((CMG and 2#1111#) = 8 and (Interfaces.Unsigned_16(CMG) * 256 + Interfaces.Unsigned_16(FLG)) mod 31 = 0);
  end Valid_Zlib_Header;


  -- Declare a subtype for passing buffers of data to Zlib-Ada. We cannot use a standard
  -- Stream_Element_Array because Stream_Element_Offset'First may not have a predecessor
  -- (it could be the most negative 64-bit integer on a 64-bit machine). Since Zlib-Ada
  -- must be able to subtract N from a buffer index of length N, and obtain zero, we must
  -- ensure that there is a predecessor value to the first index. The simplest way to do
  -- this is to require the first index to be 1. There seems to be no way to declare a
  -- subtype directly expressing this, starting with Stream_Element_Array, so we resort to
  -- a dynamic predicate, which will enforce the requirement, subject to the assertion
  -- policy in force.

  subtype Stream_Element_Buffer is Ada.Streams.Stream_Element_Array with Dynamic_Predicate =>
          Stream_Element_Buffer'first = 1;

  -- One step compression and decompression to and from Zlib format, for compressing and
  -- decompressing text and international text chunks, iCC profile chunk etc. Not to be
  -- used for larger blocks of data where dynamically allocated memory is preferable.

  ----------------------------------------------------------------------
  -- PNG files written by this package contain the following text in a
  -- tEXt chunk to indicate the software that wrote the chunk. The string
  -- is the whole data field of the chunk. The NUL character is the null
  -- byte that delimits the keyword field.

  -------------------------------------------------------------------------

  -- Define the PNG filter type codes.

  None    : constant := 0;
  Sub     : constant := 1;
  Up      : constant := 2;
  Average : constant := 3;
  Paeth   : constant := 4;

  --------------------------------------------------------------

  type Palette_Data is array (Interfaces.Unsigned_8 range <>) of Interfaces.Unsigned_8;

  type Colour_Palette(Size : Interfaces.Unsigned_8) is
    record
      R, G, B : Palette_Data(0 .. Size);
    end record;

  type Palette_Pointer is access Colour_Palette;

  subtype Buffer is Ada.Streams.Stream_Element_Array; -- TODO Change this to Stream_Element_Buffer.

  type Buffer_Pointer is access Buffer;

  subtype Buffer_4 is Buffer(1 .. 4); -- I.e. 4 bytes, 32 bits.

  -- The Text_Item type is used to store a keyword/text string pair and a
  -- link pointer. Text strings are stored in a linked list with the
  -- head pointer in the PNG_File_Descriptor. This is awkward, but
  -- since the number of these strings is not defined, and they are
  -- of variable length, it is difficult to see what else to do.

  type String_Pointer is access String;

  type Text_Item;
  type Text_Item_Pointer is access Text_Item;

  type Text_Item is
    record
      Keyword     : String_Pointer;
      Text_String : String_Pointer;
      Link        : Text_Item_Pointer;
    end record;

  type Pass_Offsets is array (Adam7.Pass_Number) of Ada.Streams.Stream_Element_Offset;

  type File_Descriptor is
    record
      Handle      : Ada.Streams.Stream_IO.File_Type;
      Stream      : Ada.Streams.Stream_IO.Stream_Access;
      Width,
      Height      : Dimension;
      Bit_Depth,
      Colour_Type,
      Compression,
      Filter,
      Interlace   : Interfaces.Unsigned_8;

      Gamma            : Boolean := False;
      Gamma_Value      : Interfaces.Unsigned_32;
      Chroma           : Boolean := False;
      White_X, White_Y,
      Red_X,     Red_Y,
      Green_X, Green_Y,
      Blue_X,   Blue_Y  : Interfaces.Unsigned_32;
      SRGB              : Boolean := False;
      Rendering         : Rendering_Intent;
      Physical          : Boolean := False;
      Phys_X, Phys_Y    : Interfaces.Unsigned_32;
      Phys_Unit         : Interfaces.Unsigned_8;
      Number_Of_Texts   : Natural := 0;
      Text_Strings      : Text_Item_Pointer;
      -- We handle iTexts differently to Texts (just above). This is because the code for
      -- iTexts was written much later (January 2019), and experimentally, it just stores
      -- the raw chunk in the descriptor (actually in a list pointed to by the descriptor),
      -- without any decoding or validity checking on read. Only when the chunk is read by
      -- a user-callable function is the data interpreted and extracted.
      -- This is a model that could be extended to other chunks, in time, transparent to
      -- the user.
      Number_Of_Itexts  : Natural := 0;
      Itext_Chunks      : Chunk_List;
      Number_Of_Chunks  : Natural := 0 with Unreferenced; -- Unrecognised ancillary chunks
      Ancillary_Chunks  : Chunk_List;   -- are tacked on here as a list.

      Palette           : Palette_Pointer;
      Uncompressed_Data : Buffer_Pointer;
      Interlace_Offsets : Pass_Offsets;
    end record;


  procedure Deallocate is new Ada.Unchecked_Deallocation(Buffer, Buffer_Pointer);

  -- TODO The following conversion functions do not actually do any modification of the
  -- data bytes. It should be possible to perform the conversion without a loop.


  procedure Check(F : in File) with Inline is
    -- Checks that the file descriptor F exists.
  begin
    if F = null then
      raise Call_Error; -- Attempt to access non-existent file descriptor
    end if;
  end Check;


  function Bits_Per_Pixel (Colour_Type : Colour_Type_Code;
                           Bit_Depth   : Depth) return Positive with Inline is
    -- The number of bits per pixel in the IDAT chunks. This is not
    -- always the same as the number of bits per pixel in the image.
    -- The only exception is that there are 24 bits per pixel for
    -- images of colour type 3 (palette colour).
    BD : constant Positive := Positive(Base.Bit_Depth_Table(Bit_Depth));
  begin
    case Colour_Type is
    when Zero  =>
      return BD;     -- Greyscale.
    when Two   =>
      return BD * 3; -- RGB
    when Three =>
      return BD;     -- Palette colour.
    when Four  =>
      return BD * 2; -- Greyscale + alpha.
    when Six =>
      return BD * 4; -- RGB + alpha.
    end case;
  end Bits_Per_Pixel;


  function Bytes_Per_Pixel (Colour_Type : Colour_Type_Code;
                            Bit_Depth   : Depth) return Positive with Inline is
    -- The number of bytes in each pixel rounded up to 1 for bit depths
    -- less than 8. Used in filtering scanlines where it defines the
    -- offset between one byte and the corresponding byte from the next
    -- pixel, or containing the next pixel.
  begin
    return Positive'max(1, (Bits_Per_Pixel(Colour_Type, Bit_Depth))/8);
  end Bytes_Per_Pixel;


  function Bytes_Per_Scanline (Colour_Type    : Colour_Type_Code;
                               Bit_Depth      : Depth;
                               Scanline_Width : Dimension) return Ada.Streams.Stream_Element_Count is
    -- The number of bytes per scanline (excluding the filter type byte) must allow for
    -- pixels of less than 8 bits with image widths which do not result in an integral
    -- number of bytes. We round up the number of bytes by adding 7 to the number of bits.
  begin
    return (Ada.Streams.Stream_Element_Count(Bits_Per_Pixel(Colour_Type, Bit_Depth) * Scanline_Width + 7)/8);
  end Bytes_Per_Scanline;


  function Image_Size (Colour_Type : Colour_Type_Code;
                       Bit_Depth   : Depth;
                       X, Y        : Dimension;
                       Interlaced  : Boolean) return Ada.Streams.Stream_Element_Count is

    -- Computes the size of image data in bytes, taking account of wasted bits, filter
    -- type bytes and interlacing, for use in allocating buffers.

    function Sub_Image_Size(W, H : Natural) return Ada.Streams.Stream_Element_Count with Inline is
    begin
      if W = 0 or H = 0 then
        return 0; -- Empty pass: see PNG Specification Section 2.6.
      else
        -- The + 1 in the next line is to allow for the filter type byte.
        return Ada.Streams.Stream_Element_Count(H) * (Bytes_Per_Scanline(Colour_Type, Bit_Depth, W) + 1);
      end if;
    end Sub_Image_Size;

  begin -- Image_Size
    if not Interlaced then
      return Sub_Image_Size(X, Y);
    else
      declare
        R : Ada.Streams.Stream_Element_Count := 0;
      begin
        for P in Adam7.Pass_Number loop
          R := R + Sub_Image_Size(Adam7.Sub_Image_Width(X, P), Adam7.Sub_Image_Height(Y, P));
        end loop;
        return R;
      end;
    end if;
  end Image_Size;


  function Interlaced (F : File) return Boolean is
    use type Interfaces.Unsigned_8;
  begin
    return (F.Interlace = 1);
  end Interlaced;


  function Mean (X, Y : Ada.Streams.Stream_Element) return Ada.Streams.Stream_Element with Inline is
    -- Function to compute the mean value used in the Average
    -- filter described in the ISO standard, Section 9.2 and Table 9.1.
    type Nine_Bit is mod 2**9;
  begin
    return Ada.Streams.Stream_Element((Nine_Bit(X) + Nine_Bit(Y))/2);
  end Mean;


  function Paeth_Predictor (A, B, C : Ada.Streams.Stream_Element) return Ada.Streams.Stream_Element with Inline is
    -- This code is based on the pseudocode given in the ISO standard, Section 9.4.
    P  : constant Integer := Integer(A) + Integer(B) - Integer(C);
    PA : constant Integer := abs(P - Integer(A));
    PB : constant Integer := abs(P - Integer(B));
    PC : constant Integer := abs(P - Integer(C));
  begin
    if PA <= PB and PA <= PC then return A;
    elsif PB <= PC then return B;
    else return C;
    end if;
  end Paeth_Predictor;


  function Width (F : File) return Dimension is
  begin
    Check (F);
    return F.Width;
  end Width;


  function Height (F : File) return Dimension is
  begin
    Check(F);
    return F.Height;
  end Height;


  function Bit_Depth (F : File) return Depth is
  begin
    Check(F);
    case F.Bit_Depth is
    when 1 =>
      return One;
    when 2 =>
      return Two;
    when 4 =>
      return Four;
    when 8 =>
      return Eight;
    when 16 =>
      return Sixteen;
    when others =>
    -- Since F.Bit_Depth was validated when the IHDR chunk
    -- was read by Open, this simply should not happen. So
      raise Program_Error;
    end case;
  end;


  function Colour_Type (F : File) return Colour_Type_Code is
  begin
    Check(F);
    declare
      T : Interfaces.Unsigned_8 renames F.Colour_Type;
      use type Interfaces.Unsigned_8;
    begin
      if T = 0 then
        return Zero;
      elsif T = 2 then
        return Two;
      elsif T = 3 then
        return Three;
      elsif T = 4 then
        return Four;
      elsif T = 6 then
        return Six;
      else
        raise Program_Error;
      end if;
    end;
  end Colour_Type;


  function Palette (F : File) return Boolean is
    -- The test here was modified 29 June 2004. Previously
    -- it checked for a colour type of 3 rather than for the
    -- presence of a palette. (Colour types 2 and 6 can have
    -- an optional palette.)
  begin
    Check(F);
    return F.Palette /= null;
  end Palette;


  procedure Open (F        : in out File;
                  Filename : in     String) is

    -- This procedure has the following major stages:
    -- 1. Read the file signature and IHDR chunk.
    -- 2. Read the IDAT, PLTE and other chunks up to the IEND chunk,
    --    placing the raw image data into a buffer and any information
    --    extracted from other chunks into the descriptor. In the case
    --    of IDAT and zTXt chunks, the data is decompressed on the fly
    --    as it is read in. Known chunks are interpreted and their data
    --    loaded into the descriptor. Unknown chunks are linked into the
    --    ancillary chunks list.
    -- 3. Defilter the uncompressed image data, leaving the result in
    --    a buffer for later use by the pixel access functions. Note
    --    that, for an interlaced image, no de-interlacing is done:
    --    the pixel access functions have to compute the location of
    --    the pixel data taking interlacing into account.

  begin

    if F /= null then
      raise Call_Error; -- File is already open
    end if;

    -- Open the file for reading.

    F := new File_Descriptor;
    Ada.Streams.Stream_IO.Open(F.Handle, Ada.Streams.Stream_IO.In_File, Filename);
    F.Stream := Ada.Streams.Stream_IO.Stream(F.Handle);

    -- Verify that the file really is a PNG file. If the signature in the
    -- first 8 bytes is OK, assume that it is a PNG file. The code below
    -- handles the case where the file has less than 8 bytes, because Read
    -- will return even if there are less than 8 bytes read. Because we
    -- initialise Signature to be zeros, any bytes that are not read from
    -- the file will remain as zeros and will fail to match with the true
    -- signature.

    declare
      Signature : Ada.Streams.Stream_Element_Array(1 .. 8) := (others => 0);
      Last      : Ada.Streams.Stream_Element_Offset;
    begin
      Ada.Streams.Stream_IO.Read (F.Handle, Signature, Last);
      if Signature /= Base.Signature then
        Close(F);
        raise Signature_Error;
      end if;
    end;

    -- Read in the rest of the file, which is structured into chunks, starting
    -- with a 4-byte chunk length field (most significant byte first), then the
    -- chunk name (4 bytes), the chunk data (variable length, from zero bytes
    -- upward), and finally the 4-byte CRC.

    Read_Chunks_Unused:
    declare

      -- The need to read 4 bytes and interpret them as an unsigned 32-bit
      -- integer arises often. Therefore, we define functions to do it, in
      -- two steps: first, to read the 4 bytes from the file into a buffer;
      -- second, to convert a 4-byte buffer into an Unsigned_32. Doing it
      -- in two steps allows us to read 32-bit values direct from the file,
      -- or from a buffer that has already been read in.

      function To_Unsigned_32(B : Buffer_4) return Interfaces.Unsigned_32 is
        L : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(B(1));
        use type Interfaces.Unsigned_32;
      begin
        for I in Ada.Streams.Stream_Element_Offset'(2) .. 4 loop
          L := Interfaces.Shift_Left(L, 8) or Interfaces.Unsigned_32(B(I));
        end loop;
        return L;
      end To_Unsigned_32;


      function Read_Unsigned_32 return Interfaces.Unsigned_32 is
        B : Buffer_4;
        L : Ada.Streams.Stream_Element_Count;
      begin
        Ada.Streams.Stream_IO.Read (F.Handle, B, L);
        if L /= B'last then
          raise End_Error;
        end if;
        return To_Unsigned_32(B);
      end Read_Unsigned_32;


      function Read_Chunk return Buffer_Pointer is

        -- A function to read the next chunk from the file into a
        -- dynamically allocated buffer, and verify its CRC.

        -- The chunk length field in the chunk represents the size of the
        -- data in the chunk. We are about to read the chunk type (4 bytes)
        -- as well as the data, so we must add 4 to the length read from the file.

        Chunk_Length : constant Interfaces.Unsigned_32 := Read_Unsigned_32;
        use type Interfaces.Unsigned_32;

        BP : Buffer_Pointer := new Buffer(1 .. Ada.Streams.Stream_Element_Count(Chunk_Length + 4));
        B  : Buffer renames BP.all;
      begin

        -- Read the chunk into the buffer. Note that if the chunk length was zero,
        -- we will read in 4 bytes (the chunk type).

        declare
          L : Ada.Streams.Stream_Element_Count;
        begin
          Ada.Streams.Stream_IO.Read (F.Handle, B, L);
          if L /= B'last then
            raise End_Error;
          end if;
        end;

        -- Check the CRC. This covers the chunk type and the chunk data, i.e.
        -- the whole content of the buffer B. The CRC at the end of the chunk is
        -- the 1's complement of the CRC computed over the chunk type and data.

        declare
          Nchunk_CRC : constant ZLib.Unsigned_32 := ZLib.Unsigned_32(Read_Unsigned_32);
          Buffer_CRC :          ZLib.Unsigned_32 := 0;
          use type ZLib.Unsigned_32;
        begin
          ZLib.CRC32 (Buffer_CRC, B);
          if Buffer_CRC /= Nchunk_CRC then
            raise CRC_Error;
          end if;
        end;

        return BP;
      exception
        when others => Deallocate(BP); raise;
      end Read_Chunk;

      use type Interfaces.Unsigned_8;

    begin

      -- Read the IHDR chunk (which must come next, immediately after the signature).

      declare
        BP : Buffer_Pointer := Read_Chunk;
        B  : Buffer renames BP.all;
        use type Interfaces.Unsigned_32;
      begin

        if B'length /= 17 or else To_Unsigned_32(B(1 .. 4)) /= Base.IHDR then
          Deallocate(BP);
          raise Format_Error;
        end if;

        -- The chunk seems OK so far, so copy the data into
        -- the file descriptor, and deallocate the chunk.

        begin
          F.Width  := Dimension(To_Unsigned_32(B(5 ..  8)));
          F.Height := Dimension(To_Unsigned_32(B(9 .. 12)));
        exception
        when Constraint_Error =>
          raise Format_Error; -- Invalid image dimension in IHDR chunk
        end;

        F.Bit_Depth   := Interfaces.Unsigned_8(B(13));
        F.Colour_Type := Interfaces.Unsigned_8(B(14));
        F.Compression := Interfaces.Unsigned_8(B(15));
        F.Filter      := Interfaces.Unsigned_8(B(16));
        F.Interlace   := Interfaces.Unsigned_8(B(17));

        Deallocate(BP);

      end;

      -- Now check that the values just read in are valid. This is a check
      -- on the validity of the encoder that wrote the PNG file rather than
      -- the integrity of the file, since we have just checked the CRC and
      -- found it to be correct.

      -- Check the colour type and the bit depth together since these are inter-related.

      declare

        procedure Verify (V : in Boolean) with Inline is
        begin
          if not V then
            raise Format_Error; -- Invalid combination of colour type and bit depth in IHDR chunk
          end if;
        end Verify;

        T : Interfaces.Unsigned_8 renames F.Colour_Type;
        D : Interfaces.Unsigned_8 renames F.Bit_Depth;

      begin
        if T = 0 then Verify(D = 1 or D = 2 or D = 4 or D = 8 or D = 16);
        elsif T = 2 then Verify(                           D = 8 or D = 16);
        elsif T = 3 then Verify(D = 1 or D = 2 or D = 4 or D = 8          );
        elsif T = 4 then Verify(                           D = 8 or D = 16);
        elsif T = 6 then Verify(                           D = 8 or D = 16);
        else
          raise Format_Error; -- Invalid colour type in IHDR chunk
        end if;
      end;

      if F.Compression /= 0 or F.Filter /= 0 or F.Interlace > 1 then
        raise Format_Error; -- Illegal compression, filter or interlace value in IHDR chunk
      end if;

      -- We are now ready to read the chunks. If the image is of colour
      -- type 3 we are looking for the PLTE chunk before the first IDAT
      -- chunk, otherwise we are looking for an IDAT chunk first.
      -- There may be other chunks present after the IDAT chunks, which must
      -- be ancillary chunks (ignored here, although their CRCs are checked).

      -- From version 4.0 of PNG_IO, the compressed data in IDAT chunks and
      -- zTXt chunks is decompressed on-the-fly. That is, it is read from the
      -- buffer containing the chunk data, and directly decompressed from there
      -- to the uncompressed data buffer in the descriptor. This behaviour may change for
      -- zTXt chunks, mirroring the way that iTXt chunks are handled from version 5.0 on.

      declare
        PLTE_Flag,
        IDAT_Flag           : Boolean := False;
        Previous_Chunk_Type : Interfaces.Unsigned_32 := 0;

        -- We verify the first two bytes of the IDAT stream (and of any zTXt streams too).
        -- Since IDAT chunks of length 1 (and even 0!) are legal, we can't guarantee that
        -- the first two bytes will be found in the first IDAT chunk (or even in consecutive
        -- IDAT chunks, since zero length IDAT chunks could occur). Therefore we may have to
        -- save the value of the first byte and check the two bytes only when we have read
        -- the second.

        IDAT_1 : Ada.Streams.Stream_Element := 0; -- To store the first byte of the IDAT stream.
        IDAT_V : Boolean := False;                -- Set True when we have seen the second byte and tested.

        procedure Validate_Zlib_Stream (CMG, FLG : in Ada.Streams.Stream_Element) is
        begin
          if not Valid_Zlib_Header(CMG, FLG) then
            raise Format_Error;
          end if;
        end Validate_Zlib_Stream;

        Z : ZLib.Filter_Type; -- This is used for the IDAT decompression.

        use type Interfaces.Unsigned_32;

      begin

        -- Allocate a buffer for the uncompressed image data in the IDAT chunks. The size of
        -- this buffer is exactly that needed for the uncompressed pixel data, which we can
        -- compute from the image parameters already read from the IHDR chunk.

        F.Uncompressed_Data := new Buffer(1 .. Image_Size(Colour_Type(F),
                                                          Bit_Depth(F),
                                                          Width(F), Height(F),
                                                          Interlaced(F)));

        For_Each_Chunk_Unused:
        while Previous_Chunk_Type /= Base.IEND loop -- Read all the chunks, including IEND.
          declare
            BP : Buffer_Pointer := Read_Chunk;
            B  : Buffer renames BP.all;

            Chunk_Type   : constant Interfaces.Unsigned_32 := To_Unsigned_32(B(1 .. 4));
            Chunk_Length : constant Ada.Streams.Stream_Element_Count := B'length - 4;

            procedure Confirm_Chunk_Length(L : in Ada.Streams.Stream_Element_Count) is
            begin
              if Chunk_Length /= L then
                raise Format_Error; -- Incorrect chunk length in chunk
              end if;
            end;

          begin -- For_Each_Chunk
            case Chunk_Type is
            when Base.PLTE =>

              -- We have to check here that:
              -- 1. The length of the chunk is divisible by 3,
              --    and that there are between 1 and 256 entries.
              -- 2. The image is a colour image.
              -- 3. There have been no previous PLTE chunks
              --    (because only one is allowed).
              -- 4. There have been no IDAT chunks (because PLTE
              --    must precede IDAT if it occurs).

              if Chunk_Length rem 3 /= 0
              or Chunk_Length > 768
              or Chunk_Length < 3 then
                raise Format_Error; -- Illegal length in PLTE chunk
              end if;
              if (F.Colour_Type and 16#02#) = 0 then
                raise Format_Error; -- Illegal PLTE chunk in greyscale PNG
              end if;
              if PLTE_Flag then
                raise Format_Error; -- Illegal multiple PLTE chunks
              end if;
              if IDAT_Flag then
                raise Format_Error; -- Illegal PLTE chunk after IDAT chunk(s)
              end if;
              PLTE_Flag := True;

              -- Allocate a palette and copy the colour palette data to it
              -- from the chunk buffer.

              F.Palette := new Colour_Palette(Interfaces.Unsigned_8(Chunk_Length/3 - 1));
              for X in F.Palette.R'range loop
                declare
                  Y : constant Ada.Streams.Stream_Element_Offset := 3 * Ada.Streams.Stream_Element_Offset(X);
                begin
                  F.Palette.R(X) := Interfaces.Unsigned_8(B(Y + 5));
                  F.Palette.G(X) := Interfaces.Unsigned_8(B(Y + 6));
                  F.Palette.B(X) := Interfaces.Unsigned_8(B(Y + 7));
                end;
              end loop;

            when Base.IDAT =>

              -- We have to check here that:
              -- 1. If this is the first IDAT chunk, a PLTE has been
              --    seen IF REQUIRED.
              -- 2. If this is not the first IDAT chunk, the previous
              --    chunk was also an IDAT (because other chunks are
              --    not allowed in between successive IDAT chunks).

              if (not PLTE_Flag) and Palette(F) then
                raise Format_Error; -- Missing PLTE chunk before IDAT chunk(s)
              end if;
              if IDAT_Flag and Previous_Chunk_Type /= Base.IDAT then
                raise Format_Error; -- IDAT chunks not consecutive
              end if;

              -- All seems OK. Proceed with decompressing the data.

              Decompress_Unused:
              declare
                U    : Buffer renames F.Uncompressed_Data.all;
                I, O : Ada.Streams.Stream_Element_Offset;
              begin
                if Chunk_Length /= 0 then -- If this is a zero length IDAT, skip.
                  if not IDAT_Flag then

                    -- This is the first IDAT chunk (or the first of non-zero length!).

                    ZLib.Inflate_Init(Z); -- Initialise the Zlib decompressor.

                    IDAT_Flag := True; -- Note that we have seen the first IDAT, and
                                       -- therefore that the Zlib decompressor has been
                                       -- intialised (and must be closed if we exit due
                                       -- to an exception).

                    if Chunk_Length > 1 then

                      -- We can check the first two bytes now. There is no need to store the
                      -- first one.

                      Validate_Zlib_Stream(B(5), B(6));
                      IDAT_V := True;
                    else

                      -- We have to store the first byte, and check the second one later when
                      -- we get another chunk with non-zero length.

                      IDAT_1 := B(5);
                    end if;

                    -- This is the first IDAT chunk so we can use the whole of the
                    -- uncompressed data buffer (we may need to, if this is the only
                    -- IDAT chunk). If there are other IDAT chunks, then this call
                    -- to Zlib will not fill the uncompressed data buffer. If this
                    -- chunk has length one, we will pass only the first header byte
                    -- into Zlib (allowed).

                    ZLib.Translate (Filter   => Z,
                                    In_Data  => B(5 .. B'last),
                                    In_Last  => I,
                                    Out_Data => U,
                                    Out_Last => O,
                                    Flush    => ZLib.No_Flush);
                  else

                    -- This is not the first (non-zero length) IDAT chunk, so we need to
                    -- get Zlib to store the decompressed data after any data that is already
                    -- in the uncompressed data buffer. We also have to check the first
                    -- two bytes if we have not done so already.

                    if not IDAT_V then
                      Validate_Zlib_Stream (IDAT_1, B(5));
                      IDAT_V := True;
                    end if;

                    ZLib.Translate (Filter   => Z,
                                    In_Data  => B(5 .. B'last),
                                    In_Last  => I,
                                    Out_Data => U(Ada.Streams.Stream_Element_Count(ZLib.Total_Out(Z)) + 1 .. U'last),
                                    Out_Last => O,
                                    Flush    => ZLib.No_Flush);
                  end if;

                  pragma Assert(I = B'last); -- There doesn't seem to be any similar
                                             -- check we can do on the value of O.
                end if;
              end Decompress_Unused;

            when Base.IEND =>
              Confirm_Chunk_Length(0);
              -- We do not check here whether there is any more
              -- data in the file after the IEND chunk, even
              -- though the presence of such data would make the
              -- PNG file which we are reading non-compliant.
              -- The compliance conditions on PNG decoders are
              -- given in section 15.2.3 of the W3C standard,
              -- and these compliance conditions do not require
              -- a decoder to raise an error.
              Ada.Streams.Stream_IO.Close (F.Handle);
              if not IDAT_Flag then
                raise Format_Error; -- No IDAT chunks in file
              end if;

            when Base.CHRM =>
              Confirm_Chunk_Length(32);
              if IDAT_Flag or PLTE_Flag then
                raise Format_Error; -- cHRM chunk after IDAT/PLTE
              elsif F.Chroma then
                raise Format_Error; -- Multiple cHRM chunks
              end if;
              F.Chroma := True;
              F.White_X := To_Unsigned_32(B( 5 ..  8));
              F.White_Y := To_Unsigned_32(B( 9 .. 12));
              F.Red_X   := To_Unsigned_32(B(13 .. 16));
              F.Red_Y   := To_Unsigned_32(B(17 .. 20));
              F.Green_X := To_Unsigned_32(B(21 .. 24));
              F.Green_Y := To_Unsigned_32(B(25 .. 28));
              F.Blue_X  := To_Unsigned_32(B(29 .. 32));
              F.Blue_Y  := To_Unsigned_32(B(33 .. 36));

            when Base.GAMA =>
              Confirm_Chunk_Length(4);
              if IDAT_Flag or PLTE_Flag then
                raise Format_Error; -- gAMA chunk after IDAT/PLTE
              elsif F.Gamma then
                raise Format_Error; -- Multiple gAMA chunks
              end if;
              F.Gamma := True;
              F.Gamma_Value := To_Unsigned_32(B(5 .. 8));

            when Base.SRGB =>
              Confirm_Chunk_Length(1);
              if IDAT_Flag or PLTE_Flag then
                raise Format_Error; -- sRGB chunk after IDAT/PLTE
              elsif F.SRGB then
                raise Format_Error; -- Multiple sRGB chunks
              end if;
              F.SRGB := True;
              declare
                B5 : Ada.Streams.Stream_Element renames B(5);
              begin
                if B5 > Rendering_Intent'pos(Rendering_Intent'last) then
                  raise Format_Error; -- sRGB chunk has bad value
                end if;
                F.Rendering := Rendering_Intent'val(B5);
              end;

            when Base.PHYS =>
              Confirm_Chunk_Length(9);
              if IDAT_Flag then
                raise Format_Error; -- pHYs chunk after IDAT
              elsif F.Physical then
                raise Format_Error; -- Multiple pHYs chunks
              end if;
              F.Physical  := True;
              F.Phys_X    := To_Unsigned_32(B(5 ..  8));
              F.Phys_Y    := To_Unsigned_32(B(9 .. 12));
              F.Phys_Unit := Interfaces.Unsigned_8(B(13));
              if (F.Phys_Unit and 16#FE#) /= 0 then
                raise Format_Error; -- Bad unit specifier in pHYs chunk
              end if;

            when Base.TEXT | Base.ZTXT =>

              -- Both types of chunk can be dealt with here, because
              -- the only difference in content is that the text (not
              -- the keyword) in a zTXt chunk is compressed and there
              -- is an extra null byte indicating deflate/inflate
              -- compression method following the null separator after
              -- the keyword text. Therefore the only extra step in
              -- reading a zTXt chunk is to check the extra null and
              -- decompress the text.

              F.Number_Of_Texts := F.Number_Of_Texts + 1;

              -- Create a new item at the head of the list. This means
              -- the list is in reverse order to what is found in the
              -- file. This is corrected when the strings are supplied
              -- to the user.

              F.Text_Strings := new Text_Item'(null, null, F.Text_Strings);

              -- Read the keyword string, verifying that the characters in
              -- the string are allowed characters. (ISO standard, Section
              -- 11.3.4.2).

              declare
                K : String(1 .. 80); -- The keyword string is always less than
                                     -- 80 characters in length, but we need to
                                     -- allow space for a NUL at the end.
                use Ada.Characters.Latin_1;
              begin
                for I in K'range loop -- Read the chunk data up to the null.
                  K(I) := Character'val(Natural(B(Ada.Streams.Stream_Element_Offset(I) + 4)));
                  if K(I) = NUL then

                    -- We have reached the end of the keyword string. Check
                    -- that it has no leading or trailing spaces, or consecutive
                    -- spaces.

                    if I = 1       -- The keyword must be at least one character.
                    or else K(1)     = Space -- Leading spaces are not permitted.
                    or else K(I - 1) = Space -- Trailing spaces ditto.
                    then
                      raise Format_Error; -- Illegal use of spaces in text chunk keyword
                    end if;
                    F.Text_Strings.Keyword := new String'(K(1 .. I - 1));
                    exit;
                  end if;

                  -- The character just read was not a NUL, check its legality.

                  if K(I) = No_Break_Space then
                    raise Format_Error; -- Illegal non-break space character in text chunk keyword
                  elsif not Ada.Characters.Handling.Is_Graphic(K(I)) then
                    raise Format_Error; -- Illegal non-graphic character in text chunk keyword
                  end if;
                end loop;

                -- The rest of the chunk is either compressed text or plain text.
                -- If it is plain text, all we need to do is copy it to the
                -- descriptor. If it is compressed we need to decompress it and
                -- then copy it to the descriptor.

                declare
                  T : Buffer renames B(F.Text_Strings.Keyword'length + 6 .. B'last);
                begin
                  if Chunk_Type = Base.ZTXT then

                    -- Verify the compression method byte.

                    if T(T'first) /= 0 then
                      raise Format_Error; -- Unknown compression method in zTXt chunk
                    end if;

                    -- Verify the first two bytes of the Zlib stream.

                    Validate_Zlib_Stream(T(T'first + 1), T(T'first + 2));

                    -- Decompress the text. We don't know the size of the
                    -- decompressed text, and assuming that zTXt chunks will not
                    -- contain huge amounts of text, we adopt the kludge of
                    -- allocating a buffer of a fixed multiple of the compressed
                    -- buffer size and hope for the best.
                    -- Pity the PNG design team didn't think to include the size
                    -- of the text in the chunk! It would have been easily done
                    -- with a few extra bytes after the compression method.

                    declare
                      DP : Buffer_Pointer := new Buffer(1 .. T'length * 10);
                      D  : Buffer renames DP.all;

                      Z  : ZLib.Filter_Type;

                      I, O : Ada.Streams.Stream_Element_Offset;
                    begin

                      ZLib.Inflate_Init (Z);
                      ZLib.Translate (Filter   => Z,
                                      In_Data  => T(T'first + 1 .. T'last),
                                      In_Last  => I,
                                      Out_Data => D,
                                      Out_Last => O,
                                      Flush    => ZLib.Finish);

                      pragma Assert(I = T'last); -- There doesn't seem to be any similar
                                                 -- check we can do on the value of O.
                      ZLib.Close (Z);

                      -- Copy the text out of the D buffer into the text item.
                      -- TODO Surely we could do this with a sliced assignment + type conversion?

                      F.Text_Strings.Text_String := new String(1 .. Natural(O));
                      for I in F.Text_Strings.Text_String'range loop
                        F.Text_Strings.Text_String(I) := Character'val(D(Ada.Streams.Stream_Element_Offset(I)));
                      end loop;

                      Deallocate(DP);
                    end;
                  elsif Chunk_Type = Base.TEXT then

                    -- Copy the text out of the T buffer into the text item.

                    F.Text_Strings.Text_String := new String(1 .. T'length);
                    for I in F.Text_Strings.Text_String'range loop
                      F.Text_Strings.Text_String(I)
                        := Character'val(T(T'first - 1 + Ada.Streams.Stream_Element_Offset(I)));
                    end loop;
                  else
                    raise Program_Error;
                  end if;

                end;
              end;

            when Base.ITXT =>

              -- Unlike the tEXt and zTXt chunks handled above, we do not interpret the
              -- chunk content here, but just store the chunk into a list linked from the
              -- descriptor, leaving interpretation of the data until the point where it
              -- is read by a user-callable function. This is a model that may be adopted
              -- in a future revision for the tEXt and zTXt chunks, and perhaps others.

              F.Number_Of_Itexts := F.Number_Of_Itexts + 1;

              declare
                Temp : constant Chunk_List := new Chunk_List_Element(Chunk_Length);
                C    : Chunk renames Temp.Chnk;
              begin
                C.Data := B(5 .. B'last);

                -- Find the end of the linked list of iTXt chunks and add the new chunk at
                -- the end (NB this is the reverse of the way the tEXt and zTXt chunks are
                -- linked - revision of the code for those chunks should match this code).

                if F.Itext_Chunks = null then
                  F.Itext_Chunks := Temp;
                else
                  declare
                    L : Chunk_List := F.Itext_Chunks;
                  begin
                    while L.Link /= null loop
                      L:= L.Link;
                    end loop;
                    L.Link := Temp;
                  end;
                end if;
              end;

            when others =>
              raise Format_Error; -- Unknown critical chunk
            end case;
            Previous_Chunk_Type := Chunk_Type;
            Deallocate(BP);
          exception
          when others =>
            Deallocate(BP);
            raise;
          end;
        end loop For_Each_Chunk_Unused;

        if IDAT_Flag then

          -- We have seen one or more IDAT chunks and therefore the Zlib filter will have been
          -- initialised/opened, and we must close it. Check also that all the IDAT data has
          -- come out of ZLib.

          declare
          begin
            if ZLib.Stream_End(Z) and ZLib.Total_Out(Z) = F.Uncompressed_Data.all'length then
              ZLib.Close(Z);
              IDAT_Flag := False; -- This indicates that the Zlib filter has been closed.
            else
              raise Format_Error; -- IDAT data has not been correctly decompressed
            end if;
          end;
        end if;
      exception
        when others => if IDAT_Flag then ZLib.Close(Z); end if;
                       raise;
      end;
    end Read_Chunks_Unused;

    -- The uncompressed data must now be processed to extract the pixel
    -- values. This requires defiltering (the inverse of the scanline-based
    -- filtering operation carried out when the PNG file was created.
    -- The defiltering can be done in place in the buffer, because once a
    -- pixel has been filtered the unfiltered value is no longer needed.
    -- There is a slight trick required to cope with the first line and
    -- first column, where pixels to the left or before the top of the
    -- image are defined to be zero for filtering.

    Defilter_Unused:
    declare

      Data   : Ada.Streams.Stream_Element_Array renames F.Uncompressed_Data.all;

      procedure Check_Filter_Type(B : in Ada.Streams.Stream_Element) with Inline is
      begin
        if B > 4 then
          raise Format_Error; -- Illegal filter type byte
        end if;
      end Check_Filter_Type;

      -- We need to know the number of bytes per pixel, which is defined
      -- to be 1 for bit depths of 1, 2, or 4. This value is used in
      -- accessing the previous scanline byte at the same column position.

      Bpp : constant Natural := Bytes_Per_Pixel(Colour_Type(F), Bit_Depth(F));

      -- To allow for the case of filtering sub-images in interlaced
      -- PNG files we provide a procedure to deal with all cases. The
      -- parameters are the number of lines to be filtered, the number
      -- of pixels in each line (excluding the filter type byte) and
      -- the index in the buffer of the filter type byte of the first line.

      Pointer : Ada.Streams.Stream_Element_Offset := 1; -- The index in the buffer of the
                                                        -- byte currently being filtered.

      procedure Pass(N_Lines, N_Pixels : in Natural) is
        -- We need to calculate the number of pixel bytes per line, including
        -- an allowance for unused bits at the end of the line if there are
        -- less than 8 bits per pixel.
        W : constant Natural := (N_Pixels * Bits_Per_Pixel(Colour_Type(F), Bit_Depth(F)) + 7)/8;
      begin
        for Y in 1 .. N_Lines loop -- We have to allow for the possibility that a pass
                                   -- has zero columns or rows as described in Section
                                   -- 8.2 of the ISO standard. If N_Lines is zero this
                                   -- loop will execute zero times and Pass will do nothing.
          exit when W = 0;         -- If W is zero we need to skip the code in the loop.
          declare
            -- P is the index of the first byte of the previous scanline, except when Y = 1.
            P  : constant Ada.Streams.Stream_Element_Offset :=
                          Ada.Streams.Stream_Element_Offset'max(1, Pointer - Ada.Streams.Stream_Element_Offset(W + 1));
            FT : Ada.Streams.Stream_Element renames Data(Pointer);
          begin
            Check_Filter_Type(FT);
            Pointer := Pointer + 1;   -- Skip the filter type byte.
            for X in 1 .. W loop      -- For each byte to be filtered.

              -- The code in this loop should be recoded to use a function
              -- to operate on each scanline as is done for the write PNG
              -- part of the package which was written later in a much more
              -- hygienic manner!

              declare
                Current : Ada.Streams.Stream_Element renames Data(Pointer);

                function Previous return Ada.Streams.Stream_Element is
                  (if X <= Bpp then 0
                               else Data(Pointer - Ada.Streams.Stream_Element_Offset(Bpp)));

                function Prior return Ada.Streams.Stream_Element is
                  (if Y = 1 then 0 else Data(P + Ada.Streams.Stream_Element_Offset(X)));

                function Prev_Prior return Ada.Streams.Stream_Element is
                  (if X <= Bpp or Y = 1 then 0
                                        else Data(P + Ada.Streams.Stream_Element_Offset(X - Bpp)));
              begin
                case FT is
                  when None    => null; -- There is no filtering to do.
                  when Sub     => Current := Current + Previous;
                  when Up      => Current := Current + Prior;
                  when Average => Current := Current + Mean (Previous, Prior);
                  when Paeth =>
                       Current := Current + Paeth_Predictor (Previous, Prior, Prev_Prior);
                  when others  =>
                    raise Program_Error; -- Can't happen.
                end case;
              end;
              Pointer := Pointer + 1; -- Skip the byte just filtered.
            end loop;
          end;
        end loop;
      end Pass;

    begin -- Defilter
      case Interlaced(F) is
        when False =>
          Pass(Height(F), Width(F)); -- There is only one pass.
        when True =>

          -- We have a tricky calculation to do here. We have to calculate
          -- from the actual dimension of the image (width or height) the
          -- dimension of the reduced image on each pass. We do this from
          -- tables in the Adam7 package.

          for P in Adam7.Pass_Number loop
            F.Interlace_Offsets(P) := Pointer; -- Remember for use in reading
                                               -- the pixels later.
            Pass (Adam7.Sub_Image_Height(Height(F), P),
                  Adam7.Sub_Image_Width (Width (F), P));
          end loop;
      end case;

      if Pointer /= F.Uncompressed_Data.all'length + 1 then
        raise Format_Error; -- Incorrect length of uncompressed image data
      end if;
    end Defilter_Unused;

  exception
  when others =>
    if F /= null then
      Close (F);
    end if;
    raise;
  end Open;


  procedure Close (F : in out File) is
    -- Called by the user but also in the event of an exception during Open.
    procedure Deallocate is new Ada.Unchecked_Deallocation(Colour_Palette, Palette_Pointer);
    procedure Deallocate is new Ada.Unchecked_Deallocation(Text_Item, Text_Item_Pointer);
    procedure Deallocate is new Ada.Unchecked_Deallocation(File_Descriptor, File);
    procedure Deallocate is new Ada.Unchecked_Deallocation(Chunk_List_Element, Chunk_List);
  begin
    if F = null then
      -- The file is not open or has not been opened (it might already have been closed.)
      -- The standard packages such as Ada.Direct_IO raise Status_Error in this situation
      -- so we do the same here. We cannot allow this to go undetected, otherwise we will
      -- get exceptions in the following code when we attempt to access elements of the
      -- record (not) accessed by F.
      raise Status_Error; -- Attempt to close non-open file
    end if;
    -- Deallocate has no effect if the access value is null. LRM 13.11.2(8)
    Deallocate(F.Palette);
    Deallocate(F.Uncompressed_Data);
    while F.Ancillary_Chunks /= null loop
      declare
        Temp : constant Chunk_List := F.Ancillary_Chunks.Link;
      begin
        Deallocate(F.Ancillary_Chunks); F.Ancillary_Chunks := Temp;
      end;
    end loop;
    while F.Text_Strings /= null loop
      declare
        Temp : constant Text_Item_Pointer := F.Text_Strings.Link;
      begin
        Deallocate(F.Text_Strings); F.Text_Strings := Temp;
      end;
    end loop;
    while F.Itext_Chunks /= null loop
      declare
        Temp : constant Chunk_List := F.Itext_Chunks.Link;
      begin
        Deallocate(F.Itext_Chunks); F.Itext_Chunks := Temp;
      end;
    end loop;
    if Ada.Streams.Stream_IO.Is_Open (F.Handle) then
      Ada.Streams.Stream_IO.Close (F.Handle);
    end if;
    Deallocate(F);
  end Close;


  function Pixel_Index (F    : File;
                        R, C : Coordinate) return Ada.Streams.Stream_Element_Offset with Inline is
    -- A function to compute the index in the uncompressed data buffer
    -- of the first byte containing the pixel at position R, C. Note
    -- that the pixel itself may occupy more or less than a whole byte.
  begin
    Check(F); -- Make sure F exists.
    if R + 1 > F.Height or C + 1 > F.Width then
      raise Constraint_Error; -- Coordinate(s) out of range
    end if;

    declare
      function Index (R, C : Coordinate;
                      W    : Dimension) return Ada.Streams.Stream_Element_Offset with Inline is
        -- Computes the index of the desired byte within the image or sub-image.
        CT  : constant Colour_Type_Code := Colour_Type (F);
        BD  : constant            Depth := Bit_Depth (F);
        Bpp : constant         Positive := Bits_Per_Pixel (CT, BD);
      begin
        return 1
          + Ada.Streams.Stream_Element_Offset(R) * Ada.Streams.Stream_Element_Offset(Bytes_Per_Scanline(CT, BD, W) + 1)
          + 1 + (Ada.Streams.Stream_Element_Offset(C) * Ada.Streams.Stream_Element_Offset(Bpp))/8;
      end Index;

      W : constant Dimension := Width(F);

    begin -- Pixel_Index
      if not Interlaced(F) then
        return Index(R, C, W);
      else
        declare
          P : constant Adam7.Pass_Number := Adam7.Pass (R, C);
        begin
          return F.Interlace_Offsets(P) - 1
               + Index(Adam7.Sub_Image_Row (R, C), Adam7.Sub_Image_Col (R, C), Adam7.Sub_Image_Width (W, P));
        end;
      end if;
    end;
  end Pixel_Index;


  -- Low-level functions to fetch the byte/word at a given coordinate in the image.
  -- The offset allows for colour and alpha bytes/words to be fetched.

  function U8 (F      : File;
               R, C   : Coordinate;
               Offset : Ada.Streams.Stream_Element_Offset := 0) return Interfaces.Unsigned_8 with Inline is
  begin
    return Interfaces.Unsigned_8(F.Uncompressed_Data(Pixel_Index(F, R, C) + Offset));
  end U8;


  function U16 (F      : File;
                R, C   : Coordinate;
                Offset : Ada.Streams.Stream_Element_Offset := 0) return Interfaces.Unsigned_16 with Inline is
    P : constant Ada.Streams.Stream_Element_Offset := Pixel_Index(F, R, C) + Offset;
    D : Stream_Element_Buffer renames F.Uncompressed_Data.all;
    use type Interfaces.Unsigned_16;
  begin
    return Interfaces.Shift_Left (Interfaces.Unsigned_16(D(P)), 8) or Interfaces.Unsigned_16(D(P + 1));
  end U16;


  -- Low-level function to fetch bits, half nibbles or nibbles at a given coordinate
  -- in the image. Since the leftmost pixels are in the high order bits of the byte
  -- ISO standard, Section 7.2), we have to reverse the position by subtraction from 7/3/1.
  -- Modified 18 July 2000 to change BD from Depth_1_2_4 to Unsigned_8 for efficiency.

  -- Because the following functions will be called a very large number of
  -- times there is limited error checking of the values in order not to
  -- slow down image reading. These functions were modified 18 July 2000 to
  -- remove calls to Bit_Depth and replace these with F.Bit_Depth because
  -- this is more efficient.

  function Alpha_Value (F    : File;
                        R, C : Coordinate) return Natural is
    -- This function may be called for a Type 4 or Type 6 PNG.  Prior to
    -- version 1.3, this function gave incorrect values for Type 4 PNGs
    -- because it did not take account of the PNG type and the offsets
    -- supplied to U8/16 were for Type 6 RGBA images.
  begin
    case F.Colour_Type is
    when 4 =>
      case F.Bit_Depth is
      when 8 =>
        return Natural(U8 (F, R, C, 1));
      when 16 =>
        return Natural(U16(F, R, C, 2));
      when others =>
        raise  Call_Error;
      end case;
    when 6 =>
      case F.Bit_Depth is
      when 8 =>
        return Natural(U8 (F, R, C, 3));
      when 16 =>
        return Natural(U16(F, R, C, 6));
      when others =>
        raise  Call_Error;
      end case;
    when others =>
      raise Call_Error;
    end case;
  end Alpha_Value;

end Png;
