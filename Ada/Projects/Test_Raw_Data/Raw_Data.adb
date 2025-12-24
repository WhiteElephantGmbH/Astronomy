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
--pragma Style_White_Elephant;

with GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with System;

package body Raw_Data is

  package OS renames GNAT.OS_Lib;

  use type Interfaces.Unsigned_8;
  use type Interfaces.Unsigned_16;
  use type Interfaces.Unsigned_32;
  use type OS.File_Descriptor;

  subtype U8  is Interfaces.Unsigned_8;
  subtype U16 is Interfaces.Unsigned_16;
  subtype U32 is Interfaces.Unsigned_32;

  type Endian_Kind is (Little, Big);

  The_Endian      : Endian_Kind := Little;
  The_Raw_Width   : Natural := 0;
  The_Raw_Height  : Natural := 0;
  The_Raw_Bits    : Natural := 0;
  The_Compression : Natural := 0;

  The_Buffer      : Byte_Array_Access := null;
  The_Buffer_Size : Natural := 0;

  procedure Free is new Ada.Unchecked_Deallocation
    (Byte_Array, Byte_Array_Access);

  procedure Clear is
  begin
    if The_Buffer /= null then
      Free (The_Buffer);
    end if;

    The_Buffer := null;
    The_Buffer_Size := 0;

    The_Raw_Width := 0;
    The_Raw_Height := 0;
    The_Raw_Bits := 0;
    The_Compression := 0;
    The_Endian := Little;
  end Clear;

  function Raw_Width return Natural is
  begin
    return The_Raw_Width;
  end Raw_Width;

  function Raw_Height return Natural is
  begin
    return The_Raw_Height;
  end Raw_Height;

  function Raw_Bits return Natural is
  begin
    return The_Raw_Bits;
  end Raw_Bits;

  function Raw_Compression return Natural is
  begin
    return The_Compression;
  end Raw_Compression;

  function Raw_Buffer return Byte_Array_Access is
  begin
    return The_Buffer;
  end Raw_Buffer;

  function Raw_Buffer_Bytes return Natural is
  begin
    return The_Buffer_Size;
  end Raw_Buffer_Bytes;

  type File_Pos is new Long_Integer;

  procedure Seek_Set (FD : OS.File_Descriptor; Pos : File_Pos) is
  begin
    OS.Lseek
      (FD     => FD,
       offset => Long_Integer (Pos),
       origin => OS.Seek_Set);
  end Seek_Set;

  procedure Read_Exact
    (FD   : OS.File_Descriptor;
     Addr : System.Address;
     N    : Positive) is
    Count : Integer;
  begin
    Count := OS.Read (FD => FD, A => Addr, N => N);
    if Count /= N then
      raise Invalid_File;
    end if;
  end Read_Exact;

  function Get_U8 (FD : OS.File_Descriptor) return U8 is
    B : aliased U8;
  begin
    Read_Exact (FD, B'address, 1);
    return B;
  end Get_U8;

  function Get_U16 (FD : OS.File_Descriptor) return U16 is
    B1 : constant U8 := Get_U8 (FD);
    B2 : constant U8 := Get_U8 (FD);
  begin
    if The_Endian = Little then
      return U16 (B1) or Interfaces.Shift_Left (U16 (B2), 8);
    else
      return U16 (B2) or Interfaces.Shift_Left (U16 (B1), 8);
    end if;
  end Get_U16;

  function Get_U32 (FD : OS.File_Descriptor) return U32 is
    B1 : constant U8 := Get_U8 (FD);
    B2 : constant U8 := Get_U8 (FD);
    B3 : constant U8 := Get_U8 (FD);
    B4 : constant U8 := Get_U8 (FD);
  begin
    if The_Endian = Little then
      return U32 (B1)
        or Interfaces.Shift_Left (U32 (B2), 8)
        or Interfaces.Shift_Left (U32 (B3), 16)
        or Interfaces.Shift_Left (U32 (B4), 24);
    else
      return U32 (B4)
        or Interfaces.Shift_Left (U32 (B3), 8)
        or Interfaces.Shift_Left (U32 (B2), 16)
        or Interfaces.Shift_Left (U32 (B1), 24);
    end if;
  end Get_U32;

  -- TIFF tags in Canon RAW IFD
  Tag_ImageWidth      : constant U16 := 16#0100#;  -- 256
  Tag_ImageLength     : constant U16 := 16#0101#;  -- 257
  Tag_BitsPerSample   : constant U16 := 16#0102#;  -- 258 (often absent in CR2 RAW IFD)
  Tag_Compression     : constant U16 := 16#0103#;  -- 259
  Tag_StripOffsets    : constant U16 := 16#0111#;  -- 273
  Tag_StripByteCounts : constant U16 := 16#0117#;  -- 279

  -- TIFF types
  Type_SHORT : constant U16 := 3;
  Type_LONG  : constant U16 := 4;

  type U32_Array is array (Positive range <>) of U32;

  function Type_Size (Typ : U16) return Natural is
  begin
    case Typ is
      when Type_SHORT =>
        return 2;
      when Type_LONG =>
        return 4;
      when others =>
        return 0;
    end case;
  end Type_Size;

  function Read_U32_Array
    (FD   : OS.File_Descriptor;
     Base : File_Pos;
     Typ  : U16;
     Cnt  : U32;
     Val  : U32) return U32_Array is
    N : constant Natural := Natural (Cnt);
    S : constant Natural := Type_Size (Typ);
  begin
    if N = 0 or else S = 0 then
      return [1 => 0];
    end if;

    declare
      A           : U32_Array (1 .. N);
      Bytes_Total : constant Natural := N * S;
    begin
      if Bytes_Total <= 4 then
        if Typ = Type_LONG then
          A (1) := Val;
        elsif Typ = Type_SHORT then
          if N >= 1 then
            A (1) := U32 (U16 (Val and 16#FFFF#));
          end if;
          if N >= 2 then
            A (2) := U32 (U16 (Interfaces.Shift_Right (Val, 16) and 16#FFFF#));
          end if;
        else
          null;
        end if;
        return A;
      end if;

      Seek_Set (FD, Base + File_Pos (Val));

      if Typ = Type_LONG then
        for I in A'range loop
          A (I) := Get_U32 (FD);
        end loop;
      elsif Typ = Type_SHORT then
        for I in A'range loop
          A (I) := U32 (Get_U16 (FD));
        end loop;
      else
        null;
      end if;

      return A;
    end;
  end Read_U32_Array;

  function Read_U32_Value
    (FD   : OS.File_Descriptor;
     Base : File_Pos;
     Typ  : U16;
     Cnt  : U32;
     Val  : U32) return U32 is
    Bytes_Total : constant Natural := Natural (Cnt) * Type_Size (Typ);
  begin
    if Cnt = 0 then
      return 0;
    end if;

    if Bytes_Total <= 4 then
      if Typ = Type_LONG then
        return Val;
      elsif Typ = Type_SHORT then
        return U32 (U16 (Val and 16#FFFF#));
      else
        return 0;
      end if;
    end if;

    Seek_Set (FD, Base + File_Pos (Val));

    if Typ = Type_LONG then
      return Get_U32 (FD);
    elsif Typ = Type_SHORT then
      return U32 (Get_U16 (FD));
    else
      return 0;
    end if;
  end Read_U32_Value;

  -- Parse Lossless JPEG in The_Buffer and extract SOF3 precision/width/height.
  -- Returns True if SOF3 was found and parsed.
  function Parse_SOF3_From_Buffer return Boolean is
    B : constant Byte_Array_Access := The_Buffer;

    function U8_At (Pos : Natural) return U8 is
    begin
      return U8 (B (Pos));
    end U8_At;

    function U16_BE_At (Pos : Natural) return U16 is
    begin
      return Interfaces.Shift_Left (U16 (U8_At (Pos)), 8)
        or U16 (U8_At (Pos + 1));
    end U16_BE_At;

    Pos : Natural := 1;
  begin
    if B = null or else The_Buffer_Size < 4 then
      return False;
    end if;

    -- Find SOI 0xFFD8
    declare
      Found_SOI : Boolean := False;
    begin
      while Pos + 1 <= The_Buffer_Size loop
        if U8_At (Pos) = 16#FF# and then U8_At (Pos + 1) = 16#D8# then
          Found_SOI := True;
          exit;
        end if;
        Pos := Pos + 1;
      end loop;

      if not Found_SOI then
        return False;
      end if;
    end;

    -- Move past SOI
    Pos := Pos + 2;

    -- Scan markers
    while Pos + 3 <= The_Buffer_Size loop
      -- Skip to next 0xFF
      while Pos <= The_Buffer_Size and then U8_At (Pos) /= 16#FF# loop
        Pos := Pos + 1;
      end loop;

      exit when Pos + 1 > The_Buffer_Size;

      -- Skip fill 0xFF bytes
      while Pos <= The_Buffer_Size and then U8_At (Pos) = 16#FF# loop
        Pos := Pos + 1;
      end loop;

      exit when Pos > The_Buffer_Size;

      declare
        Marker : constant U8 := U8_At (Pos);
      begin
        Pos := Pos + 1;

        -- Standalone markers (no length)
        if Marker = 16#D9# or else Marker = 16#DA# then
          -- EOI or SOS: after SOS, entropy-coded data begins; in lossless JPEG
          -- the SOF comes before SOS, so if we reach SOS, stop.
          return False;
        end if;

        -- Need length for most markers
        if Pos + 1 > The_Buffer_Size then
          return False;
        end if;

        declare
          Seg_Len : constant U16 := U16_BE_At (Pos);  -- includes these 2 bytes
        begin
          if Seg_Len < 2 then
            return False;
          end if;

          if Marker = 16#C3# then
            -- SOF3 payload: precision(1), height(2), width(2), components(1), ...
            if Pos + Natural (Seg_Len) - 1 > The_Buffer_Size then
              return False;
            end if;

            declare
              Precision : constant U8  := U8_At (Pos + 2);
              Height    : constant U16 := U16_BE_At (Pos + 3);
              Width     : constant U16 := U16_BE_At (Pos + 5);
            begin
              The_Raw_Bits := Natural (Precision);
              The_Raw_Width := Natural (Width);
              The_Raw_Height := Natural (Height);
              return True;
            end;
          end if;

          -- Skip this segment: length bytes start at Pos, so advance Pos by Seg_Len
          Pos := Pos + Natural (Seg_Len);
        end;
      end;
    end loop;

    return False;
  end Parse_SOF3_From_Buffer;

  procedure Read_Raw_IFD
    (FD         : OS.File_Descriptor;
     Base       : File_Pos;
     RawIFD_Off : U32) is

    Count : U16;

    Strip_Offsets : U32_Array (1 .. 1) := [1 => 0];
    Strip_Counts  : U32_Array (1 .. 1) := [1 => 0];

    Bits_From_Tag : Boolean := False;

  begin
    Seek_Set (FD, Base + File_Pos (RawIFD_Off));
    Count := Get_U16 (FD);

    for Unused_I in 1 .. Integer (Count) loop
      declare
        Tag   : constant U16 := Get_U16 (FD);
        Typ   : constant U16 := Get_U16 (FD);
        Cnt   : constant U32 := Get_U32 (FD);
        ValOr : constant U32 := Get_U32 (FD);
      begin
        case Tag is
          when Tag_ImageWidth =>
            The_Raw_Width := Natural (Read_U32_Value (FD, Base, Typ, Cnt, ValOr));

          when Tag_ImageLength =>
            The_Raw_Height := Natural (Read_U32_Value (FD, Base, Typ, Cnt, ValOr));

          when Tag_BitsPerSample =>
            The_Raw_Bits := Natural (Read_U32_Value (FD, Base, Typ, Cnt, ValOr));
            if The_Raw_Bits /= 0 then
              Bits_From_Tag := True;
            end if;

          when Tag_Compression =>
            The_Compression := Natural (Read_U32_Value (FD, Base, Typ, Cnt, ValOr));

          when Tag_StripOffsets =>
            Strip_Offsets := Read_U32_Array (FD, Base, Typ, Cnt, ValOr);

          when Tag_StripByteCounts =>
            Strip_Counts := Read_U32_Array (FD, Base, Typ, Cnt, ValOr);

          when others =>
            null;
        end case;
      end;
    end loop;

    declare
      Next_IFD : constant U32 := Get_U32 (FD);
    begin
      pragma Unreferenced (Next_IFD);
    end;

    if Strip_Offsets (Strip_Offsets'first) = 0
      or else Strip_Counts (Strip_Counts'first) = 0
    then
      raise Not_Found;
    end if;

    if Strip_Offsets'length /= Strip_Counts'length then
      raise Invalid_File;
    end if;

    -- Allocate one big buffer and concatenate strips
    declare
      Total : U32 := 0;
    begin
      for I in Strip_Counts'range loop
        Total := Total + Strip_Counts (I);
      end loop;

      The_Buffer_Size := Natural (Total);
      The_Buffer := new Byte_Array (1 .. Positive (The_Buffer_Size));

      declare
        Pos : Natural := 1;
      begin
        for I in Strip_Offsets'range loop
          declare
            Off : constant U32 := Strip_Offsets (I);
            Len : constant Natural := Natural (Strip_Counts (I));
          begin
            Seek_Set (FD, Base + File_Pos (Off));
            Read_Exact (FD, The_Buffer (Pos)'address, Positive (Len));
            Pos := Pos + Len;
          end;
        end loop;
      end;
    end;

    -- If BitsPerSample tag is missing/zero and compression indicates lossless JPEG,
    -- detect SOF3 and read precision/width/height from the JPEG stream.
    if (not Bits_From_Tag)
      and then The_Compression = 6
      and then The_Buffer /= null
    then
      declare
        Found : constant Boolean := Parse_SOF3_From_Buffer;
      begin
        pragma Unreferenced (Found);
      end;
    end if;

  end Read_Raw_IFD;

  procedure Read (Filename : String) is
    FD   : OS.File_Descriptor := OS.Invalid_FD;
    Base : constant File_Pos := 0;

    B1, B2 : U8;
    Magic  : U16;
    IFD0   : U32;

    CR_A   : U8;
    CR_B   : U8;
    Major  : U8;
    Minor  : U8;
    RawIFD : U32;

  begin
    Clear;

    FD := OS.Open_Read (Filename, OS.Binary);
    if FD = OS.Invalid_FD then
      raise File_Not_Found;
    end if;

    -- TIFF byte order
    B1 := Get_U8 (FD);
    B2 := Get_U8 (FD);

    if Character'val (Integer (B1)) = 'I'
      and then Character'val (Integer (B2)) = 'I'
    then
      The_Endian := Little;
    elsif Character'val (Integer (B1)) = 'M'
      and then Character'val (Integer (B2)) = 'M'
    then
      The_Endian := Big;
    else
      raise Invalid_File;
    end if;

    -- TIFF magic
    Magic := Get_U16 (FD);
    if Magic /= 16#002A# then
      raise Invalid_File;
    end if;

    -- IFD0 offset (we read it but we don't need it here)
    IFD0 := Get_U32 (FD);
    pragma Unreferenced (IFD0);

    -- CR2 magic "CR"
    CR_A := Get_U8 (FD);
    CR_B := Get_U8 (FD);
    if Character'val (Integer (CR_A)) /= 'C'
      or else Character'val (Integer (CR_B)) /= 'R'
    then
      raise Invalid_File;
    end if;

    -- CR2 version
    Major := Get_U8 (FD);
    Minor := Get_U8 (FD);
    pragma Unreferenced (Major, Minor);

    -- RAW IFD offset
    RawIFD := Get_U32 (FD);
    if RawIFD = 0 then
      raise Not_Found;
    end if;

    Read_Raw_IFD (FD, Base, RawIFD);

    OS.Close (FD);

  exception
    when others =>
      if FD /= OS.Invalid_FD then
        OS.Close (FD);
      end if;
      raise;
  end Read;

end Raw_Data;
