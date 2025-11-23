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

with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Raw_Interface;
with System.Storage_Elements;
with Log;

package body Raw is

  package C  renames Interfaces.C;
  package RI renames Raw_Interface;
  package SE renames System.Storage_Elements;

  -------------------
  -- Local helpers --
  -------------------

  procedure Raise_With (Msg : String) is
  begin
    raise Raw_Error with Msg;
  end Raise_With;

  procedure Check (Code : C.int; Where : String) is
    use type C.int;
  begin
    if Code /= 0 then
      Raise_With (Where & " failed (LibRaw error code " & Integer'image (Integer (Code)) & ")");
    end if;
  end Check;

  function To_C_String (S : String) return String is
  begin
    return S & Ada.Characters.Latin_1.NUL;
  end To_C_String;

  type RGB is (Green, Unused_Red, Unused_Blue) with Size => 2;

  --  One RAW mosaic sample
  type Raw_Pixel is record
    Value : Pixel;
    Color : RGB;
  end record with Size => 16; -- Bits
  for Raw_Pixel use record
    Value at 0 range 0 .. Pixel_Size - 1;
    Color at 0 range Pixel_Size .. Pixel_Size + 1;
  end record;

  --  Access type to a single raw pixel (used via unchecked conversion).
  type Raw_Pixel_Access is access all Raw_Pixel
    with Convention => C;

  function To_Raw_Pixel_Access is new Ada.Unchecked_Conversion
    (Source => System.Address,
     Target => Raw_Pixel_Access);

  -------------
  -- Grid_Of --
  -------------
  function Grid_Of (File_Name : String;
                    Size      : Square_Size) return Green_Grid
  is
    Ctx       : constant RI.Context := RI.Init (0);
    File_C    : aliased String := To_C_String (File_Name);
    Err       : C.int;
    Raw_W     : C.int;
    Raw_H     : C.int;
    Raw_Base  : System.Address;

    use type SE.Storage_Offset;

    function Raw_Base_Address return System.Address is

      Offset : constant SE.Storage_Offset := 312;

      type Address_Access is access all System.Address;

      function Convert is new Ada.Unchecked_Conversion (RI.Context, System.Address);
      function Convert is new Ada.Unchecked_Conversion (System.Address, Address_Access);

      Pointer : constant Address_Access := Convert (Convert (Ctx) + Offset);

    begin -- Raw_Base_Address
      Log.Write ("Raw_Base_Address:" & Pointer.all'image);
      return Pointer.all;
    end Raw_Base_Address;

    -- Ranges for the resulting grid.
    subtype Row_Index    is Positive range 1 .. Size;
    subtype Column_Index is Positive range 1 .. Size / 2;

    -- Result grid: rows x (greens per row).
    Result : Green_Grid (Row_Index, Column_Index) := [others => [others => 0]];

    use type RI.Context;
    use type C.int;
    use type System.Address;

  begin -- Grid_Of
    if Ctx = RI.Null_Context then
      Raise_With ("Grid_Of: libraw_init returned NULL context");
    end if;

    declare
    begin
      --  1) Open file.
      Err := RI.Open_File (Ctx, File_C'address);
      Check (Err, "libraw_open_file");

      --  2) Unpack RAW mosaic.
      Err := RI.Unpack (Ctx);
      Check (Err, "libraw_unpack");

      --  3) Get RAW mosaic dimensions.
      Raw_W := RI.Get_Raw_Width (Ctx);
      Raw_H := RI.Get_Raw_Height (Ctx);
      Log.Write ("Raw.W:" & Raw_W'image);
      Log.Write ("Raw.H:" & Raw_H'image);
      if Raw_W <= 0 or else Raw_H <= 0 then
        Raise_With ("Grid_Of: invalid RAW dimensions");
      end if;

      if C.int(Size) > Raw_W or else C.int(Size) > Raw_H then
        Raise_With
          ("Grid_Of: Square_Size exceeds RAW dimensions ("
           & Integer'image (Integer (Raw_W)) & " x"
           & Integer'image (Integer (Raw_H)) & ")");
      end if;

      --  4) Get pointer to raw mosaic (row-major, width = Raw_W).
      Raw_Base := Raw_Base_Address;
      if Raw_Base = System.Null_Address then
        Raise_With ("Grid_Of: NULL RAW pointer");
      end if;

      --  5) Central crop offsets in RAW coordinates (0-based).
      declare
        W  : constant Integer := Integer (Raw_W);
        H  : constant Integer := Integer (Raw_H);
        SS : constant Integer := Integer (Size);

        Row_Offset : constant Integer := (H - SS) / 2;
        Col_Offset : constant Integer := (W - SS) / 2;

      begin
        if Row_Offset < 0 or else Col_Offset < 0 then
          Raise_With ("Grid_Of: negative crop offset, check Square_Size");
        end if;

        --  6) Extract green samples into Result.
        --
        --  Assume RGGB pattern:
        --    Row even (0-based): R G R G ...
        --    Row odd  (0-based): G B G B ...
        --
        --  For each row in the crop:
        --    if global_row0 even: greens at columns 1,3,5,...
        --    if global_row0 odd : greens at columns 0,2,4,...
        --
        --  Linear index in raw array (0-based):
        --    idx = global_row0 * Raw_W + global_col0
        --
        --  Byte offset = idx * sizeof(Raw_Pixel).
        --
        for R in Row_Index loop
          declare
            R0          : constant Integer := R - 1;
            Global_Row0 : constant Integer := Row_Offset + R0;
          begin
            Log.Write ("R0:" & R0'image);
            Log.Write ("Global_Row0:" & Global_Row0'image);
            for C in Column_Index loop
              declare
                C0          : constant Integer := C - 1;
                Col_In_Crop : Integer;
                Global_Col0 : Integer;
                Idx         : Integer;
                Byte_Offset : SE.Storage_Offset;
                Pix_Addr    : System.Address;
                Pix_Ptr     : Raw_Pixel_Access;
              begin
                Log.Write ("C0:" & C0'image);
                --  Column inside the Square_Size-wide crop (0-based).
                if (Global_Row0 mod 2) = 0 then
                  --  Even row: greens at 1,3,5,...
                  Col_In_Crop := 2 * C0 + 1;
                else
                  --  Odd row: greens at 0,2,4,...
                  Col_In_Crop := 2 * C0;
                end if;

                Global_Col0 := Col_Offset + Col_In_Crop;
                Log.Write ("Global_Col0:" & Global_Col0'image);

                if Global_Col0 < 0 or else Global_Col0 >= W then
                  Raise_With ("Grid_Of: computed column outside RAW width");
                end if;

                if Global_Row0 < 0 or else Global_Row0 >= H then
                  Raise_With ("Grid_Of: computed row outside RAW height");
                end if;

                Idx := Global_Row0 * W + Global_Col0;

                --  Compute byte offset from base.
                Byte_Offset := SE.Storage_Offset (Idx) * SE.Storage_Offset(Raw_Pixel'size / System.Storage_Unit);
                Log.Write ("Byte_Offset:" & Global_Col0'image);

                Pix_Addr := Raw_Base + Byte_Offset;
                Pix_Ptr  := To_Raw_Pixel_Access (Pix_Addr);
                Log.Write ("Colour: " & Pix_Ptr.Color'image);
                --if Pix_Ptr.Color /= Green then
                --  raise Program_Error;
                --end if;
                Result (R, C) := Pix_Ptr.Value;
              end;
            end loop;
          end;
        end loop;
      end;

      --  Clean up LibRaw context.
      RI.Recycle (Ctx);
      RI.Close   (Ctx);

    exception
    when others =>
      --  Try to clean up, then re-raise.
      begin
        RI.Recycle (Ctx);
      exception
        when others => null;
      end;
      begin
        RI.Close (Ctx);
      exception
        when others => null;
      end;
      raise;
    end;

    return Result;
  end Grid_Of;

end Raw;
