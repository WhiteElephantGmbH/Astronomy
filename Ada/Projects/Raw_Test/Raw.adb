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
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Raw_Interface;
with System.Address_Image;
with System.Storage_Elements;

package body Raw is

  package C  renames Interfaces.C;
  package IO renames Ada.Text_IO;
  package RI renames Raw_Interface;
  package SE renames System.Storage_Elements;

  -------------------
  -- Local helpers --
  -------------------

  procedure Error (Msg : String) is
  begin
    IO.Put_Line ("### " & Msg);
    raise Raw_Error with Msg;
  end Error;

  procedure Check (Code  : C.int;
                   Where : String) is
    use type C.int;
  begin
    if Code /= 0 then
      Error (Where & " failed (LibRaw error code" &
             Integer'image (Integer (Code)) & ")");
    end if;
  end Check;

  function To_C_String (S : String) return String is
  begin
    return S & Ada.Characters.Latin_1.NUL;
  end To_C_String;

  --  Convert an arbitrary address to a pointer to one 8-bit sample.
  type Sample_Ptr is access all Pixel
    with Convention => C;

  function To_Sample_Ptr is new Ada.Unchecked_Conversion
    (Source => System.Address,
     Target => Sample_Ptr);

  -------------
  -- Grid_Of --
  -------------

  function Grid_Of (File_Name : String;
                    Size      : Square_Size) return Green_Grid
  is
    Ctx       : constant RI.Context := RI.Init (0);
    File_C    : aliased String := To_C_String (File_Name);
    Err       : C.int := 0;
    Proc_Err  : aliased C.int := 0;
    Img       : RI.Processed_Image_Ptr := null;

    W, H     : Integer;
    Channels : Integer;
    Bits     : Integer;

    --  Result index ranges.
    subtype Row_Index    is Positive range 1 .. Size;
    subtype Column_Index is Positive range 1 .. Size;

    Result : Green_Grid (Row_Index, Column_Index);

    use type RI.Context;
    use type SE.Storage_Offset;
    use type RI.Processed_Image_Ptr;
    use type System.Address;

  begin -- Grid_Of
    if Ctx = RI.Null_Context then
      Error ("Grid_Of: libraw_init returned NULL context");
    end if;

    begin
      IO.Put_Line ("1. Open file via LibRaw");
      ----------------------------------------
      Err := RI.Open_File (Ctx, File_C'address);
      Check (Err, "libraw_open_file");

      IO.Put_Line ("2. Unpack RAW");
      ------------------------------
      Err := RI.Unpack (Ctx);
      Check (Err, "libraw_unpack");

      IO.Put_Line ("3. Run dcraw-style processing");
      ----------------------------------------------
      Err := RI.Dcraw_Process (Ctx);
      Check (Err, "libraw_dcraw_process");

      IO.Put_Line ("4. Get processed image in memory");
      -------------------------------------------------
      Img := RI.Dcraw_Make_Mem_Image (Ctx, Proc_Err'access);
      Check (Proc_Err, "libraw_dcraw_make_mem_image");

      if Img = null then
        Error ("Grid_Of: libraw_dcraw_make_mem_image returned NULL pointer");
      end if;

      IO.Put_Line ("5. Extract dimensions and format info");
      ------------------------------------------------------
      W        := Integer (Img.Width);
      H        := Integer (Img.Height);
      Channels := Integer (Img.Colors);
      Bits     := Integer (Img.Bits);

      IO.Put_Line ("   - Type  :" & Img.Img_Type'image);
      IO.Put_Line ("   - Width :" & Img.Width'image);
      IO.Put_Line ("   - Height:" & Img.Height'image);
      IO.Put_Line ("   - Colors:" & Img.Colors'image);
      IO.Put_Line ("   - Bits  :" & Img.Bits'image);
      IO.Put_Line ("   - Size  :" & Img.Data_Size'image);

      if W <= 0 or else H <= 0 then
        Error ("Grid_Of: invalid processed image dimensions");
      end if;

      if Integer (Size) > W or else Integer (Size) > H then
        Error ("Grid_Of: Square_Size exceeds processed image dimensions (" &
               Integer'image (W) & " x" & Integer'image (H) & ")");
      end if;

      if Channels < 1 then
        Error ("Grid_Of: processed image has no channels");
      end if;

      --  Accept 8 or 16 bits; reject anything else.
      if Bits /= 8 and then Bits /= 16 then
        Error ("Grid_Of: only 8- or 16-bit processed images are supported - actual" &
               Bits'image & " bits");
      end if;

      IO.Put_Line ("6. Choose which channel to treat as 'green'.");
      -------------------------------------------------------------
      --     For typical RGB output: 0=R, 1=G, 2=B.
      --     If only one channel, use that as 'green' surrogate.
      declare
        Green_Index : constant Integer := (if Channels >= 2 then 1 else 0);
        Base_Adr    : constant System.Address := Img.Data'address;
        SS          : constant Integer := Integer (Size);
        Row_Offset  : constant Integer := (H - SS) / 2;
        Col_Offset  : constant Integer := (W - SS) / 2;
      begin
        IO.Put_Line ("   - Green_Index:" & Green_Index'image);
        IO.Put_Line ("   - Base_Adr   : 0x" & System.Address_Image (Base_Adr));
        if Row_Offset < 0 or else Col_Offset < 0 then
          Error ("Grid_Of: negative crop offset, check Square_Size");
        end if;
        IO.Put_Line ("   - Row_Offset :" & Row_Offset'image);
        IO.Put_Line ("   - Col_Offset :" & Col_Offset'image);

        IO.Put_Line ("7. Extract green samples from the central crop.");
        ----------------------------------------------------------------
        --
        --  Memory layout from libraw_dcraw_make_mem_image:
        --    Row-major, pixels in [0 .. H-1] × [0 .. W-1].
        --    For each pixel: Channels samples.
        --    If Bits = 16: each sample is unsigned short (2 bytes).
        --    If Bits =  8: each sample is unsigned char (1 byte).
        --
        --    sample_index =
        --      (global_row * W + global_col) * Channels + Green_Index;
        --
        --    byte_offset =
        --      sample_index * bytes_per_sample;
        for Row in Row_Index loop
          declare
            R0         : constant Integer := Row - 1;
            Global_Row : constant Integer := Row_Offset + R0;
          begin
            if Global_Row < 0 or else Global_Row >= H then
              Error ("Grid_Of: computed row outside image height");
            end if;

            for Col in Column_Index loop
              declare
                C0         : constant Integer := Col - 1;
                Global_Col : constant Integer := Col_Offset + C0;
                Pixel_Idx  : Integer;
                Sample_Idx : Integer;
                Byte_Off   : SE.Storage_Offset;
                Sample_Adr : System.Address;
              begin
                if Global_Col < 0 or else Global_Col >= W then
                  Error ("Grid_Of: computed column outside image width");
                end if;

                Pixel_Idx := Global_Row * W + Global_Col;

                if Pixel_Idx < 0 then
                  Error ("Grid_Of: negative pixel index");
                end if;

                Sample_Idx := Pixel_Idx * Channels + Green_Index;

                if Sample_Idx < 0 then
                  Error ("Grid_Of: negative sample index");
                end if;

                declare
                  Sample : Sample_Ptr;
                begin
                  Byte_Off := SE.Storage_Offset (Sample_Idx);
                  Sample_Adr := Base_Adr + Byte_Off;
                  Sample := To_Sample_Ptr (Sample_Adr);

                  if Sample = null then
                    Error ("Grid_Of: null 8-bit sample pointer");
                  end if;

                  Result (Row, Col) := Sample.all;
                end;

              end;
            end loop;
          end;
        end loop;
      end;

      IO.Put_Line ("8. Clean up LibRaw objects");
      -------------------------------------------
      RI.Dcraw_Clear_Mem (Img);
      RI.Free_Image      (Ctx);  -- no-op here but safe
      RI.Recycle         (Ctx);
      RI.Close           (Ctx);

    exception
      when others =>
        --  Best-effort cleanup, then re-raise
        begin
          if Img /= null then
            RI.Dcraw_Clear_Mem (Img);
          end if;
        exception
          when others => null;
        end;
        begin
          RI.Free_Image (Ctx);
        exception
          when others => null;
        end;
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
