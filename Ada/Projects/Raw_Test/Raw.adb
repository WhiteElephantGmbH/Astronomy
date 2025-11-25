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

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Log;
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
    Image : constant String := "### Grid_Of: " & Msg;
  begin
    IO.Put_Line (Image);
    Log.Write (Image);
    raise Raw_Error with Msg;
  end Error;

  Title_Number : Natural;

  procedure Title (Item : String) is
    Number : constant String := Title_Number'image;
    Image  : constant String := Number(Number'last-1 .. Number'last) & ". " & Item;
  begin
    IO.Put_Line (Image);
    Log.Write (Image);
    Title_Number := @ + 1;
  end Title;

  procedure Check (Code  : C.int;
                   Where : String) is
    use type C.int;
  begin
    if Code /= 0 then
      Error (Where & " failed (LibRaw error code" & Integer'image (Integer (Code)) & ")");
    end if;
  end Check;

  function To_C_String (S : String) return String is
  begin
    return S & Ascii.Nul;
  end To_C_String;

  type Sample_Ptr is access all Pixel
    with Convention => C;

  function To_Sample_Ptr is new Ada.Unchecked_Conversion (Source => System.Address,
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

    Bytes_Per_Sample : constant Integer := Pixel'size / System.Storage_Unit;

    Height : Natural;
    Width  : Natural;

    type Channel   is range 1 .. 3;
    type Bit_Count is range 1 .. 32;

    Colors    : Channel;
    Bits      : Bit_Count;
    Data_Size : Natural;

    --  Result index ranges.
    subtype Row_Index    is Rows    range 1 .. Rows(Size);
    subtype Column_Index is Columns range 1 .. Columns(Size);

    Result : Green_Grid (Row_Index, Column_Index);

    use type RI.Context;
    use type SE.Storage_Offset;
    use type RI.Processed_Image_Ptr;
    use type System.Address;

  begin -- Grid_Of
    Title_Number := 1;
    if Ctx = RI.Null_Context then
      Error ("Grid_Of: libraw_init returned NULL context");
    end if;

    begin
      Title ("Open file via LibRaw");
      -------------------------------
      Check (RI.Open_File (Ctx, File_C'address), "libraw_open_file");

      Title ("Unpack RAW");
      ---------------------
      Check (RI.Unpack (Ctx), "libraw_unpack");

      Title ("Configure sample size, linear, no auto-bright");
      -----------------------------------------------------
      RI.Set_Output_Bps (Ctx, Pixel'size);
      --  Disable auto-brightening
      RI.Set_No_Auto_Bright (Ctx, 1);
      --  Linear gamma: gamma[0] = 1.0, gamma[1] = 1.0
      RI.Set_Gamma (Ctx, 0, 1.0);
      RI.Set_Gamma (Ctx, 1, 1.0);

      Title ("Run dcraw-style processing");
      -------------------------------------
      Err := RI.Dcraw_Process (Ctx);
      Check (Err, "libraw_dcraw_process");

      Title ("Get processed image in memory");
      ----------------------------------------
      Img := RI.Dcraw_Make_Mem_Image (Ctx, Proc_Err'access);
      Check (Proc_Err, "libraw_dcraw_make_mem_image");

      if Img = null then
        Error ("Grid_Of: libraw_dcraw_make_mem_image returned NULL pointer");
      end if;

      Title ("Extract dimensions and format info");
      ---------------------------------------------
      begin
        Width  := Natural(Img.Width);
        Height := Natural(Img.Height);
        Colors := Channel(Img.Colors);
        Bits := Bit_Count(Img.Bits);
        Data_Size := Natural(Img.Data_Size);
      exception
      when others =>
        Error ("Invalid processed image data");
      end;

      IO.Put_Line ("   - Type  :" & Img.Img_Type'image);
      IO.Put_Line ("   - Width :" & Width'image);
      IO.Put_Line ("   - Height:" & Height'image);
      IO.Put_Line ("   - Colors:" & Colors'image);
      IO.Put_Line ("   - Bits  :" & Bits'image);
      IO.Put_Line ("   - Size  :" & Data_Size'image);

      if Bits /= Pixel'size then
        Error ("Grid_Of: expected" & Pixel'size'image & "bits processed image, got" &
               Bits'image & " bits (check libraw_set_output_bps support)");
      end if;

      if Data_Size /= Width * Height * Natural(Colors) * Bytes_Per_Sample then
        Error ("Grid_Of: invalid image data size");
      end if;

      Title ("Choose which channel to treat as 'green'.");
      ----------------------------------------------------
      declare
        Green_Index : constant Natural := (if Colors >= 2 then 1 else 0);
        Base_Adr    : constant System.Address := Img.Data'address;
        SS          : constant Natural := Natural(Size);
        Row_Offset  : constant Natural := (Height - SS) / 2;
        Col_Offset  : constant Natural := (Width - SS) / 2;
      begin
        IO.Put_Line ("   - Green_Index:" & Green_Index'image);
        IO.Put_Line ("   - Base_Adr   : 0x" & System.Address_Image (Base_Adr));
        IO.Put_Line ("   - Row_Offset :" & Row_Offset'image);
        IO.Put_Line ("   - Col_Offset :" & Col_Offset'image);

        Title ("Extract green samples from the central crop.");
        -------------------------------------------------------
        --
        --  Memory layout from libraw_dcraw_make_mem_image:
        --    Row-major, pixels in [0 .. H-1] × [0 .. W-1].
        --    For each pixel: Channels samples, each 2 bytes (unsigned short).
        --
        --    sample_index =
        --      (global_row * W + global_col) * Channels + Green_Index;
        --
        --    byte_offset =
        --      sample_index * Bytes_Per_Sample;

        for Row in Row_Index loop
          declare
            R0         : constant Natural := Natural(Row) - 1;
            Global_Row : constant Natural := Row_Offset + R0;
          begin
            for Col in Column_Index loop
              declare
                C0         : constant Natural := Natural(Col) - 1;
                Global_Col : constant Natural := Col_Offset + C0;
                Pixel_Idx  : constant Natural := Global_Row * Width + Global_Col;
                Sample_Idx : constant Natural := Pixel_Idx * Natural(Colors) + Green_Index;
                Byte_Off   : constant SE.Storage_Offset := SE.Storage_Offset (Sample_Idx * Bytes_Per_Sample);
                Sample     : constant Sample_Ptr := To_Sample_Ptr (Base_Adr + Byte_Off);
              begin
                Result (Row, Col) := Sample.all;
              end;
            end loop;
          end;
        end loop;
      end;

      Title ("Clean up LibRaw objects");
      ----------------------------------
      RI.Dcraw_Clear_Mem (Img);
      RI.Free_Image (Ctx); -- safe (no-op here)
      RI.Recycle (Ctx);
      RI.Close (Ctx);

    exception
    when others =>
      -- Best-effort cleanup, then re-raise
      begin
        if Img /= null then
          RI.Dcraw_Clear_Mem (Img);
        end if;
      exception
      when others =>
        null;
      end;
      begin
        RI.Free_Image (Ctx);
      exception
      when others =>
        null;
      end;
      begin
        RI.Recycle (Ctx);
      exception
      when others =>
        null;
      end;
      begin
        RI.Close (Ctx);
      exception
      when others =>
        null;
      end;
      raise;
    end;
    return Result;
  end Grid_Of;

end Raw;
