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

with Ada.Unchecked_Conversion;
with Interfaces.C;
with Camera.Raw_Interface;
with System.Storage_Elements;
with Traces;

package body Camera.Raw is

  package Log is new Traces ("Camera.Raw");

  package C  renames Interfaces.C;
  package RI renames Raw_Interface;
  package SE renames System.Storage_Elements;

  -------------------
  -- Local helpers --
  -------------------

  procedure Error (Msg : String) is
  begin
    Log.Error (Msg);
    raise Raw_Error with Msg;
  end Error;


  procedure Check (Code  : C.int;
                   Where : String) is
    use type C.int;
  begin
    Log.Write (Where);
    if Code /= 0 then
      Error (Where & " failed (LibRaw error code" & Integer'image (Integer (Code)) & ")");
    end if;
  end Check;


  function To_C_String (S : String) return String is
  begin
    return S & Ascii.Nul;
  end To_C_String;

  type Sample_Ptr is access all Pixel with Convention => C;

  function To_Sample_Ptr is new Ada.Unchecked_Conversion (Source => System.Address,
                                                          Target => Sample_Ptr);


  Ctx : RI.Context := RI.Null_Context;
  Img : RI.Processed_Image_Ptr;

  Grid_Is_Prepared : Boolean := False;

  Undefined : constant := 0;

  The_Height : Natural := Undefined;
  The_Width  : Natural := Undefined;


  procedure Cleanup is
    use type RI.Processed_Image_Ptr;
    use type RI.Context;
  begin
    The_Height := Undefined;
    The_Width := Undefined;
    Grid_Is_Prepared := False;
    begin
      if Img /= null then
        RI.Dcraw_Clear_Mem (Img);
        Img := null;
      end if;
    exception
    when others =>
      null;
    end;
    if Ctx /= RI.Null_Context then
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
      Ctx := RI.Null_Context;
    end if;
  end Cleanup;


  ------------------
  -- Prepare_Grid --
  ------------------

  Bytes_Per_Sample : constant Integer := Pixel'size / System.Storage_Unit;

  type Channel is range 1 .. 3;

  The_Colors    : Channel;
  The_Grid_Size : Square_Size;

  procedure Prepare_Grid (File_Name : String;
                          Size      : Square_Size)
  is
    File_C   : aliased String := To_C_String (File_Name);
    Proc_Err : aliased C.int := 0;

    type Bit_Count is range 1 .. 32;

    Bits      : Bit_Count;
    Data_Size : Natural;

    use type RI.Context;
    use type RI.Processed_Image_Ptr;

  begin -- Prepare_Grid
    The_Height := Undefined;
    The_Width := Undefined;
    The_Grid_Size := Size;
    Ctx := RI.Init (0);
    if Ctx = RI.Null_Context then
      Error ("libraw_init returned NULL context");
    end if;

    Check (RI.Open_File (Ctx, File_C'address), "Open file via LibRaw");

    Check (RI.Unpack (Ctx), "Unpack RAW");

    Log.Write ("Configure sample size, linear, no auto-bright");
    RI.Set_Output_Bps (Ctx, Pixel'size);
    --  Disable auto-brightening
    RI.Set_No_Auto_Bright (Ctx, 1);
    --  Linear gamma: gamma[0] = 1.0, gamma[1] = 1.0
    RI.Set_Gamma (Ctx, 0, 1.0);
    RI.Set_Gamma (Ctx, 1, 1.0);

    Check (RI.Dcraw_Process (Ctx), "Process Dcraw");

    Img := RI.Dcraw_Make_Mem_Image (Ctx, Proc_Err'access);
    Check (Proc_Err, "Make memory image");

    if Img = null then
      Error ("Make memory image returned NULL pointer");
    end if;

    Log.Write ("Extract dimensions and format info");
    begin
      The_Width  := Natural(Img.Width);
      The_Height := Natural(Img.Height);
      The_Colors := Channel(Img.Colors);
      Bits := Bit_Count(Img.Bits);
      Data_Size := Natural(Img.Data_Size);
    exception
    when others =>
      Error ("Invalid processed image data");
    end;

    Log.Write ("- Type  :" & Img.Img_Type'image);
    Log.Write ("- Height:" & The_Height'image);
    Log.Write ("- Width :" & The_Width'image);
    Log.Write ("- Colors:" & The_Colors'image);
    Log.Write ("- Bits  :" & Bits'image);
    Log.Write ("- Size  :" & Data_Size'image);

    if Bits /= Pixel'size then
      Error ("Expected" & Pixel'size'image & "bits processed image, got" &
             Bits'image & " bits (check libraw_set_output_bps support)");
    end if;

    if Data_Size /= The_Width * The_Height * Natural(The_Colors) * Bytes_Per_Sample then
      Error ("Invalid image data size");
    end if;
    Grid_Is_Prepared := True;
  exception
  when others =>
    Cleanup;
    raise;
  end Prepare_Grid;


  procedure Stop_Preparing is
  begin
    Cleanup;
  end Stop_Preparing;


  ----------
  -- Grid --
  ----------
  function Grid return Green_Grid is

    subtype Row_Index    is Rows    range 1 .. Rows(The_Grid_Size);
    subtype Column_Index is Columns range 1 .. Columns(The_Grid_Size);

    Result : Green_Grid (Row_Index, Column_Index);

    Base_Adr    : constant System.Address := Img.Data'address;
    Green_Index : constant Natural := (if The_Colors >= 2 then 1 else 0);
    SS          : constant Natural := Natural(The_Grid_Size);
    Row_Offset  : constant Natural := (The_Height - SS) / 2;
    Col_Offset  : constant Natural := (The_Width - SS) / 2;

    use type SE.Storage_Offset;
    use type System.Address;

  begin -- Grid
    if not Grid_Is_Prepared then
      Error ("Grid not prepared");
    end if;
    Log.Write ("Choose which channel to treat as 'green'");
    Log.Write ("- Green_Index:" & Green_Index'image);
    Log.Write ("- Row_Offset :" & Row_Offset'image);
    Log.Write ("- Col_Offset :" & Col_Offset'image);
    Log.Write ("Extract green samples from the central crop");
    -- Memory layout from libraw_dcraw_make_mem_image:
    --   Row-major, pixels in [0 .. H-1] × [0 .. W-1].
    --   For each pixel: Channels samples, each 2 bytes (unsigned short).
    --
    --   sample_index =
    --     (global_row * W + global_col) * Channels + Green_Index;
    --
    --   byte_offset =
    --     sample_index * Bytes_Per_Sample;
    for Row in Row_Index loop
      declare
        R0         : constant Natural := Natural(Row) - 1;
        Global_Row : constant Natural := Row_Offset + R0;
      begin
        for Col in Column_Index loop
          declare
            C0         : constant Natural := Natural(Col) - 1;
            Global_Col : constant Natural := Col_Offset + C0;
            Pixel_Idx  : constant Natural := Global_Row * The_Width + Global_Col;
            Sample_Idx : constant Natural := Pixel_Idx * Natural(The_Colors) + Green_Index;
            Byte_Off   : constant SE.Storage_Offset := SE.Storage_Offset (Sample_Idx * Bytes_Per_Sample);
            Sample     : constant Sample_Ptr := To_Sample_Ptr (Base_Adr + Byte_Off);
          begin
            Result (Row, Col) := Sample.all;
          end;
        end loop;
      end;
    end loop;
    Log.Write ("Clean up LibRaw objects");
    RI.Dcraw_Clear_Mem (Img);
    RI.Free_Image (Ctx); -- safe (no-op here)
    RI.Recycle (Ctx);
    RI.Close (Ctx);

    return Result;
  exception
  when others =>
    Cleanup;
    raise;
  end Grid;


  -----------
  -- Image --
  ---------=-
  function Height return Rows is
  begin
    return Rows(The_Height);
  end Height;


  function Width return Columns is
  begin
    return Columns(The_Width);
  end Width;

end Camera.Raw;
