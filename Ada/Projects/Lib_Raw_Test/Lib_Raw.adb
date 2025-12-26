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
with Raw_Interface;
with System;
with System.Storage_Elements;

package body Lib_Raw is

  package C  renames Interfaces.C;
  package RI renames Raw_Interface;
  package SE renames System.Storage_Elements;

  function To_C_String (S : String) return String is
  begin
    return S & Ascii.Nul;
  end To_C_String;

  procedure Check (Code : C.int; Where : String) is
    use type C.int;
  begin
    if Code /= 0 then
      raise Raw_Error with Where & " failed, code=" & Integer (Code)'image;
    end if;
  end Check;

  -- We only need the *first field* of libraw_data_t:
  --   ushort (*image)[4];
  type Lib_Raw_Data_Prefix is record
    Image : System.Address;
  end record
    with Convention => C;

  type Lib_Raw_Data_Prefix_Access is access all Lib_Raw_Data_Prefix;

  function To_Prefix is new Ada.Unchecked_Conversion
    (Source => RI.Context, Target => Lib_Raw_Data_Prefix_Access);

  -- Pixel quad: image[p][0..3]
  type Ushort is new C.unsigned_short;
  type Pixel4 is array (Natural range 0 .. 3) of Ushort
    with Convention => C;

  function Pixel4_From_Address (A : System.Address) return Pixel4 is
    type Pixel4_Access is access all Pixel4;
    function To_Pixel4 is new Ada.Unchecked_Conversion
      (Source => System.Address, Target => Pixel4_Access);
  begin
    return To_Pixel4 (A).all;
  end Pixel4_From_Address;

  function Grid_Of (File_Name : String;
                    Size      : Square_Size) return Grid
  is
    Ctx    : constant RI.Context := RI.Init (0);
    File_C : aliased String := To_C_String (File_Name);

    Img_W  : Natural := 0;
    Img_H  : Natural := 0;

    SS     : constant Natural := Natural (Size);

    subtype Row_Index    is Rows    range 1 .. Rows (SS);
    subtype Column_Index is Columns range 1 .. Columns (SS);

    Result : Grid (Row_Index, Column_Index);

    use type RI.Context;
    use type System.Address;
    use type SE.Storage_Offset;

  begin
    if Ctx = RI.Null_Context then
      raise Raw_Error with "libraw_init returned NULL";
    end if;

    begin
      Check (RI.Open_File (Ctx, File_C'address), "libraw_open_file");
      Check (RI.Unpack (Ctx), "libraw_unpack");
      Check (RI.Raw2_Image (Ctx), "libraw_raw2image");

      Img_W := Natural (RI.Get_Iwidth (Ctx));
      Img_H := Natural (RI.Get_Iheight (Ctx));

      if SS > Img_W or else SS > Img_H then
        raise Raw_Error with "Size exceeds raw dimensions";
      end if;

      declare
        Row_Offset : constant Natural := (Img_H - SS) / 2;
        Col_Offset : constant Natural := (Img_W - SS) / 2;

        Pfx  : constant Lib_Raw_Data_Prefix_Access := To_Prefix (Ctx);
        Base : constant System.Address := Pfx.Image;

        Bytes_Per_Pixel : constant SE.Storage_Offset := SE.Storage_Offset (Pixel4'size / System.Storage_Unit);

        function Pixel_Address (Global_Row, Global_Col : Natural) return System.Address is
          Idx : constant Natural := Global_Row * Img_W + Global_Col;
          Off : constant SE.Storage_Offset := SE.Storage_Offset (Idx) * Bytes_Per_Pixel;
        begin
          return Base + Off;
        end Pixel_Address;

      begin
        if Base = System.Null_Address then
          raise Raw_Error with "ctx->image is NULL (raw2image failed?)";
        end if;

        for Row in Row_Index loop
          declare
            Global_Row : constant Natural := Row_Offset + (Natural (Row) - 1);
          begin
            for Col in Column_Index loop
              declare
                Global_Col : constant Natural := Col_Offset + (Natural (Col) - 1);

                Cfa : constant Integer :=
                  Integer (RI.COLOR (Ctx, C.int (Global_Row), C.int (Global_Col)));

                P4  : constant Pixel4 :=
                  Pixel4_From_Address (Pixel_Address (Global_Row, Global_Col));

                V   : Natural := 0;
              begin
                if Cfa >= 0 and then Cfa <= 3 then
                  V := Natural (P4 (Natural (Cfa)));
                else
                  -- “6” can happen for special/non-bayer cases; keep 0 for now
                  V := 0;
                end if;

                if V > Natural(Pixel'last) then
                  V := Natural(Pixel'last);
                end if;

                Result (Row, Col) := Pixel (V);
              end;
            end loop;
          end;
        end loop;
      end;
      -- cleanup
      RI.Free_Image (Ctx);
      RI.Recycle (Ctx);
      RI.Close (Ctx);

      return Result;

    exception
      when others =>
        -- best-effort cleanup
        begin RI.Free_Image (Ctx); exception when others => null; end;
        begin RI.Recycle (Ctx);    exception when others => null; end;
        begin RI.Close (Ctx);      exception when others => null; end;
        raise;
    end;
  end Grid_Of;

end Lib_Raw;
