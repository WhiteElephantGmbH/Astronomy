-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with C;
with Camera.Raw_Interface;
with File;
with System.Storage_Elements;

package body Camera.Raw is

  package RI renames Raw_Interface;
  package SE renames System.Storage_Elements;

  ------------------
  -- Prepare_Grid --
  ------------------

  The_Filename  : Text.String;
  The_Grid_Size : Square_Size;

  procedure Prepare_Grid (File_Name : String;
                          Size      : Square_Size)is
  begin
    if File.Exists (File_Name) then
      The_Filename := [File_Name];
      The_Grid_Size := Size;
      Camera_Data.Set (Cropped);
    else
      Camera_Data.Set_Error ("File Not Found");
    end if;
  end Prepare_Grid;


  ----------
  -- Grid --
  ----------

  function Grid return Raw_Grid is

    type Lib_Raw_Data_Prefix is record
      Image : System.Address;
      -- .. : other fiels not used
    end record with
      Convention => C;

    type Lib_Raw_Data_Prefix_Access is access all Lib_Raw_Data_Prefix;

    function To_Prefix is new Ada.Unchecked_Conversion (Source => RI.Context,
                                                        Target => Lib_Raw_Data_Prefix_Access);

    type Ushort is new C.Unsigned_Short;
    type Pixel4 is array (Natural range 0 .. 3) of Ushort with Convention => C;

    function Pixel4_From_Address (A : System.Address) return Pixel4 is
      type Pixel4_Access is access all Pixel4;
      function To_Pixel4 is new Ada.Unchecked_Conversion (System.Address, Pixel4_Access);
    begin
      return To_Pixel4 (A).all;
    end Pixel4_From_Address;

    Ctx    : constant RI.Context := RI.Init (0);
    File_C : aliased String := The_Filename.S & Ascii.Nul;

    Img_W  : Natural := 0;
    Img_H  : Natural := 0;

    GS : constant Natural := Natural (The_Grid_Size);

    subtype Row_Index    is Rows    range 1 .. Rows (GS);
    subtype Column_Index is Columns range 1 .. Columns (GS);

    Result : Raw_Grid (Row_Index, Column_Index);

    use type RI.Context;
    use type System.Address;
    use type SE.Storage_Offset;

    procedure Cleanup is
    begin
      RI.Free_Image (Ctx);
      RI.Recycle (Ctx);
      RI.Close (Ctx);
    end Cleanup;

    procedure Check (Code  : C.Int;
                     Where : String) is
      use type C.Int;
      New_Line : constant String := [Ascii.Cr, Ascii.Lf];
    begin
      Log.Write (Where);
      if Code /= 0 then
        Raise_Error (Where & " failed" & New_Line & "(LibRaw error code" & Integer'image (Integer(Code)) & ")");
      end if;
    end Check;

  begin -- Grid
    Camera_Data.Check (Cropped);
    if Ctx = RI.Null_Context then
      Raise_Error ("libraw init returned NULL");
    end if;
    Check (RI.Open_File (Ctx, File_C'address), "libraw open_file " & The_Filename.S);
    Check (RI.Unpack (Ctx), "libraw unpack");
    Check (RI.Raw2_Image (Ctx), "libraw raw2image");

    Img_W := Natural(RI.Get_Iwidth (Ctx));
    Img_H := Natural(RI.Get_Iheight (Ctx));

    if GS > Img_W or else GS > Img_H then
      Raise_Error ("Size exceeds raw dimensions");
    end if;
    Camera_Data.Set (Width => Columns(Img_W));
    Camera_Data.Set (Height => Rows(Img_H));

    declare
      Row_Offset : constant Natural := (Img_H - GS) / 2;
      Col_Offset : constant Natural := (Img_W - GS) / 2;

      Pfx  : constant Lib_Raw_Data_Prefix_Access := To_Prefix (Ctx);
      Base : constant System.Address := Pfx.Image;

      Bytes_Per_Pixel : constant SE.Storage_Offset := SE.Storage_Offset (Pixel4'size / System.Storage_Unit);

      function Pixel_Address (Global_Row, Global_Col : Natural) return System.Address is
        Idx : constant Natural := Global_Row * Img_W + Global_Col;
        Off : constant SE.Storage_Offset := SE.Storage_Offset(Idx) * Bytes_Per_Pixel;
      begin
        return Base + Off;
      end Pixel_Address;

    begin
      if Base = System.Null_Address then
        Raise_Error ("ctx -> image is NULL (raw2image failed)");
      end if;
      for Row in Row_Index loop
        declare
          Global_Row : constant Natural := Row_Offset + (Natural(Row) - 1);
        begin
          for Col in Column_Index loop
            declare
              Global_Col : constant Natural := Col_Offset + (Natural(Col) - 1);
              Cfa        : constant Integer := Integer(RI.COLOR (Ctx, C.Int(Global_Row), C.Int(Global_Col)));
              P4         : constant Pixel4 := Pixel4_From_Address (Pixel_Address (Global_Row, Global_Col));
              V          : Natural := 0;
            begin
              if Cfa >= 0 and then Cfa <= 3 then
                V := Natural(P4 (Natural(Cfa)));
              else
                V := 0; -- “6” can happen for special/non-bayer cases; keep 0 for now
              end if;
              if V > Natural(Pixel'last) then
                V := Natural(Pixel'last);
              end if;
              Result (Row, Col) := Pixel(V);
            end;
          end loop;
        end;
      end loop;
      Cleanup;
      Camera_Data.Set (Idle);
      return Result;
    end;
  exception
  when Camera_Error =>
    Cleanup;
    return [];
  when Occurrence: others =>
    Cleanup;
    Camera_Data.Set_Fatal (Occurrence);
    return [];
  end Grid;

end Camera.Raw;
