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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with GNAT.OS_Lib;
with System;

package body Raw_Data is

  package OS renames GNAT.OS_Lib;
  package IO renames Ada.Text_IO;

  -- Integrated "tests" / debug output switch
  Debug : constant Boolean := True;

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

  -- Decoded samples (single component, 16-bit container)
  type U16_Array is array (Positive range <>) of U16;
  type U16_Array_Access is access all U16_Array;

  The_Pixels : U16_Array_Access := null;

  procedure Free_Bytes is new Ada.Unchecked_Deallocation
    (Byte_Array, Byte_Array_Access);

  procedure Free_U16 is new Ada.Unchecked_Deallocation
    (U16_Array, U16_Array_Access);

  procedure Clear is
  begin
    if The_Buffer /= null then
      Free_Bytes (The_Buffer);
    end if;

    if The_Pixels /= null then
      Free_U16 (The_Pixels);
    end if;

    The_Buffer := null;
    The_Buffer_Size := 0;

    The_Pixels := null;

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

  --------------------------------------------------------------------------
  -- TIFF (CR2 RAW IFD)
  --------------------------------------------------------------------------

  Tag_ImageWidth      : constant U16 := 16#0100#;
  Tag_ImageLength     : constant U16 := 16#0101#;
  Tag_BitsPerSample   : constant U16 := 16#0102#;
  Tag_Compression     : constant U16 := 16#0103#;
  Tag_StripOffsets    : constant U16 := 16#0111#;
  Tag_StripByteCounts : constant U16 := 16#0117#;

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
      A : U32_Array (1 .. N);
      Bytes_Total : constant Natural := N * S;
    begin
      if Bytes_Total <= 4 then
        if Typ = Type_LONG then
          A (1) := Val;
        else
          if N >= 1 then
            A (1) := U32 (U16 (Val and 16#FFFF#));
          end if;
          if N >= 2 then
            A (2) := U32 (U16 (Interfaces.Shift_Right (Val, 16) and 16#FFFF#));
          end if;
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

  --------------------------------------------------------------------------
  -- JPEG helpers (buffer-based)
  --------------------------------------------------------------------------

  function BE16 (B : Byte_Array; P : Natural) return U16 is
  begin
    return Interfaces.Shift_Left (U16 (B (P)), 8) or U16 (B (P + 1));
  end BE16;

  procedure Dump_Jpeg_Header_Markers (B : Byte_Array) is
    I : Natural := B'first;
  begin
    if not Debug then
      return;
    end if;

    if B'length < 4 then
      IO.Put_Line ("JPEG header scan: buffer too small");
      return;
    end if;

    if B (B'first) /= 16#FF# or else B (B'first + 1) /= 16#D8# then
      IO.Put_Line ("JPEG header scan: buffer does not start with SOI (FFD8)");
      return;
    end if;

    IO.Put_Line ("JPEG marker scan (header only):");
    IO.Put_Line ("  SOI at " & Natural'image (B'first));
    I := B'first + 2;

    while I + 3 <= B'last loop
      -- find 0xFF
      if B (I) /= 16#FF# then
        I := I + 1;
      else
        -- skip fill 0xFFs
        while I <= B'last and then B (I) = 16#FF# loop
          I := I + 1;
        end loop;

        exit when I > B'last;

        declare
          M : constant U8 := U8 (B (I));
          Seg_Start : constant Natural := I - 1;
        begin
          -- SOS => stop header scan
          if M = 16#DA# then
            IO.Put_Line ("  SOS at " & Natural'image (Seg_Start) & " (stop header scan)");
            return;
          end if;

          -- markers with length
          if I + 2 > B'last then
            IO.Put_Line ("  Truncated segment after marker at " & Natural'image (Seg_Start));
            return;
          end if;

          declare
            L : constant U16 := BE16 (B, I + 1);
            Seg_End : constant Natural := Seg_Start + Natural (L) + 1;
          begin
            IO.Put_Line
              ("  FF" & Interfaces.Unsigned_8'image (M)
               & " len=" & U16'image (L)
               & " at " & Natural'image (Seg_Start));

            if Seg_End > B'last + 1 then
              IO.Put_Line ("  Segment length runs past buffer end");
              return;
            end if;

            I := Seg_End;
          end;
        end;
      end if;
    end loop;
  end Dump_Jpeg_Header_Markers;

  --------------------------------------------------------------------------
  -- Lossless JPEG (SOF3) parse + decode (single component)
  --------------------------------------------------------------------------

  SOI  : constant U16 := 16#FFD8#;
  EOI  : constant U16 := 16#FFD9#;
  SOF3 : constant U16 := 16#FFC3#;
  DHT  : constant U16 := 16#FFC4#;
  SOSM : constant U16 := 16#FFDA#;

  function BE16_From_Buffer (Pos : Positive) return U16 is
    B1 : constant U16 := U16 (The_Buffer (Pos));
    B2 : constant U16 := U16 (The_Buffer (Pos + 1));
  begin
    return Interfaces.Shift_Left (B1, 8) or B2;
  end BE16_From_Buffer;

  function Get8B (Pos : in out Natural) return U8 is
  begin
    Pos := Pos + 1;
    return The_Buffer (Pos);
  end Get8B;

  function Get16B (Pos : in out Natural) return U16 is
    A : U16;
  begin
    A := U16 (Get8B (Pos));
    return Interfaces.Shift_Left (A, 8) or U16 (Get8B (Pos));
  end Get16B;

  type Count_1_16 is array (Positive range 1 .. 16) of U8;
  type I16_Array is array (Positive range 1 .. 16) of Integer;

  type Sym_Array is array (Positive range <>) of U8;
  type Sym_Array_Access is access all Sym_Array;

  procedure Free_Syms is new Ada.Unchecked_Deallocation
    (Sym_Array, Sym_Array_Access);

  type Huff_Table is record
    Counts   : Count_1_16 := [others => 0];
    Symbols  : Sym_Array_Access := null;

    Min_Code : I16_Array := [others => -1];
    Max_Code : I16_Array := [others => -1];
    Val_Ptr  : I16_Array := [others => 0];
  end record;

  type Huff_Table_Array is array (Natural range 0 .. 3, Natural range 0 .. 1) of Huff_Table;
  Huff : Huff_Table_Array;

  procedure Clear_Huffman is
  begin
    for Id in Huff'range (1) loop
      for Cl in Huff'range (2) loop
        if Huff (Id, Cl).Symbols /= null then
          Free_Syms (Huff (Id, Cl).Symbols);
        end if;
        Huff (Id, Cl) := (others => <>);
      end loop;
    end loop;
  end Clear_Huffman;

  procedure Build_Decode (T : in out Huff_Table) is
    Code : Integer := 0;
    K    : Integer := 1;
  begin
    for I in 1 .. 16 loop
      if T.Counts (I) = 0 then
        T.Min_Code (I) := -1;
        T.Max_Code (I) := -1;
      else
        T.Val_Ptr (I) := K;
        T.Min_Code (I) := Code;
        Code := Code + Integer (T.Counts (I)) - 1;
        T.Max_Code (I) := Code;
        Code := Code + 1;
        K := K + Integer (T.Counts (I));
      end if;
      Code := Code * 2;
    end loop;
  end Build_Decode;

  type Sof3_Component is record
    Comp_Id : U8 := 0;
    H       : U8 := 0;
    V       : U8 := 0;
    Tq      : U8 := 0;
  end record;

  type Sof3_Component_Array is array (Positive range <>) of Sof3_Component;
  type Sof3_Component_Array_Access is access all Sof3_Component_Array;

  procedure Free_Sof is new Ada.Unchecked_Deallocation
    (Sof3_Component_Array, Sof3_Component_Array_Access);

  type Sof3_Info is record
    Precision  : U8 := 0;
    Height     : U16 := 0;
    Width      : U16 := 0;
    Components : Sof3_Component_Array_Access := null;
  end record;

  type Sos_Component is record
    Comp_Id : U8 := 0;
    Td      : U8 := 0;
    Ta      : U8 := 0;
  end record;

  type Sos_Component_Array is array (Positive range <>) of Sos_Component;
  type Sos_Component_Array_Access is access all Sos_Component_Array;

  procedure Free_Sos is new Ada.Unchecked_Deallocation
    (Sos_Component_Array, Sos_Component_Array_Access);

  type Sos_Info is record
    Components      : Sos_Component_Array_Access := null;
    Predictor_Selec : U8 := 1;
    Point_Transform : U8 := 0;
  end record;

  function Extend_Signed (V : Integer; S : Natural) return Integer is
    VT : Integer := V;
    T  : constant Integer := 2 ** (Integer (S) - 1);
  begin
    if S = 0 then
      return 0;
    end if;

    if VT < T then
      VT := VT - (2 ** Integer (S) - 1);
    end if;
    return VT;
  end Extend_Signed;

  type Bit_Reader is record
    Pos      : Natural := 0;  -- position in The_Buffer (1-based)
    Bit_Buf  : U32 := 0;
    Bit_Cnt  : Natural := 0;
  end record;

  procedure Init (R : in out Bit_Reader; Start_Pos : Natural) is
  begin
    R.Pos := Start_Pos;
    R.Bit_Buf := 0;
    R.Bit_Cnt := 0;
  end Init;

  function Read_Byte_Stuffed (R : in out Bit_Reader) return U8 is
    B : U8;
  begin
    if R.Pos = 0 or else R.Pos >= The_Buffer_Size then
      raise Invalid_File;
    end if;

    R.Pos := R.Pos + 1;
    B := The_Buffer (R.Pos);

    if B = 16#FF# then
      if R.Pos >= The_Buffer_Size then
        raise Invalid_File;
      end if;
      if The_Buffer (R.Pos + 1) = 0 then
        R.Pos := R.Pos + 1; -- skip 0x00
      end if;
    end if;

    return B;
  end Read_Byte_Stuffed;

  procedure Ensure_Bits (R : in out Bit_Reader; N : Natural) is
  begin
    while R.Bit_Cnt < N loop
      declare
        B : constant U8 := Read_Byte_Stuffed (R);
      begin
        R.Bit_Buf := Interfaces.Shift_Left (R.Bit_Buf, 8) or U32 (B);
        R.Bit_Cnt := R.Bit_Cnt + 8;
      end;
    end loop;
  end Ensure_Bits;

  function Get_Bits (R : in out Bit_Reader; N : Natural) return U32 is
    Res : U32;
  begin
    if N = 0 then
      return 0;
    end if;

    Ensure_Bits (R, N);

    Res :=
      Interfaces.Shift_Right (R.Bit_Buf, Integer (R.Bit_Cnt - N))
      and (U32 (2 ** Integer (N) - 1));

    R.Bit_Cnt := R.Bit_Cnt - N;
    return Res;
  end Get_Bits;

  function Decode_Huff (R : in out Bit_Reader; T : Huff_Table) return U8 is
    Code : Integer := 0;
    I    : Natural := 1;
  begin
    loop
      Code := Code * 2 + Integer (Get_Bits (R, 1));

      if T.Min_Code (I) /= -1 and then Code <= T.Max_Code (I) then
        declare
          J : constant Integer := T.Val_Ptr (I) + (Code - T.Min_Code (I));
        begin
          if T.Symbols = null then
            raise Invalid_File;
          end if;
          return T.Symbols (Positive (J));
        end;
      end if;

      I := I + 1;
      exit when I > 16;
    end loop;

    raise Invalid_File;
  end Decode_Huff;

  procedure Parse_DHT (Pos : in out Natural; Seg_Len : Natural) is
    End_Pos : constant Natural := Pos + Seg_Len - 1;
  begin
    while Pos <= End_Pos loop
      declare
        TcTh  : constant U8 := Get8B (Pos);
        Tc    : constant Natural := Natural (Interfaces.Shift_Right (TcTh, 4));
        Th    : constant Natural := Natural (TcTh and 16#0F#);
        Cnts  : Count_1_16;
        Total : Natural := 0;
      begin
        if Th > 3 or else Tc > 1 then
          raise Invalid_File;
        end if;

        for I in 1 .. 16 loop
          Cnts (I) := Get8B (Pos);
          Total := Total + Natural (Cnts (I));
        end loop;

        declare
          Syms : Sym_Array_Access := null;
        begin
          if Total > 0 then
            Syms := new Sym_Array (1 .. Total);
            for I in 1 .. Total loop
              Syms (I) := Get8B (Pos);
            end loop;
          end if;

          if Huff (Th, Tc).Symbols /= null then
            Free_Syms (Huff (Th, Tc).Symbols);
          end if;

          Huff (Th, Tc).Counts := Cnts;
          Huff (Th, Tc).Symbols := Syms;
          Build_Decode (Huff (Th, Tc));

          if Debug then
            IO.Put_Line
              ("DHT loaded: class=" & Natural'image (Tc)
               & " id=" & Natural'image (Th)
               & " symbols=" & Natural'image (Total));
          end if;
        end;
      end;
    end loop;
  end Parse_DHT;

  procedure Parse_SOF3 (Pos : in out Natural; Seg_Len : Natural; Sof : in out Sof3_Info) is
    pragma Unreferenced (Seg_Len);
    P  : constant U8  := Get8B (Pos);
    Y  : constant U16 := Get16B (Pos);
    X  : constant U16 := Get16B (Pos);
    Nc : constant U8  := Get8B (Pos);
  begin
    Sof.Precision := P;
    Sof.Height := Y;
    Sof.Width := X;

    if Sof.Components /= null then
      Free_Sof (Sof.Components);
    end if;

    Sof.Components := new Sof3_Component_Array (1 .. Positive (Nc));

    for I in Sof.Components'range loop
      declare
        Cid : constant U8 := Get8B (Pos);
        HV  : constant U8 := Get8B (Pos);
        Tq  : constant U8 := Get8B (Pos);
      begin
        Sof.Components (I) :=
          (Comp_Id => Cid,
           H       => Interfaces.Shift_Right (HV, 4),
           V       => HV and 16#0F#,
           Tq      => Tq);
      end;
    end loop;

    if Debug then
      IO.Put_Line ("SOF3:");
      IO.Put_Line ("  precision: " & Natural'image (Natural (Sof.Precision)));
      IO.Put_Line ("  width    : " & Natural'image (Natural (Sof.Width)));
      IO.Put_Line ("  height   : " & Natural'image (Natural (Sof.Height)));
      IO.Put_Line ("  comps    : " & Natural'image (Sof.Components'length));
    end if;
  end Parse_SOF3;

  procedure Parse_SOS (Pos : in out Natural; Seg_Len : Natural; Sos : in out Sos_Info) is
    pragma Unreferenced (Seg_Len);
    Ns : constant U8 := Get8B (Pos);
  begin
    if Sos.Components /= null then
      Free_Sos (Sos.Components);
    end if;

    Sos.Components := new Sos_Component_Array (1 .. Positive (Ns));

    for I in Sos.Components'range loop
      declare
        Cid  : constant U8 := Get8B (Pos);
        TdTa : constant U8 := Get8B (Pos);
      begin
        Sos.Components (I) :=
          (Comp_Id => Cid,
           Td      => Interfaces.Shift_Right (TdTa, 4),
           Ta      => TdTa and 16#0F#);
      end;
    end loop;

    Sos.Predictor_Selec := Get8B (Pos); -- Ss
    declare
      Se   : constant U8 := Get8B (Pos);
      AhAl : constant U8 := Get8B (Pos);
    begin
      pragma Unreferenced (Se);
      Sos.Point_Transform := AhAl and 16#0F#;
    end;

    if Debug then
      IO.Put_Line ("SOS:");
      IO.Put_Line ("  comps     : " & Natural'image (Sos.Components'length));
      IO.Put_Line ("  predictor : " & Natural'image (Natural (Sos.Predictor_Selec)));
      IO.Put_Line ("  pt        : " & Natural'image (Natural (Sos.Point_Transform)));
      if Sos.Components'length > 0 then
        IO.Put_Line ("  DC table  : " & Natural'image (Natural (Sos.Components (Sos.Components'first).Td)));
      end if;
    end if;
  end Parse_SOS;

  function Predictor
    (Sel    : U8;
     Left   : Integer;
     Above  : Integer;
     UpLeft : Integer) return Integer is
  begin
    case Sel is
      when 1 =>
        return Left;
      when 2 =>
        return Above;
      when 3 =>
        return UpLeft;
      when 4 =>
        return Left + Above - UpLeft;
      when 5 =>
        return Left + (Above - UpLeft) / 2;
      when 6 =>
        return Above + (Left - UpLeft) / 2;
      when 7 =>
        return (Left + Above) / 2;
      when others =>
        return Left;
    end case;
  end Predictor;

  procedure Decode_Lossless_JPEG is
    Pos : Natural := 0;

    Sof : Sof3_Info;
    Sos : Sos_Info;

    Found_SOF3 : Boolean := False;
    Found_SOS  : Boolean := False;

    Entropy_Start : Natural := 0;

  begin
    Clear_Huffman;

    if The_Buffer = null or else The_Buffer_Size < 4 then
      raise Not_Found;
    end if;

    if BE16_From_Buffer (1) /= SOI then
      raise Not_Found;
    end if;

    Pos := 2; -- after SOI

    while Pos + 4 <= The_Buffer_Size loop
      -- seek marker 0xFF
      while Pos + 1 <= The_Buffer_Size and then The_Buffer (Pos + 1) /= 16#FF# loop
        Pos := Pos + 1;
      end loop;

      exit when Pos + 1 > The_Buffer_Size;

      while Pos + 1 <= The_Buffer_Size and then The_Buffer (Pos + 1) = 16#FF# loop
        Pos := Pos + 1;
      end loop;

      exit when Pos + 1 > The_Buffer_Size;

      Pos := Pos + 1; -- marker byte position

      declare
        M : constant U16 := 16#FF00# or U16 (The_Buffer (Pos));
      begin
        if M = EOI then
          exit;
        end if;

        if Pos + 2 > The_Buffer_Size then
          exit;
        end if;

        declare
          L  : constant Natural := Natural (BE16_From_Buffer (Pos + 1));
          P2 : Natural := Pos + 2;
        begin
          if L < 2 then
            raise Invalid_File;
          end if;

          if M = SOF3 then
            Parse_SOF3 (P2, L - 2, Sof);
            Found_SOF3 := True;

            The_Raw_Bits := Natural (Sof.Precision);
            The_Raw_Width := Natural (Sof.Width);
            The_Raw_Height := Natural (Sof.Height);

          elsif M = DHT then
            Parse_DHT (P2, L - 2);

          elsif M = SOSM then
            Parse_SOS (P2, L - 2, Sos);
            Found_SOS := True;

            Entropy_Start := Pos + 2 + (L - 2);
            exit;

          else
            null;
          end if;

          Pos := Pos + L;
        end;
      end;
    end loop;

    if not Found_SOF3 or else not Found_SOS or else Entropy_Start = 0 then
      raise Not_Found;
    end if;

    if Sos.Components = null or else Sos.Components'length < 1 then
      raise Invalid_File;
    end if;

    declare
      W : constant Natural := The_Raw_Width;
      H : constant Natural := The_Raw_Height;
      Npix : constant Natural := W * H;

      R : Bit_Reader;

      Td : constant Natural := Natural (Sos.Components (Sos.Components'first).Td);
      Dc_Table : constant Huff_Table := Huff (Td, 0);

      Pt  : constant Natural := Natural (Sos.Point_Transform);
      Sel : constant U8 := Sos.Predictor_Selec;

      MinV : Integer := Integer'last;
      MaxV : Integer := Integer'first;

    begin
      if Dc_Table.Symbols = null then
        raise Invalid_File;
      end if;

      if The_Pixels /= null then
        Free_U16 (The_Pixels);
      end if;

      The_Pixels := new U16_Array (1 .. Npix);

      Init (R, Entropy_Start);

      for Y in 1 .. H loop
        for X in 1 .. W loop
          declare
            Left   : constant Integer :=
              (if X > 1 then Integer (The_Pixels ((Y - 1) * W + (X - 1))) else 0);
            Above  : constant Integer :=
              (if Y > 1 then Integer (The_Pixels ((Y - 2) * W + X)) else 0);
            UpLeft : constant Integer :=
              (if X > 1 and then Y > 1 then Integer (The_Pixels ((Y - 2) * W + (X - 1))) else 0);

            Pred   : constant Integer := Predictor (Sel, Left, Above, UpLeft);

            SSSS   : constant U8 := Decode_Huff (R, Dc_Table);
            Diff_U : U32 := 0;
            Diff_S : Integer := 0;

            Samp   : Integer := 0;
          begin
            if SSSS /= 0 then
              Diff_U := Get_Bits (R, Natural (SSSS));
              Diff_S := Extend_Signed (Integer (Diff_U), Natural (SSSS));
            end if;

            Samp := Pred + Diff_S;
            Samp := Samp * (2 ** Integer (Pt));

            if Samp < 0 then
              Samp := 0;
            elsif Samp > 65535 then
              Samp := 65535;
            end if;

            if Samp < MinV then
              MinV := Samp;
            end if;
            if Samp > MaxV then
              MaxV := Samp;
            end if;

            The_Pixels ((Y - 1) * W + X) := U16 (Samp);
          end;
        end loop;
      end loop;

      if Debug then
        IO.Put_Line ("Decode sanity:");
        IO.Put_Line ("  pixels  : " & Natural'image (Npix));
        IO.Put_Line ("  min/max : " & Integer'image (MinV) & " / " & Integer'image (MaxV));
        IO.Put ("  first 16: ");
        declare
          Limit : constant Natural := (if Npix < 16 then Npix else 16);
        begin
          for I in 1 .. Limit loop
            IO.Put (Integer (The_Pixels (I))'image & " ");
          end loop;
        end;
        IO.New_Line;
      end if;
    end;
  end Decode_Lossless_JPEG;

  --------------------------------------------------------------------------
  -- Step 1: Read RAW IFD and build The_Buffer
  --------------------------------------------------------------------------

  procedure Read_Raw_IFD
    (FD         : OS.File_Descriptor;
     Base       : File_Pos;
     RawIFD_Off : U32) is

    Count : U16;

    Strip_Offsets : U32_Array (1 .. 1) := [1 => 0];
    Strip_Counts  : U32_Array (1 .. 1) := [1 => 0];

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

    declare
      Total : U32 := 0;
    begin
      for I in Strip_Counts'range loop
        Total := Total + Strip_Counts (I);
      end loop;

      The_Buffer_Size := Natural (Total);
      The_Buffer := new Byte_Array (1 .. Positive (The_Buffer_Size));

      declare
        P : Natural := 1;
      begin
        for I in Strip_Offsets'range loop
          declare
            Off : constant U32 := Strip_Offsets (I);
            Len : constant Natural := Natural (Strip_Counts (I));
          begin
            Seek_Set (FD, Base + File_Pos (Off));
            Read_Exact (FD, The_Buffer (P)'address, Positive (Len));
            P := P + Len;
          end;
        end loop;
      end;

      Dump_Jpeg_Header_Markers (The_Buffer.all);
    end;
  end Read_Raw_IFD;

  --------------------------------------------------------------------------
  -- Public entry
  --------------------------------------------------------------------------

  procedure Read (Filename : String) is
    FD : OS.File_Descriptor := OS.Invalid_FD;

    Base : constant File_Pos := 0;

    B1, B2   : U8;
    Magic    : U16;
    IFD0     : U32;

    CR_A     : U8;
    CR_B     : U8;
    Major    : U8;
    Minor    : U8;
    RawIFD   : U32;

  begin
    Clear;

    FD := OS.Open_Read (Filename, OS.Binary);
    if FD = OS.Invalid_FD then
      raise File_Not_Found;
    end if;

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

    Magic := Get_U16 (FD);
    if Magic /= 16#002A# then
      raise Invalid_File;
    end if;

    IFD0 := Get_U32 (FD);
    pragma Unreferenced (IFD0);

    CR_A := Get_U8 (FD);
    CR_B := Get_U8 (FD);
    if Character'val (Integer (CR_A)) /= 'C'
      or else Character'val (Integer (CR_B)) /= 'R'
    then
      raise Invalid_File;
    end if;

    Major := Get_U8 (FD);
    Minor := Get_U8 (FD);
    pragma Unreferenced (Major, Minor);

    RawIFD := Get_U32 (FD);
    if RawIFD = 0 then
      raise Not_Found;
    end if;

    Read_Raw_IFD (FD, Base, RawIFD);
    OS.Close (FD);

    -- Step 3 decode (updates width/height/bits from SOF3, which is reliable)
    Decode_Lossless_JPEG;

  exception
    when others =>
      if FD /= OS.Invalid_FD then
        OS.Close (FD);
      end if;
      raise;
  end Read;

end Raw_Data;
