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
with GNAT.OS_Lib;
with Interfaces;
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

  type Byte_Array is array (Positive range <>) of U8;
  type Byte_Array_Access is access all Byte_Array;

  procedure Free_Bytes is new Ada.Unchecked_Deallocation
    (Byte_Array, Byte_Array_Access);

  type Endian_Kind is (Little, Big);
  The_Endian : Endian_Kind := Little;

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

  -- TIFF tags in RAW IFD
  Tag_StripOffsets    : constant U16 := 16#0111#;
  Tag_StripByteCounts : constant U16 := 16#0117#;

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

  --------------------------------------------------------------------------
  -- Lossless JPEG decode (SOF3) from a memory buffer
  --------------------------------------------------------------------------

  type Count_1_16 is array (Positive range 1 .. 16) of U8;
  type I16_Array  is array (Positive range 1 .. 16) of Integer;

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
        T.Val_Ptr (I)  := K;
        T.Min_Code (I) := Code;
        Code := Code + Integer (T.Counts (I)) - 1;
        T.Max_Code (I) := Code;
        Code := Code + 1;
        K := K + Integer (T.Counts (I));
      end if;
      Code := Code * 2;
    end loop;
  end Build_Decode;

  type Sof3_Info is record
    Precision : U8 := 0;
    Height    : U16 := 0;
    Width     : U16 := 0;  -- JPEG SOF width (often half sensor width for Canon)
    Comps     : U8 := 0;
  end record;

  type Sos_Info is record
    Ns        : U8 := 0;
    Td0       : U8 := 0;
    Td1       : U8 := 0;
    Predictor : U8 := 1;
    Pt        : U8 := 0;
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

  -- Bit reader for entropy-coded segment (byte-stuffed)
  type Bit_Reader is record
    Buf   : Byte_Array_Access := null;
    Size  : Natural := 0;
    Pos   : Natural := 0;  -- 1-based index into Buf
    Bits  : U32 := 0;
    Cnt   : Natural := 0;
  end record;

  procedure BR_Init (R : in out Bit_Reader; B : Byte_Array_Access; Start_Pos : Natural) is
  begin
    R.Buf  := B;
    R.Size := B'length;
    R.Pos  := Start_Pos;
    R.Bits := 0;
    R.Cnt  := 0;
  end BR_Init;

  function BR_Read_Byte (R : in out Bit_Reader) return U8 is
    B : U8;
  begin
    if R.Pos < 1 or else R.Pos > R.Size then
      raise Invalid_File;
    end if;

    B := R.Buf (R.Pos);
    R.Pos := R.Pos + 1;

    if B = 16#FF# then
      if R.Pos > R.Size then
        raise Invalid_File;
      end if;
      if R.Buf (R.Pos) = 0 then
        R.Pos := R.Pos + 1; -- stuffed 0x00
      end if;
    end if;

    return B;
  end BR_Read_Byte;

  procedure BR_Ensure (R : in out Bit_Reader; N : Natural) is
  begin
    while R.Cnt < N loop
      declare
        B : constant U8 := BR_Read_Byte (R);
      begin
        R.Bits := Interfaces.Shift_Left (R.Bits, 8) or U32 (B);
        R.Cnt  := R.Cnt + 8;
      end;
    end loop;
  end BR_Ensure;

  function BR_Get (R : in out Bit_Reader; N : Natural) return U32 is
    Res  : U32;
    Mask : U32;
  begin
    if N = 0 then
      return 0;
    end if;

    if N > 31 then
      raise Invalid_File;
    end if;

    BR_Ensure (R, N);

    Mask := Interfaces.Shift_Left (1, Integer (N)) - 1;
    Res :=
      Interfaces.Shift_Right (R.Bits, Integer (R.Cnt - N)) and Mask;

    R.Cnt := R.Cnt - N;
    return Res;
  end BR_Get;

  function Decode_Huff (R : in out Bit_Reader; T : Huff_Table) return U8 is
    Code : Integer := 0;
    I    : Natural := 1;
  begin
    loop
      Code := Code * 2 + Integer (BR_Get (R, 1));

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

  function BE16 (B : Byte_Array_Access; P : Natural) return U16 is
  begin
    return Interfaces.Shift_Left (U16 (B (P)), 8) or U16 (B (P + 1));
  end BE16;

  procedure Parse_JPEG_Header
    (B            : Byte_Array_Access;
     Sof          : out Sof3_Info;
     Sos          : out Sos_Info;
     Entropy_Pos  : out Natural) is

    SOI  : constant U16 := 16#FFD8#;
    SOF3 : constant U16 := 16#FFC3#;
    DHT  : constant U16 := 16#FFC4#;
    SOSM : constant U16 := 16#FFDA#;
    EOI  : constant U16 := 16#FFD9#;

    Pos : Natural := 1;

  begin
    Clear_Huffman;
    Entropy_Pos := 0;

    Sof := (others => <>);
    Sos := (others => <>);

    if B = null or else B'length < 4 then
      raise Not_Found;
    end if;

    if BE16 (B, 1) /= SOI then
      raise Not_Found;
    end if;

    Pos := 3; -- after SOI (FFD8)

    loop
      exit when Pos + 1 > B'last;

      -- seek 0xFF
      while Pos <= B'last and then B (Pos) /= 16#FF# loop
        Pos := Pos + 1;
      end loop;
      exit when Pos > B'last;

      -- skip fill 0xFF bytes
      while Pos <= B'last and then B (Pos) = 16#FF# loop
        Pos := Pos + 1;
      end loop;
      exit when Pos > B'last;

      declare
        Marker : constant U16 := 16#FF00# or U16 (B (Pos));
        L      : U16;
        Start  : Natural;
      begin
        Pos := Pos + 1;

        if Marker = EOI then
          exit;
        end if;

        if Pos + 1 > B'last then
          raise Invalid_File;
        end if;

        L := BE16 (B, Pos);
        if L < 2 then
          raise Invalid_File;
        end if;

        Start := Pos + 2; -- payload start

        if Marker = DHT then
          declare
            P    : Natural := Start;
            EndP : constant Natural := Start + Natural (L - 2) - 1;
          begin
            while P <= EndP loop
              declare
                TcTh  : constant U8 := B (P);
                Tc    : constant Natural := Natural (Interfaces.Shift_Right (TcTh, 4));
                Th    : constant Natural := Natural (TcTh and 16#0F#);
                Cnts  : Count_1_16;
                Total : Natural := 0;
              begin
                P := P + 1;
                if Th > 3 or else Tc > 1 then
                  raise Invalid_File;
                end if;

                for I in 1 .. 16 loop
                  Cnts (I) := B (P);
                  P := P + 1;
                  Total := Total + Natural (Cnts (I));
                end loop;

                declare
                  Syms : Sym_Array_Access := null;
                begin
                  if Total > 0 then
                    Syms := new Sym_Array (1 .. Total);
                    for I in 1 .. Total loop
                      Syms (I) := B (P);
                      P := P + 1;
                    end loop;
                  end if;

                  if Huff (Th, Tc).Symbols /= null then
                    Free_Syms (Huff (Th, Tc).Symbols);
                  end if;

                  Huff (Th, Tc).Counts  := Cnts;
                  Huff (Th, Tc).Symbols := Syms;
                  Build_Decode (Huff (Th, Tc));
                end;
              end;
            end loop;
          end;

        elsif Marker = SOF3 then
          declare
            P  : Natural := Start;
            Pr : U8;
            H  : U16;
            W  : U16;
            Nc : U8;
          begin
            Pr := B (P);
            P := P + 1;
            H := BE16 (B, P);
            P := P + 2;
            W := BE16 (B, P);
            P := P + 2;
            Nc := B (P);
            P := P + 1;

            Sof.Precision := Pr;
            Sof.Height := H;
            Sof.Width := W;
            Sof.Comps := Nc;

            if Natural (Nc) * 3 > Natural (L - 2) then
              raise Invalid_File;
            end if;
          end;

        elsif Marker = SOSM then
          declare
            P  : Natural := Start;
            Ns : U8;
          begin
            Ns := B (P);
            P := P + 1;

            Sos.Ns := Ns;
            if Ns = 0 then
              raise Invalid_File;
            end if;

            if Ns /= 2 then
              raise Unsupported;
            end if;

            declare
              Cid  : U8 with Unreferenced;
              TdTa : U8;
            begin
              Cid := B (P); P := P + 1;
              TdTa := B (P); P := P + 1;
              Sos.Td0 := Interfaces.Shift_Right (TdTa, 4);

              Cid := B (P); P := P + 1;
              TdTa := B (P); P := P + 1;
              Sos.Td1 := Interfaces.Shift_Right (TdTa, 4);
            end;

            Sos.Predictor := B (P);
            P := P + 1;

            P := P + 1; -- Se

            declare
              AhAl : constant U8 := B (P);
            begin
              P := P + 1;
              Sos.Pt := AhAl and 16#0F#;
            end;

            if Sos.Predictor < 1 or else Sos.Predictor > 7 then
              raise Invalid_File;
            end if;
            if Sos.Pt > 15 then
              raise Invalid_File;
            end if;

            Entropy_Pos := Start + Natural (L - 2);
            return;
          end;

        end if;

        Pos := Pos + Natural (L);
      end;
    end loop;

    raise Not_Found;
  end Parse_JPEG_Header;

  function Decode_Lossless_To_Grid
    (B    : Byte_Array_Access;
     Size : Square_Size) return Raw_Grid is

    Sof : Sof3_Info;
    Sos : Sos_Info;
    Entropy_Pos : Natural := 0;

  begin
    Parse_JPEG_Header (B, Sof, Sos, Entropy_Pos);

    if Entropy_Pos = 0 then
      raise Not_Found;
    end if;

    if Sof.Comps /= 2 or else Sos.Ns /= 2 then
      raise Unsupported;
    end if;

    declare
      W_Half : constant Natural := Natural (Sof.Width);
      H      : constant Natural := Natural (Sof.Height);
      W_Full : constant Natural := W_Half * 2;

      Needed : constant Natural := Natural (Size);

      Row0 : constant Integer := Integer (H / 2) - Integer (Needed / 2);
      Col0 : constant Integer := Integer (W_Full / 2) - Integer (Needed / 2);

      function In_Range return Boolean is
      begin
        return Row0 >= 0
          and then Col0 >= 0
          and then Row0 + Integer (Needed) <= Integer (H)
          and then Col0 + Integer (Needed) <= Integer (W_Full);
      end In_Range;

      type Int_Row is array (Positive range <>) of Integer;
      Prev0 : Int_Row (1 .. W_Half) := [others => 0];
      Prev1 : Int_Row (1 .. W_Half) := [others => 0];
      Curr0 : Int_Row (1 .. W_Half) := [others => 0];
      Curr1 : Int_Row (1 .. W_Half) := [others => 0];

      R : Bit_Reader;

      Td0 : constant Natural := Natural (Sos.Td0);
      Td1 : constant Natural := Natural (Sos.Td1);

      T0  : constant Huff_Table := Huff (Td0, 0);
      T1  : constant Huff_Table := Huff (Td1, 0);

      Pt  : constant Natural := Natural (Sos.Pt);
      Sel : constant U8 := Sos.Predictor;

      Result : Raw_Grid (Rows (1) .. Rows (Needed), Columns (1) .. Columns (Needed));

      function Pow2 (N : Natural) return Long_Long_Integer is
      begin
        if N > 30 then
          raise Invalid_File;
        end if;
        return Long_Long_Integer (2) ** Integer (N);
      end Pow2;

      procedure Put_Sample
        (Y       : Natural;
         X_Full  : Natural;
         Value   : Long_Long_Integer) is
        Y0 : constant Integer := Integer (Y) - 1;
        X0 : constant Integer := Integer (X_Full) - 1;
      begin
        if Y0 >= Row0 and then Y0 < Row0 + Integer (Needed)
          and then X0 >= Col0 and then X0 < Col0 + Integer (Needed)
        then
          declare
            Rr : constant Rows := Rows (1 + (Y0 - Row0));
            Cc : constant Columns := Columns (1 + (X0 - Col0));
            V  : Long_Long_Integer := Value;
          begin
            if V < 0 then
              V := 0;
            elsif V > Long_Long_Integer (Pixel'last) then
              V := Long_Long_Integer (Pixel'last);
            end if;
            Result (Rr, Cc) := Pixel (Natural (V));
          end;
        end if;
      end Put_Sample;

      -- IMPORTANT: predictor domain is *reduced precision* (Precision - Pt)
      Bits_Total   : constant Natural := Natural (Sof.Precision);
      Bits_Reduced : constant Natural := (if Bits_Total >= Pt then Bits_Total - Pt else 0);

      Init_Pred : constant Integer :=
        (if Bits_Reduced = 0 then 0 else Integer (Pow2 (Bits_Reduced - 1)));

    begin
      if not In_Range then
        raise Size_Error;
      end if;

      if T0.Symbols = null or else T1.Symbols = null then
        raise Invalid_File;
      end if;

      if Bits_Reduced = 0 then
        raise Invalid_File;
      end if;

      BR_Init (R, B, Entropy_Pos);

      for Y in 1 .. H loop
        Curr0 := [others => 0];
        Curr1 := [others => 0];

        for X in 1 .. W_Half loop
          -- component 0 => full col 2*X-1
          declare
            Left   : constant Integer := (if X > 1 then Curr0 (X - 1) else Init_Pred);
            Above  : constant Integer := (if Y > 1 then Prev0 (X)     else Init_Pred);
            UpLeft : constant Integer := (if X > 1 and then Y > 1 then Prev0 (X - 1) else Init_Pred);
            Pred   : constant Integer := Predictor (Sel, Left, Above, UpLeft);
            SSSS   : constant U8 := Decode_Huff (R, T0);
            Diff_S : Integer := 0;
            Rec    : Integer;
            Samp   : Long_Long_Integer;
          begin
            if SSSS /= 0 then
              Diff_S := Extend_Signed (Integer (BR_Get (R, Natural (SSSS))), Natural (SSSS));
            end if;

            -- IMPORTANT: store reconstructed sample in reduced domain
            Rec := Pred + Diff_S;
            Curr0 (X) := Rec;

            -- output scale-up happens only here
            Samp := Long_Long_Integer (Rec) * Pow2 (Pt);
            Put_Sample (Y, 2 * X - 1, Samp);
          end;

          -- component 1 => full col 2*X
          declare
            Left   : constant Integer := (if X > 1 then Curr1 (X - 1) else Init_Pred);
            Above  : constant Integer := (if Y > 1 then Prev1 (X)     else Init_Pred);
            UpLeft : constant Integer := (if X > 1 and then Y > 1 then Prev1 (X - 1) else Init_Pred);
            Pred   : constant Integer := Predictor (Sel, Left, Above, UpLeft);
            SSSS   : constant U8 := Decode_Huff (R, T1);
            Diff_S : Integer := 0;
            Rec    : Integer;
            Samp   : Long_Long_Integer;
          begin
            if SSSS /= 0 then
              Diff_S := Extend_Signed (Integer (BR_Get (R, Natural (SSSS))), Natural (SSSS));
            end if;

            Rec := Pred + Diff_S;
            Curr1 (X) := Rec;

            Samp := Long_Long_Integer (Rec) * Pow2 (Pt);
            Put_Sample (Y, 2 * X, Samp);
          end;
        end loop;

        Prev0 := Curr0;
        Prev1 := Curr1;
      end loop;

      return Result;
    end;
  end Decode_Lossless_To_Grid;

  --------------------------------------------------------------------------
  -- Read RAW IFD: concatenate strips into one buffer (contains lossless JPEG)
  --------------------------------------------------------------------------

  function Read_Raw_Strips (Filename : String) return Byte_Array_Access is
    FD   : OS.File_Descriptor := OS.Invalid_FD;
    Base : constant File_Pos := 0;

    B1, B2 : U8;
    Magic  : U16;
    IFD0   : U32 with Unreferenced;

    CR_A   : U8;
    CR_B   : U8;
    Major  : U8 with Unreferenced;
    Minor  : U8 with Unreferenced;
    RawIFD : U32;

    Strip_Offsets : U32_Array (1 .. 1) := [1 => 0];
    Strip_Counts  : U32_Array (1 .. 1) := [1 => 0];

    Buffer : Byte_Array_Access := null;

  begin
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

    CR_A := Get_U8 (FD);
    CR_B := Get_U8 (FD);
    if Character'val (Integer (CR_A)) /= 'C'
      or else Character'val (Integer (CR_B)) /= 'R'
    then
      raise Invalid_File;
    end if;

    Major := Get_U8 (FD);
    Minor := Get_U8 (FD);

    RawIFD := Get_U32 (FD);
    if RawIFD = 0 then
      raise Not_Found;
    end if;

    Seek_Set (FD, Base + File_Pos (RawIFD));
    declare
      Count : constant U16 := Get_U16 (FD);
    begin
      for Unused_I in 1 .. Integer (Count) loop
        declare
          Tag   : constant U16 := Get_U16 (FD);
          Typ   : constant U16 := Get_U16 (FD);
          Cnt   : constant U32 := Get_U32 (FD);
          ValOr : constant U32 := Get_U32 (FD);
        begin
          case Tag is
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
        Next_IFD : constant U32 := Get_U32 (FD) with Unreferenced;
      begin
        null;
      end;
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

      Buffer := new Byte_Array (1 .. Positive (Natural (Total)));

      declare
        Pos : Natural := 1;
      begin
        for I in Strip_Offsets'range loop
          declare
            Off : constant U32 := Strip_Offsets (I);
            Len : constant Natural := Natural (Strip_Counts (I));
          begin
            Seek_Set (FD, Base + File_Pos (Off));
            Read_Exact (FD, Buffer (Pos)'address, Positive (Len));
            Pos := Pos + Len;
          end;
        end loop;
      end;
    end;

    OS.Close (FD);
    return Buffer;

  exception
    when others =>
      if FD /= OS.Invalid_FD then
        OS.Close (FD);
      end if;
      if Buffer /= null then
        Free_Bytes (Buffer);
      end if;
      raise;
  end Read_Raw_Strips;

  --------------------------------------------------------------------------
  -- Public API
  --------------------------------------------------------------------------

  function Grid
    (Filename : String;
     Size     : Square_Size) return Raw_Grid is

    Buf : Byte_Array_Access := null;

  begin
    Buf := Read_Raw_Strips (Filename);

    declare
      G : constant Raw_Grid := Decode_Lossless_To_Grid (Buf, Size);
    begin
      Free_Bytes (Buf);
      return G;
    end;

  exception
    when others =>
      if Buf /= null then
        Free_Bytes (Buf);
      end if;
      raise;
  end Grid;

end Raw_Data;
