--  A PNG stream is made of several "chunks" (see type PNG_Chunk_tag).
--  The image itself is contained in the IDAT chunk(s).
--
--  Steps for decoding an image (step numbers are from the ISO standard):
--
--  10: Inflate deflated data; at each output buffer (slide),
--        process with step 9.
--   9: Read filter code (row begin), or unfilter bytes, go with step 8
--   8: Display pixels these bytes represent;
--        eventually, locate the interlaced image current point
--
--  Reference: Portable Network Graphics (PNG) Specification (Third Edition)
--  W3C Recommendation 21 September 2023
--  http://www.w3.org/TR/PNG/
--
with GID.Buffering,
     GID.Decoding_PNG.Huffman;

with Ada.Text_IO;

package body GID.Decoding_PNG is
  use Interfaces;

  generic
    type Number is mod <>;
  procedure Big_Endian_Number
    (from : in out Input_Buffer;
     n    :    out Number);
  pragma Inline (Big_Endian_Number);

  procedure Big_Endian_Number
    (from : in out Input_Buffer;
     n    :    out Number)
  is
    b : U8;
  begin
    n := 0;
    for Unused in 1 .. Number'size / 8 loop
      Buffering.Get_Byte (from, b);
      n := n * 256 + Number (b);
    end loop;
  end Big_Endian_Number;

  procedure Big_Endian is new Big_Endian_Number (U16);
  procedure Big_Endian is new Big_Endian_Number (U32);

  function Chunk_Info (kind : PNG_Chunk_Tag) return String is
  (case kind is
     when IHDR => "Image Header",             --  11.2.1
     when PLTE => "Palette",                  --  11.2.2
     when IDAT => "Image Data",               --  11.2.3
     when IEND => "Image Trailer",            --  11.2.4
     when tEXt => "Textual Data",             --  11.3.3.2
     when acTL => "Animation Control Chunk",  --  11.3.6.1 (APNG)
     when fcTL => "Frame Control Chunk",      --  11.3.6.2 (APNG)
     when fdAT => "Frame Data Chunk",         --  11.3.6.3 (APNG)
     when unknown_ancillary_chunk =>
       "(unknown)",
     when others =>
       "");

  procedure Read_Chunk_Header
    (image : in out Image_Descriptor; ch : out Chunk_Header)
  is
    str4 : String (1 .. 4);
    b : U8;
  begin
    Big_Endian (image.buffer, ch.length);
    for i in str4'range loop
      Buffering.Get_Byte (image.buffer, b);
      str4 (i) := Character'val (b);
    end loop;
    begin
      ch.kind := PNG_Chunk_Tag'value (str4);
    exception
      --  13.1 Error handling
      when Constraint_Error =>
        if str4 (1) in 'a' .. 'z' then
          --  "Encountering an unknown ancillary chunk is never an error.
          --   The chunk can simply be ignored."
          ch.kind := unknown_ancillary_chunk;
        else
          --  Anything else than 'a' .. 'z':
          --  an unknown critical chunk ('A' .. 'Z'), or corrupt data.
          --  "Encountering an unknown critical chunk is a fatal condition"
          raise error_in_image_data with
            "PNG chunk is unknown: " &
            Character'pos (str4 (1))'image &
            Character'pos (str4 (2))'image &
            Character'pos (str4 (3))'image &
            Character'pos (str4 (4))'image &
            " (" & str4 & "), or PNG data is corrupt";
        end if;
    end;
    if some_trace then
      Ada.Text_IO.Put_Line
        ("Chunk [" & str4 & "], length:" & ch.length'image &
         "   " & Chunk_Info (ch.kind));
    end if;
  end Read_Chunk_Header;

  package CRC32 is

    procedure Init (CRC : out Unsigned_32);

    function  Final (CRC : Unsigned_32) return Unsigned_32;

    procedure Update (CRC : in out Unsigned_32; InBuf : Byte_Array);
    pragma Inline (Update);

  end CRC32;

  package body CRC32 is

    CRC32_Table : array (Unsigned_32'(0) .. 255) of Unsigned_32;

    procedure Prepare_Table is
      --  CRC-32 algorithm, ISO-3309
      Seed : constant := 16#EDB88320#;
      l : Unsigned_32;
    begin
      for i in CRC32_Table'range loop
        l := i;
        for Unused in 0 .. 7 loop
          if (l and 1) = 0 then
            l := Shift_Right (l, 1);
          else
            l := Shift_Right (l, 1) xor Seed;
          end if;
        end loop;
        CRC32_Table (i) := l;
      end loop;
    end Prepare_Table;

    procedure Update (CRC : in out Unsigned_32; InBuf : Byte_Array) is
      local_CRC : Unsigned_32;
    begin
      local_CRC := CRC;
      for i in InBuf'range loop
        local_CRC :=
          CRC32_Table (16#FF# and (local_CRC xor Unsigned_32 (InBuf (i))))
          xor
          Shift_Right (local_CRC, 8);
      end loop;
      CRC := local_CRC;
    end Update;

    table_empty : Boolean := True;

    procedure Init (CRC : out Unsigned_32) is
    begin
      if table_empty then
        Prepare_Table;
        table_empty := False;
      end if;
      CRC := 16#FFFF_FFFF#;
    end Init;

    function Final (CRC : Unsigned_32) return Unsigned_32 is
    begin
      return not CRC;
    end Final;

  end CRC32;

  ----------
  -- Load --
  ----------

  procedure Load
    (image      : in out Image_Descriptor;
     next_frame :    out Ada.Calendar.Day_Duration)
  is

    ch : Chunk_Header;

    ----------------------
    -- Load_specialized --
    ----------------------

    generic
      --  These values are invariant through the whole picture,
      --  so we can make them generic parameters. As a result, all
      --  "if", "case", etc. using them at the center of the decoding
      --  are optimized out at compile-time.
      interlaced         : Boolean;
      png_bits_per_pixel : Positive;
      bytes_to_unfilter  : Positive;
        --  ^ amount of bytes to unfilter at a time
        --  = Integer'Max(1, bits_per_pixel / 8);
      subformat_id       : Natural;
    procedure Load_specialized;
    --
    procedure Load_specialized is
      frame_width  : constant Positive := Positive (image.PNG_stuff.frame_width);
      frame_height : constant Positive := Positive (image.PNG_stuff.frame_height);
      x_offset     : constant Natural  := Natural (image.PNG_stuff.x_offset);
      y_offset     : constant Natural  := Natural (image.PNG_stuff.y_offset);

      --  The target Y coordinates are bottom-up (0 is the absolute bottom).
      frame_top : constant Natural := Positive (image.height) - 1 - y_offset;

      subtype Mem_row_bytes_array is Byte_Array (0 .. frame_width * 8);
      --
      mem_row_bytes : array (0 .. 1) of Mem_row_bytes_array;
      --  We need to memorize two image rows, for un-filtering
      curr_row : Natural := 1;
      --  either current is 1 and old is 0, or the reverse

      subtype X_range is Integer range -1 .. frame_width  - 1;
      subtype Y_range is Integer range  0 .. frame_height - 1;
      --  X position -1 is for the row's filter methode code

      --  Coordinates are relative to the frame's corner.
      x : X_range := X_range'first;
      y : Y_range := Y_range'first;

      x_max : X_range;  --  for non-interlaced images:   = X_range'Last
      y_max : Y_range;  --  for non-interlaced images:   = Y_range'Last

      pass : Positive range 1 .. 7 := 1;

      --------------------------
      -- ** 9: Unfiltering ** --
      --------------------------
      --  http://www.w3.org/TR/PNG/#9Filters

      type Filter_method_0 is (None, Sub, Up, Average, Paeth);

      current_filter : Filter_method_0;

      procedure Unfilter_bytes
        (f : in  Byte_Array;  --  filtered
         u : out Byte_Array)  --  unfiltered
      is
      pragma Inline (Unfilter_bytes);
        --  Byte positions (f is the byte to be unfiltered):
        --
        --  c b
        --  a f
        a, b, c, p, pa, pb, pc, pr : Integer;
        j : Integer := 0;
      begin
        if full_trace and then x = 0 then
          if y = 0 then
            Ada.Text_IO.New_Line;
          end if;
          Ada.Text_IO.Put_Line
            ("row" & y'image & ": filter= " & current_filter'image);
        end if;
        --
        --  !! find a way to have f99n0g04.png decoded correctly...
        --     seems a filter issue.
        --
        case current_filter is
          when None    =>
            --  Recon(x) = Filt(x)
            u := f;
          when Sub     =>
            --  Recon(x) = Filt(x) + Recon(a)
            if x > 0 then
              for i in f'range loop
                u (u'first + j) := f (i) + mem_row_bytes (curr_row)((x - 1) * bytes_to_unfilter + j);
                j := j + 1;
              end loop;
            else
              u := f;
            end if;
          when Up      =>
            --  Recon(x) = Filt(x) + Recon(b)
            if y > 0 then
              for i in f'range loop
                u (u'first + j) := f (i) + mem_row_bytes (1 - curr_row)(x * bytes_to_unfilter + j);
                j := j + 1;
              end loop;
            else
              u := f;
            end if;
          when Average =>
            --  Recon(x) = Filt(x) + floor((Recon(a) + Recon(b)) / 2)
            for i in f'range loop
              if x > 0 then
                a := Integer (mem_row_bytes (curr_row)((x - 1) * bytes_to_unfilter + j));
              else
                a := 0;
              end if;
              if y > 0 then
                b := Integer (mem_row_bytes (1 - curr_row)(x * bytes_to_unfilter + j));
              else
                b := 0;
              end if;
              u (u'first + j) := U8 ((Integer (f (i)) + (a + b) / 2) mod 256);
              j := j + 1;
            end loop;
          when Paeth   =>
            --  Recon(x) = Filt(x) + PaethPredictor(Recon(a), Recon(b), Recon(c))
            for i in f'range loop
              if x > 0 then
                a := Integer (mem_row_bytes (curr_row)((x - 1) * bytes_to_unfilter + j));
              else
                a := 0;
              end if;
              if y > 0 then
                b := Integer (mem_row_bytes (1 - curr_row)(x * bytes_to_unfilter + j));
              else
                b := 0;
              end if;
              if x > 0 and y > 0 then
                c := Integer (mem_row_bytes (1 - curr_row)((x - 1) * bytes_to_unfilter + j));
              else
                c := 0;
              end if;
              p := a + b - c;
              pa := abs (p - a);
              pb := abs (p - b);
              pc := abs (p - c);
              if pa <= pb and then pa <= pc then
                pr := a;
              elsif pb <= pc then
                pr := b;
              else
                pr := c;
              end if;
              u (u'first + j) := f (i) + U8 (pr);
              j := j + 1;
            end loop;
        end case;
        j := 0;
        for i in u'range loop
          mem_row_bytes (curr_row)(x * bytes_to_unfilter + j) := u (i);
          j := j + 1;
        end loop;
        --  if u'Length /= bytes_to_unfilter then
        --    raise Constraint_Error;
        --  end if;
      end Unfilter_bytes;

      filter_stat : array (Filter_method_0) of Natural := [others => 0];

      ----------------------------------------------
      -- ** 8: Interlacing and pass extraction ** --
      ----------------------------------------------
      --  http://www.w3.org/TR/PNG/#8Interlace

      --  Output bytes from decompression
      --
      procedure Output_uncompressed (
        data  : in     Byte_Array;
        reject :    out Natural
        --  amount of bytes to be resent here next time,
        --  in order to have a full multi-byte pixel
      )
      is
        --  Display of pixels coded on 8 bits per channel in the PNG stream
        procedure Out_Pixel_8 (br, bg, bb, ba : U8) is
        pragma Inline (Out_Pixel_8);
          function Times_257 (x : Primary_Color_Range) return Primary_Color_Range is
          pragma Inline (Times_257);
          begin
            return 16 * (16 * x) + x;  --  this is 257 * x, = 16#0101# * x
            --  Numbers 8-bit -> no OA warning at instantiation.
            --  Returns x if type Primary_color_range is mod 2**8.
          end Times_257;
        begin
          case Primary_Color_Range'modulus is
            when 256 =>
              Put_Pixel
                (Primary_Color_Range (br),
                 Primary_Color_Range (bg),
                 Primary_Color_Range (bb),
                 Primary_Color_Range (ba));
            when 65_536 =>
              Put_Pixel
                (Times_257 (Primary_Color_Range (br)),
                 Times_257 (Primary_Color_Range (bg)),
                 Times_257 (Primary_Color_Range (bb)),
                 Times_257 (Primary_Color_Range (ba)));
                 --  Times_257 makes max intensity FF go to FFFF
            when others =>
              raise invalid_primary_color_range
                with "PNG: color range not supported";
          end case;
        end Out_Pixel_8;

        procedure Out_Pixel_Palette (ix : U8) is
        pragma Inline (Out_Pixel_Palette);
          color_idx : constant Natural := Integer (ix);
        begin
          Out_Pixel_8 (
            image.palette (color_idx).red,
            image.palette (color_idx).green,
            image.palette (color_idx).blue,
            255
          );
        end Out_Pixel_Palette;

        --  Display of pixels coded on 16 bits per channel in the PNG stream
        procedure Out_Pixel_16 (br, bg, bb, ba : U16) is
        pragma Inline (Out_Pixel_16);
        begin
          case Primary_Color_Range'modulus is
            when 256 =>
              Put_Pixel (
                Primary_Color_Range (br / 256),
                Primary_Color_Range (bg / 256),
                Primary_Color_Range (bb / 256),
                Primary_Color_Range (ba / 256)
              );
            when 65_536 =>
              Put_Pixel (
                Primary_Color_Range (br),
                Primary_Color_Range (bg),
                Primary_Color_Range (bb),
                Primary_Color_Range (ba)
              );
            when others =>
              raise invalid_primary_color_range with "PNG: color range not supported";
          end case;
        end Out_Pixel_16;

        procedure Inc_XY is
        pragma Inline (Inc_XY);
          xm, ym : Integer;
        begin
          if x < x_max then
            x := x + 1;
            if interlaced then
              --  Position of pixels depending on pass:
              --
              --   1 6 4 6 2 6 4 6
              --   7 7 7 7 7 7 7 7
              --   5 6 5 6 5 6 5 6
              --   7 7 7 7 7 7 7 7
              --   3 6 4 6 3 6 4 6
              --   7 7 7 7 7 7 7 7
              --   5 6 5 6 5 6 5 6
              --   7 7 7 7 7 7 7 7
              case pass is
                when 1 =>
                  Set_X_Y (x_offset + x * 8,     frame_top     - y * 8);
                when 2 =>
                  Set_X_Y (x_offset + 4 + x * 8, frame_top     - y * 8);
                when 3 =>
                  Set_X_Y (x_offset + x * 4,     frame_top - 4 - y * 8);
                when 4 =>
                  Set_X_Y (x_offset + 2 + x * 4, frame_top     - y * 4);
                when 5 =>
                  Set_X_Y (x_offset + x * 2,     frame_top - 2 - y * 4);
                when 6 =>
                  Set_X_Y (x_offset + 1 + x * 2, frame_top     - y * 2);
                when 7 =>
                  null;  --  Nothing to to, pixels are contiguous
              end case;
            end if;
          else
            --  New row
            x := X_range'first;
            if y < y_max then
              y := y + 1;
              curr_row := 1 - curr_row;  --  Swap row index for filtering
              if not interlaced then
                Feedback ((y * 100) / frame_height);
              end if;
              if x_offset /= 0 then
                Set_X_Y (x_offset, frame_top - y);
              end if;
            elsif interlaced then  --  Last row has beed displayed
              while pass < 7 loop
                pass := pass + 1;
                y := 0;
                case pass is
                  when 1 =>
                    null;
                  when 2 =>
                    xm := (frame_width  + 3) / 8 - 1;
                    ym := (frame_height + 7) / 8 - 1;
                  when 3 =>
                    xm := (frame_width  + 3) / 4 - 1;
                    ym := (frame_height + 3) / 8 - 1;
                  when 4 =>
                    xm := (frame_width  + 1) / 4 - 1;
                    ym := (frame_height + 3) / 4 - 1;
                  when 5 =>
                    xm := (frame_width  + 1) / 2 - 1;
                    ym := (frame_height + 1) / 4 - 1;
                  when 6 =>
                    xm := (frame_width) / 2 - 1;
                    ym := (frame_height + 1) / 2 - 1;
                  when 7 =>
                    xm := frame_width      - 1;
                    ym := frame_height / 2 - 1;
                end case;
                if xm >= 0 and xm <= X_range'last and ym in Y_range then
                  --  This pass is not empty (otherwise, we will continue
                  --  to the next one, if any).
                  x_max := xm;
                  y_max := ym;
                  exit;
                end if;
              end loop;
            end if;
          end if;
        end Inc_XY;

        uf : Byte_Array (0 .. 15);  --  Unfiltered bytes for a pixel
        w1, w2 : U16;
        i : Integer;

      begin
        if some_trace then
          Ada.Text_IO.Put_Line
            ("[Output Uncompressed]; frame: " &
             frame_width'image & " x " & frame_height'image);
        end if;
        --  Depending on the row size, bpp, etc., we can have
        --  several rows, or less than one, being displayed
        --  with the present uncompressed data batch.
        --
        i := data'first;
        if i > data'last then
          reject := 0;
          return;  --  Data is empty, do nothing
        end if;
        --
        --  Main loop over data
        --
        loop
          if x = X_range'first then  --  Pseudo-column for filter method
            exit when i > data'last;
            begin
              current_filter := Filter_method_0'val (data (i));
              if some_trace then
                filter_stat (current_filter) := filter_stat (current_filter) + 1;
              end if;
            exception
              when Constraint_Error =>
                raise error_in_image_data with
                  "PNG: wrong filter code, row #" & y'image &
                  " code:" & data (i)'image;
            end;
            if interlaced then
              case pass is
                when 1 .. 6 =>
                  null;  --  Set_X_Y is called for each pixel
                when 7 =>
                  Set_X_Y (x_offset, frame_top - 1 - y * 2);
              end case;
            else
              Set_X_Y (x_offset, frame_top - y);
            end if;
            i := i + 1;
          else  --  Normal pixel
            --
            --  We quit the loop if all data has been used
            --  (except for a possible incomplete pixel)
            exit when i > data'last - (bytes_to_unfilter - 1);
            --  NB, for per-channel bpp < 8:
            --  7.2 Scanlines - some low-order bits of the
            --  last byte of a scanline may go unused.
            case subformat_id is
              when 0 =>
                -----------------------
                -- Type 0: Greyscale --
                -----------------------
                case png_bits_per_pixel is
                  when 1 | 2 | 4  =>
                    Unfilter_bytes (data (i .. i), uf (0 .. 0));
                    i := i + 1;
                    declare
                      b : U8;
                      shift : Integer := 8 - png_bits_per_pixel;
                      max : constant U8 := U8 (Shift_Left (Unsigned_32'(1), png_bits_per_pixel) - 1);
                      --  Scaling factor to obtain the correct color value on a 0..255 range.
                      --  The division is exact in all cases (bpp=8,4,2,1),
                      --  since 255 = 3 * 5 * 17 and max = 255, 15, 3 or 1.
                      --  This factor ensures: 0 -> 0, max -> 255
                      factor : constant U8 := 255 / max;
                    begin
                      --  Loop through the number of pixels in this byte:
                      for k in reverse 1 .. 8 / png_bits_per_pixel loop
                        b := (max and U8 (Shift_Right (Unsigned_8 (uf (0)), shift))) * factor;
                        shift := shift - png_bits_per_pixel;
                        Out_Pixel_8 (b, b, b, 255);
                        exit when x >= x_max or k = 1;
                        Inc_XY;
                      end loop;
                    end;
                  when 8 =>
                    --  NB: with bpp as generic param, this case could be merged
                    --  into the general 1,2,4[,8] case without loss of performance
                    --  if the compiler is smart enough to simplify the code, given
                    --  the value of bits_per_pixel.
                    --  But we let it here for two reasons:
                    --    1) a compiler might be not smart enough
                    --    2) it is a very simple case, perhaps helpful for
                    --       understanding the algorithm.
                    Unfilter_bytes (data (i .. i), uf (0 .. 0));
                    i := i + 1;
                    Out_Pixel_8 (uf (0), uf (0), uf (0), 255);
                  when 16 =>
                    Unfilter_bytes (data (i .. i + 1), uf (0 .. 1));
                    i := i + 2;
                    w1 := U16 (uf (0)) * 256 + U16 (uf (1));
                    Out_Pixel_16 (w1, w1, w1, 65535);
                  when others =>
                    null; -- undefined in PNG standard
                end case;
              when 2 =>
                -----------------
                -- Type 2: RGB --
                -----------------
                case png_bits_per_pixel is
                  when 24 =>
                    Unfilter_bytes (data (i .. i + 2), uf (0 .. 2));
                    i := i + 3;
                    Out_Pixel_8 (uf (0), uf (1), uf (2), 255);
                  when 48 =>
                    Unfilter_bytes (data (i .. i + 5), uf (0 .. 5));
                    i := i + 6;
                    Out_Pixel_16 (
                      U16 (uf (0)) * 256 + U16 (uf (1)),
                      U16 (uf (2)) * 256 + U16 (uf (3)),
                      U16 (uf (4)) * 256 + U16 (uf (5)),
                      65_535
                    );
                  when others =>
                    null;
                end case;
              when 3 =>
                ------------------------------
                -- Type 3: RGB with palette --
                ------------------------------
                Unfilter_bytes (data (i .. i), uf (0 .. 0));
                i := i + 1;
                case png_bits_per_pixel is
                  when 1 | 2 | 4 =>
                    declare
                      shift : Integer := 8 - png_bits_per_pixel;
                      max : constant U8 := U8 (Shift_Left (Unsigned_32'(1), png_bits_per_pixel) - 1);
                    begin
                      --  Loop through the number of pixels in this byte:
                      for k in reverse 1 .. 8 / png_bits_per_pixel loop
                        Out_Pixel_Palette (max and U8 (Shift_Right (Unsigned_8 (uf (0)), shift)));
                        shift := shift - png_bits_per_pixel;
                        exit when x >= x_max or k = 1;
                        Inc_XY;
                      end loop;
                    end;
                  when 8 =>
                    --  Same remark for this case (8bpp) as
                    --  within Image Type 0 / Greyscale above
                    Out_Pixel_Palette (uf (0));
                  when others =>
                    null;
                end case;
              when 4 =>
                -------------------------------
                -- Type 4: Greyscale & Alpha --
                -------------------------------
                case png_bits_per_pixel is
                  when 16 =>
                    Unfilter_bytes (data (i .. i + 1), uf (0 .. 1));
                    i := i + 2;
                    Out_Pixel_8 (uf (0), uf (0), uf (0), uf (1));
                  when 32 =>
                    Unfilter_bytes (data (i .. i + 3), uf (0 .. 3));
                    i := i + 4;
                    w1 := U16 (uf (0)) * 256 + U16 (uf (1));
                    w2 := U16 (uf (2)) * 256 + U16 (uf (3));
                    Out_Pixel_16 (w1, w1, w1, w2);
                  when others =>
                    null;  --  Undefined in PNG standard
                end case;
              when 6 =>
                ------------------
                -- Type 6: RGBA --
                ------------------
                case png_bits_per_pixel is
                  when 32 =>
                    Unfilter_bytes (data (i .. i + 3), uf (0 .. 3));
                    i := i + 4;
                    Out_Pixel_8 (uf (0), uf (1), uf (2), uf (3));
                  when 64 =>
                    Unfilter_bytes (data (i .. i + 7), uf (0 .. 7));
                    i := i + 8;
                    Out_Pixel_16 (
                      U16 (uf (0)) * 256 + U16 (uf (1)),
                      U16 (uf (2)) * 256 + U16 (uf (3)),
                      U16 (uf (4)) * 256 + U16 (uf (5)),
                      U16 (uf (6)) * 256 + U16 (uf (7))
                    );
                  when others =>
                    null;
                end case;
              when others =>
                null;  --  Unknown - exception already raised at header level
            end case;
          end if;
          Inc_XY;
        end loop;
        --  i is between data'Last-(bytes_to_unfilter-2) and data'Last+1
        reject := (data'last + 1) - i;
        if reject > 0 then
          if some_trace then
            Ada.Text_IO.Put ("[rj" & Integer'image (reject) & ']');
          end if;
        end if;
      end Output_uncompressed;

      --  Out of some intelligent design, there might be an IDAT chunk
      --  boundary anywhere inside the zlib compressed block...
      procedure Jump_IDAT is
        dummy : U32;
      begin
        Big_Endian (image.buffer, dummy);  --  Ending chunk's CRC
        --  New chunk begins here.
        loop
          Read_Chunk_Header (image, ch);
          exit when ch.kind /= IDAT or ch.length > 0;
        end loop;
        if ch.kind /= IDAT then
          raise error_in_image_data
            with "PNG: additional data chunk must be an IDAT";
        end if;
      end Jump_IDAT;

      ---------------------------------------------------------------------
      -- ** 10: Decompression **                                         --
      -- Excerpt and simplification from UnZip.Decompress (Inflate only) --
      ---------------------------------------------------------------------
      --  http://www.w3.org/TR/PNG/#10Compression

      --  Size of sliding dictionary and circular output buffer
      wsize : constant := 16#10000#;

      --------------------------------------
      -- Specifications of UnZ_* packages --
      --------------------------------------

      package UnZ_Glob is
        --  I/O Buffers
        --  > Sliding dictionary for unzipping, and output buffer as well
        slide : Byte_Array (0 .. wsize) with Warnings => Off;
        slide_index : Integer := 0;  --  Current Position in slide
        Zip_EOF  : constant Boolean := False;
        crc32val : Unsigned_32 with Warnings => Off;  --  CRC calculated from data
      end UnZ_Glob;

      package UnZ_IO is

        procedure Init_Buffers;

        procedure Read_raw_byte (bt : out U8);
          pragma Inline (Read_raw_byte);

        package Bit_buffer is
          procedure Init;
          --  Read at least n bits into the bit buffer, returns the n first bits
          function Read (n : Natural) return Integer;
            pragma Inline (Read);
          function Read_U32 (n : Natural) return Unsigned_32;
            pragma Inline (Read_U32);
          --  Dump n bits no longer needed from the bit buffer
          procedure Dump (n : Natural);
            pragma Inline (Dump);
          procedure Dump_to_byte_boundary;
          function Read_and_dump (n : Natural) return Integer;
            pragma Inline (Read_and_dump);
          function Read_and_dump_U32 (n : Natural) return Unsigned_32;
            pragma Inline (Read_and_dump_U32);
        end Bit_buffer;

        procedure Flush (x : Natural);  --  Directly from slide to output stream

        procedure Flush_if_full (W : in out Integer);
          pragma Inline (Flush_if_full);

        procedure Copy
          (distance, length :        Natural;
           index            : in out Natural);
        pragma Inline (Copy);

      end UnZ_IO;

      package UnZ_Meth is
        procedure Inflate;
      end UnZ_Meth;

      ------------------------------
      -- Bodies of UnZ_* packages --
      ------------------------------
      package body UnZ_IO is

        procedure Init_Buffers is
        begin
          UnZ_Glob.slide_index := 0;
          Bit_buffer.Init;
          CRC32.Init (UnZ_Glob.crc32val);
        end Init_Buffers;

        procedure Read_raw_byte (bt : out U8) is
        begin
          if ch.length = 0 then
            --  We hit the end of a PNG 'IDAT' chunk, so we go to the next one
            --  - in petto, it's strange design, but well...
            --  This "feature" has taken some time (and nerves) to be addressed.
            --  Incidentally, to solve the mystery, I have reprogrammed the
            --  whole Huffman decoding, and looked at many other wrong places!
            Jump_IDAT;
          end if;
          Buffering.Get_Byte (image.buffer, bt);
          ch.length := ch.length - 1;
        end Read_raw_byte;

        package body Bit_buffer is
          B : Unsigned_32;
          K : Integer;

          procedure Init is
          begin
            B := 0;
            K := 0;
          end Init;

          procedure Need (n : Natural) is
            pragma Inline (Need);
            bt : U8;
          begin
            while K < n loop
              Read_raw_byte (bt);
              B := B or Shift_Left (Unsigned_32 (bt), K);
              K := K + 8;
            end loop;
          end Need;

          procedure Dump (n : Natural) is
          begin
            B := Shift_Right (B, n);
            K := K - n;
          end Dump;

          procedure Dump_to_byte_boundary is
          begin
            Dump (K mod 8);
          end Dump_to_byte_boundary;

          function Read_U32 (n : Natural) return Unsigned_32 is
          begin
            Need (n);
            return B and (Shift_Left (1, n) - 1);
          end Read_U32;

          function Read (n : Natural) return Integer is
          begin
            return Integer (Read_U32 (n));
          end Read;

          function Read_and_dump (n : Natural) return Integer is
            res : Integer;
          begin
            res := Read (n);
            Dump (n);
            return res;
          end Read_and_dump;

          function Read_and_dump_U32 (n : Natural) return Unsigned_32 is
            res : Unsigned_32;
          begin
            res := Read_U32 (n);
            Dump (n);
            return res;
          end Read_and_dump_U32;

        end Bit_buffer;

        old_bytes : Natural := 0;
        --  ^ How many bytes to be resent from last Inflate output?
        byte_mem : Byte_Array (1 .. 8);

        procedure Flush (x : Natural) is
        begin
          if full_trace then
            Ada.Text_IO.Put ("[Flush..." & x'image);
          end if;
          CRC32.Update (UnZ_Glob.crc32val, UnZ_Glob.slide (0 .. x - 1));
          if old_bytes > 0 then
            declare
              app : constant Byte_Array :=
                byte_mem (1 .. old_bytes) & UnZ_Glob.slide (0 .. x - 1);
            begin
              Output_uncompressed (app, old_bytes);
              --  In extreme cases (x very small), we might have some of
              --  the rejected bytes from byte_mem.
              if old_bytes > 0 then
                byte_mem (1 .. old_bytes) := app (app'last - (old_bytes - 1) .. app'last);
              end if;
            end;
          else
            Output_uncompressed (UnZ_Glob.slide (0 .. x - 1), old_bytes);
            if old_bytes > 0 then
              byte_mem (1 .. old_bytes) := UnZ_Glob.slide (x - old_bytes .. x - 1);
            end if;
          end if;
          if full_trace then
            Ada.Text_IO.Put_Line ("finished]");
          end if;
        end Flush;

        procedure Flush_if_full (W : in out Integer) is
        begin
          if W = wsize then
            Flush (wsize);
            W := 0;
          end if;
        end Flush_if_full;

        ----------------------------------------------------
        -- Reproduction of sequences in the output slide. --
        ----------------------------------------------------

        --  Internal:

        procedure Adjust_to_Slide
          (source         : in out Integer;
           remain         : in out Natural;
           part           :    out Integer;
           index          : in     Integer)
        is
          pragma Inline (Adjust_to_Slide);
        begin
          source := source mod wsize;
          --  source and index are now in 0..WSize-1
          if  source > index then
            part := wsize - source;
          else
            part := wsize - index;
          end if;
          --  NB: part is in 1..WSize (part cannot be 0)
          if part > remain then
            part := remain;
          end if;
          --  Now part <= remain
          remain := remain - part;
          --  NB: remain cannot be < 0
        end Adjust_to_Slide;

        procedure Copy_range (source, index : in out Natural; amount : Positive) is
          pragma Inline (Copy_range);
        begin
          if abs (index - source) < amount then
            --  if source >= index, the effect of copy is
            --  just like the non-overlapping case
            for Unused in reverse 1 .. amount loop
              UnZ_Glob.slide (index) := UnZ_Glob.slide (source);
              index := index  + 1;
              source := source + 1;
            end loop;
          else  --  non-overlapping -> copy slice
            UnZ_Glob.slide (index .. index + amount - 1) :=
              UnZ_Glob.slide (source .. source + amount - 1);
            index := index  + amount;
            source := source + amount;
          end if;
        end Copy_range;

        --  The copying routines:

        procedure Copy (
            distance, length :        Natural;
            index           : in out Natural)
        is
          source, part, remain : Integer;
        begin
          source := index - distance;
          remain := length;
          loop
            Adjust_to_Slide (source, remain, part, index);
            Copy_range (source, index, part);
            Flush_if_full (index);
            exit when remain = 0;
          end loop;
        end Copy;

      end UnZ_IO;

      package body UnZ_Meth is

        use GID.Decoding_PNG.Huffman;

        --------[ Method: Inflate ]--------

        procedure Inflate_Codes (Tl, Td : p_Table_list; Bl, Bd : Integer) is
          CT     : p_HufT_table;  --  Current table
          CT_idx : Integer;       --  Current table index
          length : Natural;
          E      : Integer;       --  Table entry flag/number of extra bits
          W      : Integer := UnZ_Glob.slide_index;
          --  More local variable for slide index
        begin
          if full_trace then
            Ada.Text_IO.Put_Line ("Begin Inflate_Codes");
          end if;

          --  inflate the coded data
          main_loop :
          while not UnZ_Glob.Zip_EOF loop
            CT := Tl.table;
            CT_idx := UnZ_IO.Bit_buffer.Read (Bl);

            loop
              E := CT (CT_idx).extra_bits;
              exit when E <= 16;
              if E = invalid then
                raise error_in_image_data with "PNG: invalid code in Deflate compression";
              end if;

              --  then it's a literal
              UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);
              E := E - 16;
              CT := CT (CT_idx).next_table;
              CT_idx := UnZ_IO.Bit_buffer.Read (E);
            end loop;

            UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);

            case E is
              when 16 =>     -- CTE.N is a Litteral
                UnZ_Glob.slide (W) :=  U8 (CT (CT_idx).n);
                W := W + 1;
                UnZ_IO.Flush_if_full (W);

              when 15 =>     -- End of block (EOB)
                if full_trace then
                  Ada.Text_IO.Put_Line ("Exit  Inflate_codes, e=15 EOB");
                end if;
                exit main_loop;

              when others => -- We have a length/distance

                --  Get length of block to copy:
                length := CT (CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump (E);

                --  Decode distance of block to copy:
                CT := Td.table;
                CT_idx := UnZ_IO.Bit_buffer.Read (Bd);
                loop
                  E := CT (CT_idx).extra_bits;
                  exit when E <= 16;
                  if E = invalid then
                    raise error_in_image_data
                      with "PNG: invalid code in Deflate compression (LZ distance)";
                  end if;
                  UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);
                  E := E - 16;
                  CT := CT (CT_idx).next_table;
                  CT_idx := UnZ_IO.Bit_buffer.Read (E);
                end loop;
                UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);

                UnZ_IO.Copy (
                  distance => CT (CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump (E),
                  length   => length,
                  index    => W
                );
            end case;
          end loop main_loop;

          UnZ_Glob.slide_index := W;

          if full_trace then
            Ada.Text_IO.Put_Line ("End   Inflate_codes");
          end if;
        end Inflate_Codes;

        procedure Inflate_stored_block is -- Actually, nothing to inflate
          N : Integer;
        begin
          if full_trace then
            Ada.Text_IO.Put_Line ("Begin Inflate_stored_block");
          end if;
          UnZ_IO.Bit_buffer.Dump_to_byte_boundary;

          --  Get the block length and its complement
          N := UnZ_IO.Bit_buffer.Read_and_dump (16);
          if  N /= Integer (
           (not UnZ_IO.Bit_buffer.Read_and_dump_U32 (16))
           and 16#ffff#)
          then
            raise error_in_image_data with "PNG: invalid check code in Deflate stored block";
          end if;
          while N > 0  and then not UnZ_Glob.Zip_EOF loop
            --  Read and output the non-compressed data
            N := N - 1;
            UnZ_Glob.slide (UnZ_Glob.slide_index) :=
              U8 (UnZ_IO.Bit_buffer.Read_and_dump (8));
            UnZ_Glob.slide_index := UnZ_Glob.slide_index + 1;
            UnZ_IO.Flush_if_full (UnZ_Glob.slide_index);
          end loop;
          if full_trace then
            Ada.Text_IO.Put_Line ("End   Inflate_stored_block");
          end if;
        end Inflate_stored_block;

        --  Copy lengths for literal codes 257..285

        copy_lengths_literal : constant Length_array (0 .. 30) :=
             [3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
               35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0];

        --  Extra bits for literal codes 257..285

        extra_bits_literal : constant Length_array (0 .. 30) :=
               [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
                 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid, invalid];

        --  Copy offsets for distance codes 0..29 (30..31: deflate_e)

        copy_offset_distance : constant Length_array (0 .. 31) :=
             [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
               257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
               8193, 12289, 16385, 24577, 32769, 49153];

        --  Extra bits for distance codes

        extra_bits_distance : constant Length_array (0 .. 31) :=
             [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
               7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14];

        max_dist : constant := 29;

        procedure Inflate_fixed_block is
          Tl,                        -- literal/length code table
            Td : p_Table_list;            -- distance code table
          Bl, Bd : Integer;          -- lookup bits for tl/bd
          huft_incomplete : Boolean;

          --  length list for HufT_build (literal table)
          L : constant Length_array (0 .. 287) :=
            [0 .. 143 => 8, 144 .. 255 => 9, 256 .. 279 => 7, 280 .. 287 => 8];

        begin
          if full_trace then
            Ada.Text_IO.Put_Line ("Begin Inflate_fixed_block");
          end if;

          --  make a complete, but wrong code set
          Bl := 7;
          HufT_build (
            L, 257, copy_lengths_literal, extra_bits_literal,
            Tl, Bl, huft_incomplete
          );

          --  Make an incomplete code set
          Bd := 5;
          begin
            HufT_build (
              [0 .. max_dist => 5], 0,
              copy_offset_distance, extra_bits_distance,
              Td, Bd, huft_incomplete
            );
            if huft_incomplete then
              if full_trace then
                Ada.Text_IO.Put_Line (
                  "td is incomplete, pointer=null: " &
                  Boolean'image (Td = null)
                );
              end if;
            end if;
          exception
            when huft_out_of_memory | huft_error =>
              HufT_free (Tl);
              raise error_in_image_data
                with "PNG: error in Deflate compression (Huffman #1)";
          end;

          Inflate_Codes (Tl, Td, Bl, Bd);

          HufT_free (Tl);
          HufT_free (Td);

          if full_trace then
            Ada.Text_IO.Put_Line ("End   Inflate_fixed_block");
          end if;
        end Inflate_fixed_block;

        procedure Inflate_dynamic_block is
          bit_order : constant array (0 .. 18) of Natural :=
           [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15];

          Lbits : constant := 9;
          Dbits : constant := 6;

          current_length : Natural := 0;
          defined, number_of_lengths : Natural;

          Tl,                             -- literal/length code tables
            Td : p_Table_list;            -- distance code tables

          CT_dyn_idx : Integer;  -- current table element

          Bl, Bd : Integer;                  -- lookup bits for tl/bd
          Nb : Natural;  -- number of bit length codes
          Nl : Natural;  -- number of literal length codes
          Nd : Natural;  -- number of distance codes

          --  literal/length and distance code lengths
          Ll : Length_array (0 .. 288 + 32 - 1) := [others => 0];

          huft_incomplete : Boolean;

          procedure Repeat_length_code (amount : Natural) is
          begin
            if defined + amount > number_of_lengths then
              raise error_in_image_data
                with "PNG: invalid data in Deflate dynamic compression structure (#1)";
            end if;
            for Unused in reverse 1 .. amount loop
              Ll (defined) := Natural_M32 (current_length);
              defined := defined + 1;
            end loop;
          end Repeat_length_code;

        begin
          if full_trace then
            Ada.Text_IO.Put_Line ("Begin Inflate_dynamic_block");
          end if;

          --  Read in table lengths
          Nl := 257 + UnZ_IO.Bit_buffer.Read_and_dump (5);
          Nd :=   1 + UnZ_IO.Bit_buffer.Read_and_dump (5);
          Nb :=   4 + UnZ_IO.Bit_buffer.Read_and_dump (4);

          if Nl > 288 or else Nd > 32 then
            raise error_in_image_data
                with "PNG: invalid data in Deflate dynamic compression structure (#2)";
          end if;

          --  Read in bit-length-code lengths.
          --  The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
          for J in  0 .. Nb - 1  loop
            Ll (bit_order (J)) := Natural_M32 (UnZ_IO.Bit_buffer.Read_and_dump (3));
          end loop;

          --  Build decoding table for trees--single level, 7 bit lookup
          Bl := 7;
          begin
            HufT_build (
              Ll (0 .. 18), 19, empty, empty, Tl, Bl, huft_incomplete
            );
            if huft_incomplete then
              HufT_free (Tl);
              raise error_in_image_data
                with "PNG: error in Deflate compression (Huffman #2)";
            end if;
          exception
            when others =>
              raise error_in_image_data
                with "PNG: error in Deflate compression (Huffman #3)";
          end;

          --  Read in literal and distance code lengths
          number_of_lengths := Nl + Nd;
          defined := 0;
          current_length := 0;

          while  defined < number_of_lengths  loop
            CT_dyn_idx := UnZ_IO.Bit_buffer.Read (Bl);
            UnZ_IO.Bit_buffer.Dump (Tl.table (CT_dyn_idx).bits);

            case Tl.table (CT_dyn_idx).n is
              when 0 .. 15 =>       -- length of code in bits (0..15)
                current_length := Tl.table (CT_dyn_idx).n;
                Ll (defined) := Natural_M32 (current_length);
                defined := defined + 1;

              when 16 =>          -- repeat last length 3 to 6 times
                Repeat_length_code (3 + UnZ_IO.Bit_buffer.Read_and_dump (2));

              when 17 =>          -- 3 to 10 zero length codes
                current_length := 0;
                Repeat_length_code (3 + UnZ_IO.Bit_buffer.Read_and_dump (3));

              when 18 =>          -- 11 to 138 zero length codes
                current_length := 0;
                Repeat_length_code (11 + UnZ_IO.Bit_buffer.Read_and_dump (7));

              when others =>
                if full_trace then
                  Ada.Text_IO.Put_Line (
                    "Illegal length code: " &
                    Integer'image (Tl.table (CT_dyn_idx).n)
                  );
                end if;

            end case;
          end loop;

          HufT_free (Tl);        -- free decoding table for trees

          --  Build the decoding tables for literal/length codes
          Bl := Lbits;
          begin
            HufT_build (
              Ll (0 .. Nl - 1), 257,
              copy_lengths_literal, extra_bits_literal,
              Tl, Bl, huft_incomplete
            );
            if huft_incomplete then
              HufT_free (Tl);
              raise error_in_image_data
                with "PNG: error in Deflate compression (Huffman #4)";
            end if;
          exception
            when others =>
              raise error_in_image_data
                with "PNG: error in Deflate compression (Huffman #5)";
          end;

          --  Build the decoding tables for distance codes
          Bd := Dbits;
          begin
            HufT_build (
              Ll (Nl .. Nl + Nd - 1), 0,
              copy_offset_distance, extra_bits_distance,
              Td, Bd, huft_incomplete
            );
            if huft_incomplete then -- do nothing!
              if full_trace then
                Ada.Text_IO.Put_Line ("PKZIP 1.93a bug workaround");
              end if;
            end if;
          exception
            when huft_out_of_memory | huft_error =>
              HufT_free (Tl);
              raise error_in_image_data
                with "PNG: error in Deflate compression (Huffman #6)";
          end;

          --  Decompress until an end-of-block code

          Inflate_Codes (Tl, Td, Bl, Bd);
          HufT_free (Tl);
          HufT_free (Td);

          if full_trace then
            Ada.Text_IO.Put_Line ("End   Inflate_dynamic_block");
          end if;
        end Inflate_dynamic_block;

        procedure Inflate_Block (last_block : out Boolean) is
        begin
          last_block := Boolean'val (UnZ_IO.Bit_buffer.Read_and_dump (1));
          case UnZ_IO.Bit_buffer.Read_and_dump (2) is -- Block type = 0,1,2,3
            when 0 =>      Inflate_stored_block;
            when 1 =>      Inflate_fixed_block;
            when 2 =>      Inflate_dynamic_block;
            when others =>
              raise error_in_image_data with
                "PNG: error in Deflate compression: bad block type (3)";
          end case;
        end Inflate_Block;

        procedure Inflate is
          is_last_block : Boolean;
          blocks : Positive := 1;
        begin
          loop
            Inflate_Block (is_last_block);
            exit when is_last_block;
            blocks := blocks + 1;
          end loop;
          UnZ_IO.Flush (UnZ_Glob.slide_index);
          UnZ_Glob.slide_index := 0;
          if some_trace then
            Ada.Text_IO.Put ("# blocks:" & Integer'image (blocks));
          end if;
          UnZ_Glob.crc32val := CRC32.Final (UnZ_Glob.crc32val);
        end Inflate;

      end UnZ_Meth;

      --------------------------------------------------------------------
      -- End of the Decompression part, and of UnZip.Decompress excerpt --
      --------------------------------------------------------------------

      b : U8;
      z_crc : U32;

      procedure Decompress_Data is
      begin
        UnZ_IO.Read_raw_byte (b);  --  zlib compression method/flags code
        UnZ_IO.Read_raw_byte (b);  --  Additional flags/check bits
        --
        UnZ_IO.Init_Buffers;
        --  ^ we indicate that we have a byte reserve of chunk's length,
        --    minus both zlib header bytes.
        UnZ_Meth.Inflate;
        z_crc := 0;
        for Unused in 1 .. 4 loop
          UnZ_IO.Read_raw_byte (b);
          z_crc := z_crc * 256 + U32 (b);
        end loop;
        --  z_crc : zlib Check value
        --  if z_crc /= U32(UnZ_Glob.crc32val) then
        --    ada.text_io.put(z_crc 'img &  UnZ_Glob.crc32val'img);
        --    raise
        --      error_in_image_data with
        --      "PNG: deflate stream corrupt";
        --  end if;
        --  ** Mystery: this check fails even with images which decompress perfectly
        --  ** Is CRC init value different between zip and zlib ? Is it Adler32 ?
      end Decompress_Data;

    begin
      if some_trace then
        Ada.Text_IO.Put_Line ("[begin Load_specialized]");
      end if;
      --
      --  For optimization reasons, bytes_to_unfilter is passed as a
      --  generic parameter but should be always as below right to "/=" :
      --
      if bytes_to_unfilter /= Integer'max (1, png_bits_per_pixel / 8) then
        raise Program_Error;
      end if;
      if interlaced then
        x_max := (frame_width  + 7) / 8 - 1;
        y_max := (frame_height + 7) / 8 - 1;
      else
        x_max := X_range'last;
        y_max := Y_range'last;
      end if;
      Decompress_Data;
      if some_trace then
        for f in Filter_method_0 loop
          Ada.Text_IO.Put_Line
            ("Filters used for this frame: " & f'image & filter_stat (f)'image);
        end loop;
      end if;
      Feedback (100);
    end Load_specialized;

    --  Instances of Load_specialized, with hard-coded parameters.
    --  They may take an insane amount of time to compile, and bloat the
    --  .o code , but are significantly faster since they make the
    --  compiler skip corresponding tests at pixel level.
    --  These instances are for most current PNG sub-formats.

    procedure Load_interlaced_1pal is new Load_specialized (True,  1, 1, 3);
    procedure Load_interlaced_2pal is new Load_specialized (True,  2, 1, 3);
    procedure Load_interlaced_4pal is new Load_specialized (True,  4, 1, 3);
    procedure Load_interlaced_8pal is new Load_specialized (True,  8, 1, 3);
    procedure Load_interlaced_24   is new Load_specialized (True, 24, 3, 2);
    procedure Load_interlaced_32   is new Load_specialized (True, 32, 4, 6);
    --
    procedure Load_straight_1pal is new Load_specialized (False,  1, 1, 3);
    procedure Load_straight_2pal is new Load_specialized (False,  2, 1, 3);
    procedure Load_straight_4pal is new Load_specialized (False,  4, 1, 3);
    procedure Load_straight_8pal is new Load_specialized (False,  8, 1, 3);
    procedure Load_straight_24   is new Load_specialized (False, 24, 3, 2);
    procedure Load_straight_32   is new Load_specialized (False, 32, 4, 6);
    --
    --  For unusual sub-formats, we prefer to fall back to the
    --  slightly slower, general version, where parameters values
    --  are not known at compile-time:
    --
    procedure Load_Frame_Dynamic_Parameters is new
      Load_specialized
        (interlaced         => image.progressive,
         png_bits_per_pixel => image.bits_per_pixel,
         bytes_to_unfilter  => Integer'max (1, image.bits_per_pixel / 8),
         subformat_id       => image.subformat_id);

    procedure Load_Frame is
    begin
      if some_trace then
        Ada.Text_IO.Put_Line ("[begin Load_Frame]");
      end if;
      --
      --  All these case tests are better done at the picture
      --  level than at the pixel level.
      --
      case image.subformat_id is
        when 2 =>  --  RGB
          case image.bits_per_pixel is
            when 24 =>
              if image.progressive then
                Load_interlaced_24;
              else
                Load_straight_24;
              end if;
            when others =>
              Load_Frame_Dynamic_Parameters;
          end case;
        when 3 =>  --  Palette
          case image.bits_per_pixel is
            when 1 =>
              if image.progressive then
                Load_interlaced_1pal;
              else
                Load_straight_1pal;
              end if;
            when 2 =>
              if image.progressive then
                Load_interlaced_2pal;
              else
                Load_straight_2pal;
              end if;
            when 4 =>
              if image.progressive then
                Load_interlaced_4pal;
              else
                Load_straight_4pal;
              end if;
            when 8 =>
              if image.progressive then
                Load_interlaced_8pal;
              else
                Load_straight_8pal;
              end if;
            when others =>
              Load_Frame_Dynamic_Parameters;
          end case;
        when 6 =>  --  RGBA
          case image.bits_per_pixel is
            when 32 =>
              if image.progressive then
                Load_interlaced_32;
              else
                Load_straight_32;
              end if;
            when others =>
              Load_Frame_Dynamic_Parameters;
          end case;
        when others =>
          Load_Frame_Dynamic_Parameters;
      end case;
    end Load_Frame;

    procedure Textual_Data is
      b : U8;
    begin
      for Unused in 1 .. ch.length loop
        Buffering.Get_Byte (image.buffer, b);
        if some_trace then
          if b = 0 then  --  Separates keywords in message
            Ada.Text_IO.New_Line;
          else
            Ada.Text_IO.Put (Character'val (b));
          end if;
        end if;
      end loop;
    end Textual_Data;

    procedure Frame_Control_Chunk is
      sequence_number : U32;  --  Sequence number of the animation chunk, starting from 0
      width           : U32;  --  Width of the following frame
      height          : U32;  --  Height of the following frame
      x_offset        : U32;  --  X position at which to render the following frame
      y_offset        : U32;  --  Y position at which to render the following frame
      delay_num       : U16;  --  Frame delay fraction numerator
      delay_den       : U16;  --  Frame delay fraction denominator
      dispose_op_byte : U8;   --  Type of frame area disposal to be done after rendering this frame
      blend_op_byte   : U8;   --  Type of frame area rendering for this frame
      dispose_op      : PNG_Defs.Dispose_Op_Type;
      blend_op        : PNG_Defs.Blend_Op_Type;
      --
      frame_delay : Ada.Calendar.Day_Duration;
    begin
      Big_Endian (image.buffer, sequence_number);
      Big_Endian (image.buffer, width);
      Big_Endian (image.buffer, height);
      Big_Endian (image.buffer, x_offset);
      Big_Endian (image.buffer, y_offset);
      Big_Endian (image.buffer, delay_num);
      Big_Endian (image.buffer, delay_den);
      Buffering.Get_Byte (image.buffer, dispose_op_byte);
      Buffering.Get_Byte (image.buffer, blend_op_byte);
      --
      frame_delay :=
        Ada.Calendar.Day_Duration (delay_num) /
        Ada.Calendar.Day_Duration (delay_den);
      image.next_frame := image.next_frame + frame_delay;
      next_frame := image.next_frame;
      begin
        dispose_op := PNG_Defs.Dispose_Op_Type'val (dispose_op_byte);
      exception
        when Constraint_Error =>
          raise error_in_image_data with
            "APNG Dispose_Op_Type value invalid: " & dispose_op_byte'image;
      end;
      begin
        blend_op := PNG_Defs.Blend_Op_Type'val (blend_op_byte);
      exception
        when Constraint_Error =>
          raise error_in_image_data with
            "APNG Blend_Op_Type value invalid: " & blend_op_byte'image;
      end;
      image.PNG_stuff :=
        (frame_width  => Positive_32 (width),
         frame_height => Positive_32 (height),
         x_offset     => Natural_32 (x_offset),
         y_offset     => Natural_32 (y_offset),
         dispose_op   => dispose_op,
         blend_op     => blend_op);
      --
      if some_trace then
        Ada.Text_IO.Put_Line ("  Frame Control");
        Ada.Text_IO.Put_Line ("    Dimensions (pixels):" & width'image & " x" & height'image);
        Ada.Text_IO.Put_Line ("    Offset             :" & x_offset'image & ',' & y_offset'image);
        Ada.Text_IO.Put_Line ("    Delay              :" & frame_delay'image);
        Ada.Text_IO.Put_Line ("    Dispose operation  : " & dispose_op'image);
        Ada.Text_IO.Put_Line ("    Blend operation    : " & blend_op'image);
      end if;
    end Frame_Control_Chunk;

    procedure Frame_Data_Chunk is
      sequence_number : U32;
    begin
      Big_Endian (image.buffer, sequence_number);
      Load_Frame;
    end Frame_Data_Chunk;

    pause_chunk_processing : Boolean := False;
    dummy : U32;
    b : U8;

  begin
    if some_trace then
      Ada.Text_IO.Put_Line ("[begin Load]");
    end if;
    next_frame := 0.0;

    main_chunk_loop :
    loop
      loop
        Read_Chunk_Header (image, ch);
        exit when ch.kind = IEND or ch.length > 0;
      end loop;
      case ch.kind is
        when IEND =>  --  11.2.4 IEND Image trailer
          image.next_frame := 0.0;
          exit main_chunk_loop;
        when IDAT =>  --  11.2.3 IDAT Image data
          --
          --  NB: the compressed data may hold on several IDAT chunks.
          --      It means that right in the middle of compressed data, you
          --      can have a chunk crc, and a new IDAT header!...
          begin
            Load_Frame;
          exception
            when error_in_image_data =>
              --  Vicious IEND at the wrong place
              --  basi4a08.png test image (corrupt, imho)
              null;
          end;
          pause_chunk_processing := True;  --  Come back on next frame...
        when tEXt =>
          Textual_Data;
        when fcTL =>
          --  APNG - Frame control chunk
          Frame_Control_Chunk;
        when fdAT =>
          --  APNG - Frame data chunk
          Frame_Data_Chunk;
          pause_chunk_processing := True;  --  Come back on next frame...
        when others =>
          --  Skip acTL or unknown chunk's data
          for Unused in 1 .. ch.length loop
            Buffering.Get_Byte (image.buffer, b);
          end loop;
      end case;
      Big_Endian (image.buffer, dummy);  --  Skip chunk's CRC
      exit main_chunk_loop when pause_chunk_processing;
    end loop main_chunk_loop;

    if some_trace then
      Ada.Text_IO.Put_Line ("[end Load]; next_frame =" & next_frame'image);
    end if;
  end Load;

end GID.Decoding_PNG;
