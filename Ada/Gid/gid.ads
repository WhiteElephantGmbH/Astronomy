---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
--  Purpose:
--
--     The Generic Image Decoder is a low-level package for decoding
--     a broad variety of image formats, from any data stream, to any
--     kind of medium, be it an in-memory bitmap, a GUI object,
--     some other stream, arrays of floating-point initial data
--     for scientific calculations, a browser element, a device,...
--     Animations are supported.
--
--     The code is unconditionally portable, independent of the
--     choice of operating system, processor, endianess and compiler.
--
--  Image types currently supported:
--
--     BMP, GIF, JPEG, PNG, PNM, QOI, TGA
--
--  Credits:
--
--     - Andre van Splunter: GIF's LZW decoder in Ada
--     - Martin J. Fiedler: most of baseline JPEG (from NanoJPEG)
--     - Tiago Becerra Paolini: most of progressive JPEG (from PyJpegDecoder)
--
--     More credits in gid_work.xls, sheet "credits".
--
--  Copyright (c) Gautier de Montmollin 2010 .. 2024
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 2-May-2010 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Ada.Calendar, Ada.Streams, Ada.Strings.Bounded, Ada.Finalization;
with Interfaces;
with System;

package GID is

  type Image_Descriptor is private;

  ---------------------------------------------------
  -- 1) Load the image header from the data stream --
  ---------------------------------------------------

  procedure Load_Image_Header
    (image   :    out Image_Descriptor;
     from    : in out Ada.Streams.Root_Stream_Type'class;
     try_tga :        Boolean := False);

  --  try_tga: if no known signature is found, assume it might be
  --  the TGA format (which hasn't a signature) and try to load an
  --  image of this format

  unknown_image_format,
  known_but_unsupported_image_format,
  unsupported_image_subformat,
  error_in_image_data,
  invalid_primary_color_range : exception;

  ----------------------------------------------------------------------
  -- 2) If needed, use dimensions to prepare the retrieval of the     --
  --    image, for instance: reserving an in-memory bitmap, sizing a  --
  --    GUI object, defining a browser element, setting up a device   --
  ----------------------------------------------------------------------

  function Pixel_Width (image : Image_Descriptor) return Positive;
  function Pixel_Height (image : Image_Descriptor) return Positive;

  --  "Unchanged" orientation has origin at top left

  type Orientation is
    (Unchanged,
     Rotation_90, Rotation_180, Rotation_270);

  function Display_Orientation (image : Image_Descriptor) return Orientation;

  --------------------------------------------------------------------
  -- 3) Load and decode the image itself. If the image is animated, --
  --    call Load_image_contents until next_frame is 0.0            --
  --------------------------------------------------------------------

  type Display_Mode is (fast, redundant);
  --  For both display modes the final result is exactly the same.
  --
  --  However, for progressive or interlaced image formats, inaccurate
  --  versions of the image will be displayed during decoding as
  --  foreseen by those formats. The inaccurate display is covering
  --  completely the image's frame.
  --  The advantage of the `redundant` mode is that if the stream is
  --  slow or the image is huge, the person contemplating the image
  --  can see earlier most characteristics of the image. The downside
  --  is that the intermediary display makes the overall decoding slower
  --  and part of the pixels at the same position will be displayed
  --  multiple times.
  --
  --  So, for prioritizing speed, choose the `fast` mode.

  generic
    type Primary_Color_Range is mod <>;
    --  Coding of primary colors (red, green or blue)
    --     and of opacity (also known as alpha channel), on the target "device".
    --  Currently, only 8-bit and 16-bit are admitted.
    --      8-bit coding is usual: TrueColor, PC graphics, etc.;
    --     16-bit coding is seen in some high-end apps/devices/formats.
    --
    with procedure Set_X_Y (x, y : Natural);
    --  After Set_X_Y, next pixel is meant to be displayed at position (x,y)
    --  TBD: switch to Natural_32 for ensuring dimensions larger than 32767.
    with procedure Put_Pixel
      (red, green, blue : Primary_Color_Range;
       alpha            : Primary_Color_Range);
    --  When Put_Pixel is called twice without a Set_X_Y inbetween,
    --  the pixel must be displayed on the next X position after the last one.
    --  [ Rationale: if the image lands into an array with contiguous pixels
    --    on the X axis, this approach allows full address calculation to be
    --    made only at the beginning of each row, which is much faster ]
    --
    with procedure Feedback (percents : Natural);
    --
    mode : Display_Mode;
    --
  procedure Load_Image_Contents
    (image      : in out Image_Descriptor;
     next_frame :    out Ada.Calendar.Day_Duration);
     --  ^ Animation: real time lapse foreseen between the first image
     --    and the image right after this one.
     --
     --    The return value is 0.0 if there is no next frame;
     --    this includes the case of simgle-image data or formats.

  ---------------------------------------------------------------------
  --  Some informations about the image, not necessary for decoding  --
  ---------------------------------------------------------------------

  type Image_Format_Type is
    (BMP, FITS, GIF, JPEG, PNG, PNM, QOI, RIFF, TGA, TIFF);
  --  NB: RIFF is a container for the WebP format.

  function Format (image : Image_Descriptor) return Image_Format_Type;
  function Detailed_Format (image : Image_Descriptor) return String;
  --  example: "GIF89a, interlaced"
  function Subformat (image : Image_Descriptor) return Integer;
  --  example the 'color type' in PNG

  function Bits_per_Pixel (image : Image_Descriptor) return Positive;
  function Is_RLE_Encoded (image : Image_Descriptor) return Boolean;
  function Is_Progressive (image : Image_Descriptor) return Boolean;
  function Is_Interlaced (image : Image_Descriptor) return Boolean
    renames Is_Progressive;
  function Greyscale (image : Image_Descriptor) return Boolean;
  function Has_Palette (image : Image_Descriptor) return Boolean;
  function Expect_Transparency (image : Image_Descriptor) return Boolean;

  ---------------------------------------------------------------------
  --  Information about frame-to-frame handling in animations.       --
  --  Currently, it's APNG-only, so we use directly the APNG types.  --
  ---------------------------------------------------------------------

  package PNG_Defs is

    type Dispose_Op_Type is
      (APNG_DISPOSE_OP_NONE,        --  No disposal done on this frame before rendering the next.
       APNG_DISPOSE_OP_BACKGROUND,  --  To be cleared to fully transparent black.
       APNG_DISPOSE_OP_PREVIOUS);   --  To be reverted to the previous contents.

    type Blend_Op_Type is
      (APNG_BLEND_OP_SOURCE,  --  Overwrite the current contents of the frame's output buffer.
       APNG_BLEND_OP_OVER);   --  Composited onto the output buffer based on its alpha.

  end PNG_Defs;

  procedure Get_Next_Frame_Informations
    (image             : in     Image_Descriptor;
     dispose_operation :    out PNG_Defs.Dispose_Op_Type;
     blend_operation   :    out PNG_Defs.Blend_Op_Type);

  ----------------------------------------------------------------
  --  Information about this package - e.g. for an "about" box  --
  ----------------------------------------------------------------

  version   : constant String := "013";
  reference : constant String := "02-Mar-2024";
  web       : constant String := "http://gen-img-dec.sf.net/";
  --  Hopefully the latest version is at that URL..........^
  --
  --  There is a mirror too @ https://github.com/zertovitch/gid

private

  subtype U8  is Interfaces.Unsigned_8;
  subtype U16 is Interfaces.Unsigned_16;
  subtype U32 is Interfaces.Unsigned_32;

  package Bounded_255 is
    new Ada.Strings.Bounded.Generic_Bounded_Length (255);

  type Stream_Access is access all Ada.Streams.Root_Stream_Type'class;

  type RGB_Color_8_Bit is record
    red, green, blue : U8;
  end record;

  type Color_Table is array (Integer range <>) of RGB_Color_8_Bit;

  type p_Color_Table is access Color_Table;

  min_bits : constant := Integer'max (32, System.Word_Size);
  --  13.3(8): A word is the largest amount of storage that can be
  --  conveniently and efficiently manipulated by the hardware,
  --  given the implementation's run-time model.

  type Integer_M32 is range -2**(min_bits - 1) .. 2**(min_bits - 1) - 1;
  --  We define an Integer type which is at least 32 bits, but n bits
  --  on a native n > 32 bits architecture (no performance hit on 64+
  --  bits architectures).

  subtype Natural_M32 is Integer_M32 range 0 .. Integer_M32'last;
  subtype Positive_M32 is Integer_M32 range 1 .. Integer_M32'last;

  type Byte_Array is array (Integer range <>) of U8;

  type Input_Buffer is record
    data        : Byte_Array (1 .. 1024);
    stream      : Stream_Access := null;
    InBufIdx    : Positive := 1;  --  Points to next char in buffer to be read
    MaxInBufIdx : Natural := 0;   --  Count of valid chars in input buffer
    InputEoF    : Boolean;        --  End of file indicator
  end record;
  --  Initial values ensure call to Fill_Buffer on first Get_Byte

  subtype Natural_32  is Interfaces.Integer_32
    range 0 .. Interfaces.Integer_32'last;

  subtype Positive_32 is Interfaces.Integer_32
    range 1 .. Interfaces.Integer_32'last;

  -------------------------------------------------------------------------
  --  JPEG-Specific stuff.                                               --
  --  NB: JPEG may store data _before_ any image header (SOF), then we   --
  --  have to make the image descriptor store that information, alas...  --
  -------------------------------------------------------------------------

  package JPEG_Defs is

    type Component is
      (Y,   --  brightness
       Cb,  --  hue
       Cr,  --  saturation
       I,   --  ??
       Q);  --  ??

    type Quantization_Table is array (0 .. 63) of Natural;
    type Quantization_Table_List is array (0 .. 7) of Quantization_Table;

    type Compo_Set_Type is array (Component) of Boolean;

    type Upsampling_Profile_Type is
      (component_not_covered,
       --  Value pairs: (#samples, upsampling factor)
       hor_1_1_ver_1_1,
       hor_1_2_ver_1_2,
       hor_2_1_ver_2_1,
       other_profile);

    type Upsampling_Parameters is record
      samples_hor : Natural := 0;
      up_factor_x : Natural;  --  how much we must repeat horizontally
      samples_ver : Natural := 0;
      up_factor_y : Natural;  --  how much we must repeat vertically
    end record;

    function Identify
      (param : Upsampling_Parameters) return Upsampling_Profile_Type;

    type Info_per_Component_A is record  --  B is defined inside the decoder
      qt_assoc           : Natural;
      repeat             : Natural;
      shape_x            : Natural;  --  x dimension (in pixels) of the MCU
      shape_y            : Natural;  --  y dimension (in pixels) of the MCU
      ups                : Upsampling_Parameters;
      upsampling_profile : Upsampling_Profile_Type := component_not_covered;
    end record;

    type Component_Info_A is array (Component) of Info_per_Component_A;

    type Supported_Color_Space is
      (YCbCr,   --  3-dim color space
       Y_Grey,  --  1-dim greyscale
       CMYK);   --  4-dim Cyan, Magenta, Yellow, blacK

    type AC_DC is (AC, DC);

    type VLC_Code is record
      bits, code : U8;
    end record;

    type VLC_Table is array (0 .. 65_535) of VLC_Code;

    type p_VLC_Table is access VLC_Table;

    type VLC_Defs_Type is array (AC_DC, 0 .. 7) of p_VLC_Table;

  end JPEG_Defs;

  type JPEG_Stuff_Type is record
    compo_set        : JPEG_Defs.Compo_Set_Type := [others => False];
    color_space      : JPEG_Defs.Supported_Color_Space;
    info             : JPEG_Defs.Component_Info_A;
    max_samples_hor  : Natural;
    max_samples_ver  : Natural;
    qt_list          : JPEG_Defs.Quantization_Table_List;
    vlc_defs         : JPEG_Defs.VLC_Defs_Type := [others => [others => null]];
    restart_interval : Natural;  --  Predictor restarts every... (0: never)
  end record;

  type PNG_Stuff_Type is record
    --  APNG stuff:
    frame_width  : Positive_32;
    frame_height : Positive_32;
    x_offset     : Natural_32;
    y_offset     : Natural_32;
    dispose_op   : PNG_Defs.Dispose_Op_Type;
    blend_op     : PNG_Defs.Blend_Op_Type;
  end record;

  type Endianess_Type is (little, big);  --  For TIFF images

  type Image_Descriptor is new Ada.Finalization.Controlled with record
    format              : Image_Format_Type;
    detailed_format     : Bounded_255.Bounded_String;  --  For humans only!
    subformat_id        : Integer := 0;
    width, height       : Positive_32;
    display_orientation : Orientation;
    top_first           : Boolean;  --  Data orientation in TGA
    bits_per_pixel      : Positive;
    RLE_encoded         : Boolean := False;
    transparency        : Boolean := False;
    greyscale           : Boolean := False;
    progressive         : Boolean := False;  --  JPEG, PNG; for GIF: per frame
    endianess           : Endianess_Type;    --  TIFF
    JPEG_stuff          : JPEG_Stuff_Type;
    stream              : Stream_Access;
    buffer              : Input_Buffer;
    palette             : p_Color_Table := null;
    first_byte          : U8;
    next_frame          : Ada.Calendar.Day_Duration;
    PNG_stuff           : PNG_Stuff_Type;
  end record;

  overriding procedure Adjust (Object : in out Image_Descriptor);
  overriding procedure Finalize (Object : in out Image_Descriptor);

  to_be_done : exception;
  --  ^ This exception should not happen, even with malformed files.
  --  Its role is to pop up when a feature is set as implemented
  --  but one aspect (e.g. palette) was forgotten.

  --
  --  Primitive tracing using Ada.Text_IO, for debugging,
  --  or explaining internals.
  --
  subtype Trace_Type is Integer range 0 .. 9;
  --  0  :  No trace at all, no use of console from the library
  --  1  :  Image / frame technical informations
  --  2  :  Byte / pixel / compressed block details
  --  9  :  Dump files

  trace : constant Trace_Type := 0;  --  <=====  The choice is here

  no_trace   : constant Boolean := trace  = 0;
  some_trace : constant Boolean := trace >= 1;
  more_trace : constant Boolean := trace >= 2;
  full_trace : constant Boolean := trace >= 9;

end GID;
