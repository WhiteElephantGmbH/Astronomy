-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Interfaces.C;
with System;

package Raw_Interface is

  pragma Linker_Options ("-lraw");

  package C renames Interfaces.C;

  -- Opaque pointer to libraw_data_t (C type).
  type Context is private;

  Null_Context : constant Context;

  ----------------------------
  -- Basic LibRaw lifecycle --
  ----------------------------

  -- libraw_data_t *libraw_init(unsigned int flags);
  function Init (Flags : C.unsigned := 0) return Context
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_init";

  -- int libraw_open_file(libraw_data_t *, const char *);
  function Open_File (Ctx  : Context;
                      Name : System.Address) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_open_file";

  -- int libraw_unpack(libraw_data_t *);
  function Unpack (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_unpack";

  -- int libraw_raw2image(libraw_data_t *); (not used, but kept for completeness.)
  function Raw2_Image (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_raw2image";

  -- void libraw_free_image(libraw_data_t *);
  procedure Free_Image (Ctx : Context)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_free_image";

  -- void libraw_recycle(libraw_data_t *);
  procedure Recycle (Ctx : Context)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_recycle";

  -- void libraw_close(libraw_data_t *);
  procedure Close (Ctx : Context)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_close";

  ----------------------------------
  -- Image dimensions (processed) --
  ----------------------------------

  -- int libraw_get_iwidth(libraw_data_t *lr);
  function Get_Iwidth (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_get_iwidth";

  -- int libraw_get_iheight(libraw_data_t *lr);
  function Get_Iheight (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_get_iheight";

  -------------------
  -- Error strings --
  -------------------

  -- const char *libraw_strerror(int errorcode);
  function Str_Error (Code : C.int) return System.Address
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_strerror";

  ----------------------------------------------
  -- RAW mosaic access (dimensions, if needed) --
  ----------------------------------------------

  -- int libraw_get_raw_width(libraw_data_t *lr);
  function Get_Raw_Width (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_get_raw_width";

  -- int libraw_get_raw_height(libraw_data_t *lr);
  function Get_Raw_Height (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_get_raw_height";

  -----------------------------------------
  -- Processed image via dcraw emulation --
  -----------------------------------------

  -- int libraw_dcraw_process(libraw_data_t *lr);
  function Dcraw_Process (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_dcraw_process";

  -- C struct (from libraw_types.h):
  --
  -- typedef struct
  -- {
  --   enum LibRaw_image_formats type;
  --   ushort height, width, colors, bits;
  --   unsigned int data_size;
  --   unsigned char data[1];
  -- } libraw_processed_image_t;
  --
  -- We model 'data' as a single byte; we use its 'Address as the start of the larger buffer allocated by LibRaw.

  type Processed_Image is record
    Img_Type  : C.int;
    Height    : C.unsigned_short;
    Width     : C.unsigned_short;
    Colors    : C.unsigned_short;
    Bits      : C.unsigned_short;
    Data_Size : C.unsigned; -- unsigned int in C
    Data      : aliased C.unsigned_char;
  end record
    with Convention => C;

  type Processed_Image_Ptr is access all Processed_Image
    with Convention => C;

  -- libraw_processed_image_t * libraw_dcraw_make_mem_image(libraw_data_t *lr, int *errc);
  function Dcraw_Make_Mem_Image (Ctx        : Context;
                                 Error_Code : access C.int) return Processed_Image_Ptr
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_dcraw_make_mem_image";

  -- void libraw_dcraw_clear_mem(libraw_processed_image_t *);
  procedure Dcraw_Clear_Mem (Img : Processed_Image_Ptr)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_dcraw_clear_mem";

  --------------------------------
  -- Output / development knobs --
  --------------------------------

  -- void libraw_set_output_bps(libraw_data_t *lr, int value);
  procedure Set_Output_Bps (Ctx   : Context;
                            Value : C.int)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_set_output_bps";

  -- void libraw_set_no_auto_bright(libraw_data_t *lr, int value);
  procedure Set_No_Auto_Bright (Ctx   : Context;
                                Value : C.int)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_set_no_auto_bright";

  -- void libraw_set_gamma(libraw_data_t *lr, int index, float value);
  procedure Set_Gamma (Ctx   : Context;
                       Index : C.int;
                       Value : C.C_float)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_set_gamma";

private

  -- Represent Context as a raw C pointer (void* / libraw_data_t*).
  type Context is new System.Address
    with Convention => C;

  Null_Context : constant Context := Context (System.Null_Address);

end Raw_Interface;
