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
  --  Or "-lraw" depending on which LibRaw .lib/.dll you link against.

  package C renames Interfaces.C;

  --  Opaque pointer to libraw_data_t (C type).
  --  In C: libraw_data_t *.
  type Context is private;

  Null_Context : constant Context;

  ----------------------------------
  -- Basic LibRaw lifecycle
  ----------------------------------

  --  In C:
  --    libraw_data_t *libraw_init(unsigned int flags);
  function Init (Flags : C.unsigned := 0) return Context
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_init";

  --  In C:
  --    int libraw_open_file(libraw_data_t *, const char *);
  function Open_File
    (Ctx  : Context;
     Name : System.Address) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_open_file";

  --  In C:
  --    int libraw_unpack(libraw_data_t *);
  function Unpack (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_unpack";

  --  In C:
  --    int libraw_raw2image(libraw_data_t *);
  function Raw2_Image (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_raw2image";

  --  In C:
  --    void libraw_free_image(libraw_data_t *);
  procedure Free_Image (Ctx : Context)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_free_image";

  --  In C:
  --    void libraw_recycle(libraw_data_t *);
  procedure Recycle (Ctx : Context)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_recycle";

  --  In C:
  --    void libraw_close(libraw_data_t *);
  procedure Close (Ctx : Context)
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_close";

  ----------------------------------
  -- Image dimensions
  ----------------------------------

  --  In C:
  --    int libraw_get_iwidth(libraw_data_t *lr);
  function Get_Iwidth (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_get_iwidth";

  --  In C:
  --    int libraw_get_iheight(libraw_data_t *lr);
  function Get_Iheight (Ctx : Context) return C.int
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_get_iheight";

  ----------------------------------
  -- Error strings
  ----------------------------------

  --  In C:
  --    const char *libraw_strerror(int errorcode);
  --
  --  We return System.Address here; higher layer converts it to String.
  function Str_Error (Code : C.int) return System.Address
    with Import        => True,
         Convention    => C,
         External_Name => "libraw_strerror";

private

  --  Represent Context as a raw C pointer (void* / libraw_data_t*).
  type Context is new System.Address
    with Convention => C;

  Null_Context : constant Context := Context (System.Null_Address);

end Raw_Interface;
