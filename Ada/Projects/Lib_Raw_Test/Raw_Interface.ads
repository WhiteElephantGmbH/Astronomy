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

  type Context is private;
  Null_Context : constant Context;

  function Init (Flags : C.unsigned := 0) return Context
    with Import, Convention => C, External_Name => "libraw_init";

  function Open_File (Ctx : Context; Name : System.Address) return C.int
    with Import, Convention => C, External_Name => "libraw_open_file";

  function Unpack (Ctx : Context) return C.int
    with Import, Convention => C, External_Name => "libraw_unpack";

  function Raw2_Image (Ctx : Context) return C.int
    with Import, Convention => C, External_Name => "libraw_raw2image";

  procedure Free_Image (Ctx : Context)
    with Import, Convention => C, External_Name => "libraw_free_image";

  procedure Recycle (Ctx : Context)
    with Import, Convention => C, External_Name => "libraw_recycle";

  procedure Close (Ctx : Context)
    with Import, Convention => C, External_Name => "libraw_close";

  function Get_Raw_Width (Ctx : Context) return C.int
    with Import, Convention => C, External_Name => "libraw_get_raw_width";

  function Get_Raw_Height (Ctx : Context) return C.int
    with Import, Convention => C, External_Name => "libraw_get_raw_height";

  function Get_Iwidth (Ctx : Context) return C.int
    with Import, Convention => C, External_Name => "libraw_get_iwidth";

  function Get_Iheight (Ctx : Context) return C.int
    with Import, Convention => C, External_Name => "libraw_get_iheight";

  function COLOR (Ctx : Context; Row, Col : C.int) return C.int
    with Import, Convention => C, External_Name => "libraw_COLOR";

private
  type Context is new System.Address with Convention => C;
  Null_Context : constant Context := Context (System.Null_Address);
end Raw_Interface;
