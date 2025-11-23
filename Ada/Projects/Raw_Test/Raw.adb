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

with Ada.Characters.Latin_1;

package body Raw is

  -----------------
  -- Open_File   --
  -----------------

  procedure Open_File
    (Ctx       : out Context;
     File_Name : String)
  is
    use type RI.Context;
    use type C.int;

    Local : Context;

    --  NUL-terminated file name for C
    C_Name : aliased String := File_Name & Ada.Characters.Latin_1.NUL;

    Err : C.int;
  begin
    --  Initialize LibRaw context
    Local.Handle := RI.Init (0);

    if Local.Handle = RI.Null_Context then
      raise Raw_Error;
    end if;

    --  Open file
    Err := RI.Open_File (Local.Handle, C_Name'address);
    if Err /= 0 then
      RI.Close (Local.Handle);
      raise Raw_Error;
    end if;

    --  Unpack RAW data
    Err := RI.Unpack (Local.Handle);
    if Err /= 0 then
      RI.Recycle (Local.Handle);
      RI.Close (Local.Handle);
      raise Raw_Error;
    end if;

    --  Convert RAW into LibRaw's internal image buffer
    Err := RI.Raw2_Image (Local.Handle);
    if Err /= 0 then
      RI.Recycle (Local.Handle);
      RI.Close (Local.Handle);
      raise Raw_Error;
    end if;

    Local.Opened := True;
    Ctx          := Local;
  end Open_File;

  -------------------
  -- Image_Width   --
  -------------------

  function Image_Width (Ctx : Context) return Natural is
    use type RI.Context;
  begin
    if (not Ctx.Opened) or else Ctx.Handle = RI.Null_Context then
      return 0;
    else
      return Natural (RI.Get_Iwidth (Ctx.Handle));
    end if;
  end Image_Width;

  --------------------
  -- Image_Height   --
  --------------------

  function Image_Height (Ctx : Context) return Natural is
    use type RI.Context;
  begin
    if (not Ctx.Opened) or else Ctx.Handle = RI.Null_Context then
      return 0;
    else
      return Natural (RI.Get_Iheight (Ctx.Handle));
    end if;
  end Image_Height;

  -------------
  -- Close   --
  -------------

  procedure Close (Ctx : in out Context) is
    use type RI.Context;
  begin
    if Ctx.Handle /= RI.Null_Context then
      --  Free image buffers (if any)
      RI.Free_Image (Ctx.Handle);
      --  Recycle LibRaw internal data
      RI.Recycle (Ctx.Handle);
      --  Destroy context
      RI.Close (Ctx.Handle);

      Ctx.Handle := RI.Null_Context;
      Ctx.Opened := False;
    end if;
  end Close;

end Raw;
