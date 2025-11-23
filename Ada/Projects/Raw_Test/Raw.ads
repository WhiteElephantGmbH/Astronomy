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

with Interfaces.C;
private with Raw_Interface;

package Raw is

  package C renames Interfaces.C;

  type Context is private;

  Raw_Error : exception;

  --  Open a RAW file, unpack it, and run raw2image.
  --
  --  Steps:
  --    - libraw_init
  --    - libraw_open_file
  --    - libraw_unpack
  --    - libraw_raw2image
  --
  --  On any LibRaw error, Raw_Error is raised and all LibRaw resources
  --  allocated in this operation are cleaned up.
  procedure Open_File
    (Ctx       : out Context;
     File_Name : String);

  --  Get image width (iwidth in LibRaw).
  --  Returns 0 if Ctx is not open/valid.
  function Image_Width (Ctx : Context) return Natural;

  --  Get image height (iheight in LibRaw).
  --  Returns 0 if Ctx is not open/valid.
  function Image_Height (Ctx : Context) return Natural;

  --  Close LibRaw context and free any associated resources.
  --  Safe to call multiple times; after this, Ctx is reset.
  procedure Close (Ctx : in out Context);

private

  package RI renames Raw_Interface;

  type Context is record
    Handle : RI.Context := RI.Null_Context;
    Opened : Boolean := False;
  end record;

end Raw;
