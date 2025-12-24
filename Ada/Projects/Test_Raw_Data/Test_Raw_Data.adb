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

pragma Build (Description => "Test Raw Data",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");


with Ada.Text_IO;
with Raw_Data;

procedure Test_Raw_Data is
  use Ada.Text_IO;
  use type Raw_Data.Byte_Array_Access;
begin
  Raw_Data.Read ("sample.CR2");

  Put_Line ("RAW width       : " & Natural'image (Raw_Data.Raw_Width));
  Put_Line ("RAW height      : " & Natural'image (Raw_Data.Raw_Height));
  Put_Line ("Bits per sample : " & Natural'image (Raw_Data.Raw_Bits));
  Put_Line ("Compression     : " & Natural'image (Raw_Data.Raw_Compression));
  Put_Line ("Buffer bytes    : " & Natural'image (Raw_Data.Raw_Buffer_Bytes));

  if Raw_Data.Raw_Buffer /= null then
    Put_Line ("First 16 bytes:");
    declare
      B : constant Raw_Data.Byte_Array_Access := Raw_Data.Raw_Buffer;
    begin
      for I in 1 .. Integer'min (16, Raw_Data.Raw_Buffer_Bytes) loop
        Put (Integer (B (I))'image & " ");
      end loop;
      New_Line;
    end;
  end if;

exception
  when Raw_Data.File_Not_Found =>
    Put_Line ("File not found.");
  when Raw_Data.Not_Found =>
    Put_Line ("RAW data not found.");
  when Raw_Data.Invalid_File =>
    Put_Line ("Invalid CR2 file.");
end Test_Raw_Data;
