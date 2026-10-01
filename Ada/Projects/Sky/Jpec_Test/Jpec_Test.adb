-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

pragma Build (Description => "Jpec test",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");

with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Streams;
with AWS.Client;
with AWS.Response;
with Exceptions;

procedure Jpec_Test is

  package IO  renames Ada.Text_IO;
  package SIO renames Ada.Streams.Stream_IO;

  function Read_File (Filename : String) return Ada.Streams.Stream_Element_Array is
    File : SIO.File_Type;
  begin
    SIO.Open (File, SIO.In_File, Filename);
    declare
      Size : constant Ada.Streams.Stream_Element_Offset := Ada.Streams.Stream_Element_Offset (SIO.Size (File));
      Data : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last : Ada.Streams.Stream_Element_Offset;
    begin
      SIO.Read (File, Data, Last);
      SIO.Close (File);
      return Data;
    end;
  end Read_File;

  Result : AWS.Response.Data;

begin
  IO.Put_Line ("Jpec Test");
  declare
    Data : constant Ada.Streams.Stream_Element_Array := Read_File ("test.jpg");
  begin
    IO.Put_Line ("JPEG size:" & Data'length'image);
    Result := AWS.Client.Post (URL          => "http://192.168.56.1:8000/image",
                               Data         => Data,
                               Content_Type => "image/jpeg");
    IO.Put_Line ("Status:" & AWS.Response.Status_Code (Result)'image);
    IO.Put_Line ("Response:" & AWS.Response.Message_Body (Result));
  end;
exception
when Item : others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Jpec_Test;
