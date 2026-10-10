-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

pragma Build (Description => "Processor test",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");

with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;
with File;
with Exceptions;
with Time;
with Url_Names;

procedure Processor_Test is

  package IO  renames Ada.Text_IO;
  package SIO renames Ada.Streams.Stream_IO;

  function Read_File (Filename : String) return Ada.Streams.Stream_Element_Array is
    The_File : SIO.File_Type;
  begin
    SIO.Open (The_File, SIO.In_File, Filename);
    declare
      Size : constant Ada.Streams.Stream_Element_Offset := Ada.Streams.Stream_Element_Offset (SIO.Size (The_File));
      Data : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last : Ada.Streams.Stream_Element_Offset;
    begin
      SIO.Read (The_File, Data, Last);
      SIO.Close (The_File);
      return Data;
    end;
  end Read_File;

  Result : AWS.Response.Data;

  Ra  : constant String := Url_Names.Ra;
  Dec : constant String := Url_Names.Dec;
  P   : AWS.Parameters.List;

begin
  IO.Put_Line ("Processor Test");
  P.Add (Ra, "16.6");
  P.Add (Dec, "36.5");
  IO.Put_Line ("URL -> http://localhost:12000" & Url_Names.Light_Parameters & P.URI_Format);
  Result := AWS.Client.Get ("http://localhost:12000" & Url_Names.Light_Parameters & P.URI_Format);
  declare
    Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
  begin
    if not (Status in  AWS.Messages.Success) then
      IO.Put_Line ("Status:" & AWS.Response.Status_Code (Result)'image);
      IO.Put_Line ("Response:" & AWS.Response.Message_Body (Result));
      return;
    end if;
  end;
  for The_Filename of File.Iterator_For ("D:\Pictures\M13") loop
    IO.Put_Line ("File: " & The_Filename);
    declare
      Data : constant Ada.Streams.Stream_Element_Array := Read_File (The_Filename);
    begin
      IO.Put_Line ("RAW size:" & Data'length'image);
      Result := AWS.Client.Post (URL          => "http://localhost:12000/picture",
                                 Data         => Data,
                                 Content_Type => "application/octet-stream");
      declare
        Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
      begin
        if not (Status in  AWS.Messages.Success) then
          IO.Put_Line ("Status:" & AWS.Response.Status_Code (Result)'image);
          IO.Put_Line ("Response:" & AWS.Response.Message_Body (Result));
          exit;
        end if;
      end;
    end;
    Time.Wait (10.0);
  end loop;
exception
when Item : others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Processor_Test;
