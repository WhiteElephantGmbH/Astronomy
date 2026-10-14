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
with AWS.Client;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;
with Text;
with Traces;
with Url_Names;

package body Feeder is

  package Log is new Traces ("Feeder");

  package SIO renames Ada.Streams.Stream_IO;

  Base_URL : constant String := "http://localhost:12000";


  procedure Set (Focal_Length : Units.Focal_Length) is
  begin
    null; --!!! not implemented
  end Set;


  procedure Set (Pixel_Size : Units.Pixel_Size) is
  begin
    null; --!!! not implemented
  end Set;


  procedure Check (Result : AWS.Response.Data) is
    Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
  begin
    if not (Status in  AWS.Messages.Success) then
      Log.Error ("Status:" & AWS.Response.Status_Code (Result)'image);
      raise Operation_Failed;
    end if;
  end Check;


  procedure New_Darks (Id    : Units.Darks_Id;
                       Count : Units.Picture_Number) is
    P : AWS.Parameters.List;
  begin
    P.Add (Url_Names.Id, Id);
    P.Add (Url_Names.Count, Text.Trimmed (Count'image));
    Check (AWS.Client.Get (Base_URL & Url_Names.Dark_Parameters & P.URI_Format));
  end New_Darks;


  procedure New_Sequence (Target   : Space.Direction;
                          Darks_Id : Units.Darks_Id) is
    P : AWS.Parameters.List;
  begin
    P.Add (Url_Names.Ra, Space.Ra_Image_Of (Target));
    P.Add (Url_Names.Dec, Space.Dec_Image_Of (Target));
    P.Add (Url_Names.Id, Darks_Id);
    Check (AWS.Client.Get (Base_URL & Url_Names.Light_Parameters & P.URI_Format));
  end New_Sequence;


  procedure Next_Picture (Filename : String) is

    function Read_File return Ada.Streams.Stream_Element_Array is
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

  begin -- Next_Picture
    Check (AWS.Client.Post (URL          => Base_URL & Url_Names.Picture,
                            Data         => Read_File,
                            Content_Type => "application/octet-stream"));
  end Next_Picture;


  procedure Stop is
  begin
    Check (AWS.Client.Get (Base_URL & Url_Names.Stop));
  end Stop;

end Feeder;
