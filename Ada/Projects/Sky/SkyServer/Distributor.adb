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

with Ada.Streams.Stream_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Traces;
with Url_Names;

package body Distributor is

  package Log is new Traces ("Distributor");

  package SIO renames Ada.Streams.Stream_IO;

  subtype Data_Array  is Ada.Streams.Stream_Element_Array;
  subtype Data_Offset is Ada.Streams.Stream_Element_Offset;

  use type Data_Offset;


  procedure Check (Result : AWS.Response.Data) is
    Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
  begin
    if not (Status in  AWS.Messages.Success) then
      Log.Error ("Status:" & AWS.Response.Status_Code (Result)'image);
    end if;
  end Check;


  procedure Send_Jpeg (Filename : String) is

    function Read_File return Data_Array is
      The_File : SIO.File_Type;
    begin
      SIO.Open (The_File, SIO.In_File, Filename);
      declare
        Size : constant Ada.Streams.Stream_Element_Offset := Data_Offset(SIO.Size (The_File));
        Data : Data_Array(1 .. Size);
        Last : Data_Offset;
      begin
        SIO.Read (The_File, Data, Last);
        SIO.Close (The_File);
        if Last /= Data'last then
          raise Program_Error with "Incomplete JPEG file read";
        end if;
        return Data;
      end;
    end Read_File;

  begin -- Send_Jpeg
    Check (AWS.Client.Post (URL          => "http://192.168.178.20:8000/image",
                            Data         => Read_File,
                            Content_Type => "image/jpeg"));
  end Send_Jpeg;


  procedure Send_Fits (Filename : String) is

    function Read_File return Data_Array is
      The_File : SIO.File_Type;
    begin
      SIO.Open (The_File, SIO.In_File, Filename);
      declare
        Size : constant Data_Offset := Data_Offset (SIO.Size (The_File));
        Data : Data_Array(1 .. Size);
        Last : Data_Offset;
      begin
        SIO.Read (The_File, Data, Last);
        SIO.Close (The_File);
        if Last /= Data'last then
          raise Program_Error with "Incomplete FITS file read";
        end if;
        return Data;
      end;
    end Read_File;

  begin -- Send_Fits
    Check (AWS.Client.Post (URL          => "http://Localhost:11001" & Url_Names.Picture,
                            Data         => Read_File,
                            Content_Type => "application/octet-stream"));
  end Send_Fits;

end Distributor;
