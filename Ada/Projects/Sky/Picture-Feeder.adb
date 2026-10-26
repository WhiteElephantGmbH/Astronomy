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
with Exposure;
with Sensitivity;
with Sky;
with Url_Names;

package body Picture.Feeder is

  package SIO renames Ada.Streams.Stream_IO;

  Base_URL : constant String := "http://localhost:11000";


  type Sequence_Type is (Start, Darks, Lights);

  The_Sequence_Type : Sequence_Type := Start;

  procedure Check (Sequence : Sequence_Type) is
  begin
    if Sequence /= The_Sequence_Type then
      raise Program_Error;
    end if;
  end Check;


  The_Target_Type      : Sky.Object_Type;
  The_Target_Direction : Space.Direction;
  The_Exposure         : Exposure.Item;
  The_Sensitivity      : Sensitivity.Item;

  Ten_Seconds : constant Exposure.Item    := Exposure.Value("10");
  Iso_1600    : constant Sensitivity.Item := Sensitivity.Value("1600");


  procedure Define_Parameters_For (Target : Name.Id) is
    use all type Sky.Object_Type;
  begin
    The_Target_Type := Name.Type_Of (Target);
    case The_Target_Type is
    when Moon =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Star =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Double =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Stars =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Cluster =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Galaxy =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Nebula =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Quasar =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Satellite =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    when Unknown =>
      The_Exposure := Ten_Seconds;
      The_Sensitivity := Iso_1600;
    end case;
    The_Target_Direction := Name.Direction_Of (Target, Time.Universal);
  end Define_Parameters_For;


  function Darks_Filename return String is
  begin
    return "Darks_" & The_Exposure'image & '_' & The_Sensitivity'image;
  end Darks_Filename;


  procedure Check (Result : AWS.Response.Data) is
    Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
  begin
    if not (Status in  AWS.Messages.Success) then
      Log.Error ("Status:" & AWS.Response.Status_Code (Result)'image);
      raise Processor_Not_Connected;
    end if;
  end Check;


  procedure Send_Next is

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

  begin -- Send_Next
    Check (AWS.Client.Post (URL          => Base_URL & Url_Names.Picture,
                            Data         => Read_File,
                            Content_Type => "application/octet-stream"));
  end Send_Next;


  procedure Store_Darks is
    P : AWS.Parameters.List;
  begin
    Log.Write ("store darks - filename: " & Darks_Filename);
    P.Add (Url_Names.Id, Darks_Filename);
    Check (AWS.Client.Get (Base_URL & Url_Names.Store_Darks & P.URI_Format));
  end Store_Darks;


  procedure Send_Light_Parameters is
    P : AWS.Parameters.List;
  begin
    P.Add (Url_Names.Ra, Space.Ra_Image_Of (The_Target_Direction));
    P.Add (Url_Names.Dec, Space.Dec_Image_Of (The_Target_Direction));
    P.Add (Url_Names.Id, Darks_Filename);
    Check (AWS.Client.Get (Base_URL & Url_Names.Light_Parameters & P.URI_Format));
  end Send_Light_Parameters;


  The_Darks_To_Send : Units.Picture_Number;

  procedure New_Darks_Sequence (Target : Name.Id) is
  begin
    Log.Write ("new darks sequence");
    Check (Start);
    The_Darks_To_Send := The_Number_Of_Darks;
    Define_Parameters_For (Target);
    Check (AWS.Client.Get (Base_URL & Url_Names.New_Darks));
    The_Sequence_Type := Darks;
  end New_Darks_Sequence;


  function Send_Next_Dark_Was_Last return Boolean is
    use type Units.Picture_Number;
  begin
    Log.Write ("send next dark");
    Check (Darks);
    Send_Next;
    case The_Sequence_Type is
    when Darks =>
      if The_Darks_To_Send = 1 then
        Store_Darks;
        The_Sequence_Type := Start;
        return True;
      else
        The_Darks_To_Send := @ - 1;
        return False;
      end if;
    when others =>
      raise Program_Error;
    end case;
  end Send_Next_Dark_Was_Last;


  Is_First_Light : Boolean;

  procedure New_Light_Sequence (Target : Name.Id) is
  begin
    Log.Write ("new light sequence");
    Check (Start);
    Define_Parameters_For (Target);
    Is_First_Light := True;
  end New_Light_Sequence;


  procedure Send_Next_Light is
  begin
    Log.Write ("send next light");
    Check (Lights);
    if Is_First_Light then
      Send_Light_Parameters;
      The_Sequence_Type := Lights;
      Is_First_Light := False;
    end if;
    Send_Next;
  end Send_Next_Light;


  procedure Stop is
  begin
    if The_Sequence_Type /= Start then
      Check (AWS.Client.Get (Base_URL & Url_Names.Stop));
      The_Sequence_Type := Start;
    end if;
  end Stop;

end Picture.Feeder;
