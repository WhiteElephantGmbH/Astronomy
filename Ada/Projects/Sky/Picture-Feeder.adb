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

with Ada.Streams.Stream_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;
with Sky;
with Stellarium;
with Text;
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

  type Exposures     is array (Units.Known_Camera_Model, Sky.Object_Type) of Exposure.Item;
  type Sensitivities is array (Units.Known_Camera_Model, Sky.Object_Type) of Sensitivity.Item;

  use all type Units.Camera_Model;
  use all type Sky.Object_Type;

  Target_Exposure : constant Exposures := [
    Canon_Eos_6D | Canon_Eos_60D => [
      Moon    | Satellite | Unknown        => Exposure.Value ("1/100"),
      Cluster | Double    | Star   | Stars => Exposure.Value ("10"),
      Galaxy  | Nebula    | Quasar         => Exposure.Value ("30")],
    QHY600C => [
      Moon    => Exposure.Value ("0.005"),
      Cluster => Exposure.Value ("5"),
      others  => Exposure.Value ("15")],
    others => [others => Exposure.Value ("[]")]];

  Target_Sensitivity : constant Sensitivities := [
    Canon_Eos_6D | Canon_Eos_60D => [
      Moon    | Satellite | Unknown        => Sensitivity.Value ("100"),
      Cluster | Double    | Star   | Stars => Sensitivity.Value ("1600"),
      Galaxy  | Nebula    | Quasar         => Sensitivity.Value ("6400")],
    QHY600C => [
      Moon    => Sensitivity.Value ("[]"),
      Cluster => Sensitivity.Value ("[]"),
      others  => Sensitivity.Value ("[]")]];


  procedure Define_Parameters_For (Target           : Name.Id;
                                   Target_Direction : Space.Direction) is
  begin
    The_Target_Type := Name.Type_Of (Target);
    Log.Write ("target type: " & Text.Legible_Of (The_Target_Type'image));
    The_Exposure := Target_Exposure (Stellarium.Camera_Model, The_Target_Type);
    The_Sensitivity := Target_Sensitivity (Stellarium.Camera_Model, The_Target_Type);
    The_Target_Direction := Target_Direction;
  end Define_Parameters_For;


  function Darks_Filename return String is
  begin
    return Stellarium.Camera_Model'image & '_' & The_Exposure.Filename_Part & "_" & The_Sensitivity.Filename_Part;
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

  procedure New_Darks_Sequence (Target           : Name.Id;
                                Target_Direction : Space.Direction) is
  begin
    Log.Write ("new darks sequence for " & Name.Image_Of (Target));
    Check (Start);
    The_Darks_To_Send := The_Number_Of_Darks;
    Define_Parameters_For (Target, Target_Direction);
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

  procedure New_Light_Sequence (Target           : Name.Id;
                                Target_Direction : Space.Direction) is
  begin
    Log.Write ("new light sequence for " & Name.Image_Of (Target));
    Check (Start);
    Define_Parameters_For (Target, Target_Direction);
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
