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

with AWS.Client;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;
with GNATCOLL.JSON;
with Url_Names;

package body Stellarium.Client is

  package JS renames GNATCOLL.JSON;


  function Url (Path : String) return String is
    Host : constant String := Network.Image_Of (The_Client_Address);
    Port : constant String := Text.Trimmed (The_Client_Port'image);
  begin
    return "http://" & Host & ":" & Port & Path;
  end Url;


  function Data_Of (Format : String) return String is
  begin
    return Format(Format'first + 1 .. Format'last); --remove '?'
  end Data_Of;


  function Checked (Result : AWS.Response.Data) return AWS.Response.Data is
    Status : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Result);
  begin
    if not (Status in  AWS.Messages.Success) then
      Log.Error ("Status:" & AWS.Response.Status_Code (Result)'image);
      raise Operation_Failed;
    end if;
    return Result;
  end Checked;


  procedure Check (Result : AWS.Response.Data) is
    Dummy : constant AWS.Response.Data := Checked (Result);
  begin
    null;
  end Check;


  function Get_Response (Path : String) return AWS.Response.Data is

  begin
    return Checked (AWS.Client.Get (Url (Path)));
  end Get_Response;


  function Get_Target return Target_Data is
    Response : constant AWS.Response.Data := Get_Response ("/api/objects/info?format=json");
  begin
    declare
      Result : constant String := AWS.Response.Message_Body (Response);
    begin
      declare
        Value : constant JS.JSON_Value := JS.Read (Result);
      begin
        declare
          Target    : constant JS.JSON_Value := Value.Get ("name");
          Kind      : constant JS.JSON_Value := Value.Get ("type");
          Ra        : constant JS.JSON_Value := Value.Get ("ra");
          Dec       : constant JS.JSON_Value := Value.Get ("dec");
          Ra_J2000  : constant JS.JSON_Value := Value.Get ("raJ2000");
          Dec_J2000 : constant JS.JSON_Value := Value.Get ("decJ2000");
          The_Data  : Target_Data;
        begin
          The_Data := (Target          => [Target.Get],
                       Kind            => [Kind.Get],
                       Direction       => Space.Direction_Of (Ra  => Angle.Degrees(Ra.Get_Long_Float),
                                                              Dec => Angle.Degrees(Dec.Get_Long_Float)),
                       Direction_J2000 => Space.Direction_Of (Ra  => Angle.Degrees(Ra_J2000.Get_Long_Float),
                                                              Dec => Angle.Degrees(Dec_J2000.Get_Long_Float)));
          return The_Data;
        end;
      end;
    exception
    when others =>
      return No_Target;
    end;
  exception
  when Operation_Failed =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise;
  end Get_Target;


  procedure Set_Focus_To (Target_Name : String;
                          Mode        : Focus_Mode) is
    Parameters : AWS.Parameters.List;
    Mode_Image : constant String := Text.Lowercase_Of (Mode'image);
  begin
    Log.Write ("Set_Focus_To: " & Target_Name & " - Mode: " & Mode_Image);
    Parameters.Add (Url_Names.Target, Target_Name);
    Parameters.Add (Url_Names.Mode, Mode_Image);
    Check (AWS.Client.Post (URL  => Url (Path => "/api/main/focus"),
                            Data => Data_Of (Parameters.URI_Format)));
  end Set_Focus_To;


  Api_Property : constant String := "/api/stelproperty/";

  CCD_Rotation_Angle      : constant String := "Oculars.selectedCCDRotationAngle";
  Oculars_CCD_Index       : constant String := "Oculars.selectedCCDIndex";
  Oculars_Lens_Index      : constant String := "Oculars.selectedLensIndex";
  Oculars_Ocular_Index    : constant String := "Oculars.selectedOcularIndex";
  Oculars_Telescope_Index : constant String := "Oculars.selectedTelescopeIndex";


  function Get_Properties return JS.JSON_Value is
  begin
    return JS.Read (String'(AWS.Response.Message_Body (Get_Response (Api_Property & "list?format=json"))));
  exception
  when others =>
    raise Operation_Failed;
  end Get_Properties;


  function Index_Of (Item : JS.JSON_Value;
                     Id   : String) return Property_Index is
    Json_Value : constant JS.JSON_Value := Item.Get (Id);
  begin
    return Property_Index(Integer'(Json_Value.Get (Url_Names.Value)));
  end Index_Of;


  function Get return Data is

    Properties : constant JS.JSON_Value := Get_Properties;

    function Property_Index_Of (Id : String) return Property_Index is (Index_Of (Properties, Id));

    function Long_Float_Of (Item : String) return Long_Float is
      Json_Value : constant JS.JSON_Value := Properties.Get (Item).Get (Url_Names.Value);
    begin
      case Json_Value.Kind is
      when JS.JSON_Int_Type =>
        return Long_Float (Integer'(Json_Value.Get));
      when JS.JSON_Float_Type =>
        return Json_Value.Get_Long_Float;
      when others =>
        raise Program_Error;
      end case;
    end Long_Float_Of;

  begin -- Get
    declare
      CCD_Index       : constant Property_Index := Property_Index_Of (Oculars_CCD_Index);
      Lens_Index      : constant Property_Index := Property_Index_Of (Oculars_Lens_Index);
      Ocular_Index    : constant Property_Index := Property_Index_Of (Oculars_Ocular_Index);
      Telescope_Index : constant Property_Index := Property_Index_Of (Oculars_Telescope_Index);
    begin
      return (Rotation  => Units.Rotation(Long_Float_Of (CCD_Rotation_Angle)),
              Camera    => Camera (CCD_Index),
              Lens      => (if Lens_Index = Not_Available then No_Lens_Data else Lens (Lens_Index)),
              Ocular    => Ocular (Ocular_Index),
              Telescope => Telescope (Telescope_Index));
    end;
  exception
  when Operation_Failed =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise Operation_Failed;
  end Get;


  function Get_Index (Id : String) return Property_Index is
    Properties : constant JS.JSON_Value := Get_Properties;
  begin
    return Index_Of (Properties, Id);
  exception
  when Operation_Failed =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise Operation_Failed;
  end Get_Index;


  procedure Set_Property (Id    : String;
                          Value : String) is
    Parameters : AWS.Parameters.List;
  begin
    Parameters.Add (Url_Names.Id, Id);
    Parameters.Add (Url_Names.Value, Value);
    Check (AWS.Client.Post (URL  => Url (Path => Api_Property & "set"),
                            Data => Data_Of (Parameters.URI_Format)));
  end Set_Property;


  procedure Set_Rotation (Value : Units.Rotation) is
    Value_Image : constant String := Text.Trimmed (Value'image);
  begin
    Log.Write ("Set_Rotation: " & Value_Image);
    Set_Property (CCD_Rotation_Angle, Value_Image);
  end Set_Rotation;


  procedure Set_Index (Id    : String;
                       Index : Property_Index) is
    Index_Image : constant String := Text.Trimmed (Index'image);
  begin
    Set_Property (Id, Index_Image);
    if Get_Index (Id) /= Index then
      raise Not_Found;
    end if;
  end Set_Index;


  procedure Set_CCD (Index : Property_Id) is
  begin
    Set_Index (Oculars_CCD_Index, Index);
  end Set_CCD;


  procedure Set_Lens (Index : Property_Index) is
  begin
    Set_Index (Oculars_Lens_Index, Index);
  end Set_Lens;


  procedure Set_Ocular (Index : Property_Id) is
  begin
    Set_Index (Oculars_Ocular_Index, Index);
  end Set_Ocular;


  procedure Set_Telescope (Index : Property_Id) is
  begin
    Set_Index (Oculars_Telescope_Index, Index);
  end Set_Telescope;

end Stellarium.Client;
