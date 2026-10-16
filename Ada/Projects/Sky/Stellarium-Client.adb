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
with AWS.Response;
with GNATCOLL.JSON;

with Ada.Text_IO;

package body Stellarium.Client is

  package JS renames GNATCOLL.JSON;


  function Get_Response (Path : String) return AWS.Response.Data is

    Host : constant String := Network.Image_Of (The_Client_Address);
    Port : constant String := Text.Trimmed (The_Client_Port'image);
    Url  : constant String := "http://" & Host & ":" & Port & Path;
  begin
    return AWS.Client.Get (Url);
  end Get_Response;


  function Get_Target return Target_Data is

    Response : constant AWS.Response.Data := Get_Response ("/api/objects/info?format=json");
    Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);

    use type AWS.Messages.Status_Code;

  begin
    if Status /= AWS.Messages.S200 then
      raise No_Data;
    end if;
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
    end;
  exception
  when No_Data =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise;
  end Get_Target;


  function Get return Data is

    Response : constant AWS.Response.Data := Get_Response ("/api/stelproperty/list?format=json");
    Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);

    use type AWS.Messages.Status_Code;

  begin
    if Status /= AWS.Messages.S200 then
      raise No_Data;
    end if;
    declare
      Result     : constant String := AWS.Response.Message_Body (Response);
      Properties : constant JS.JSON_Value := JS.Read (Result);
      Value      : constant String := "value";

      function Integer_Of (Item : String) return Integer is
        Json_Value : constant JS.JSON_Value := Properties.Get (Item);
      begin
        return Json_Value.Get (Value);
      end Integer_Of;

      function Long_Float_Of (Item : String) return Long_Float is
        Json_Value : constant JS.JSON_Value := Properties.Get (Item).Get (Value);
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

      CCD_Index       : constant Integer := Integer_Of ("Oculars.selectedCCDIndex");
      Lens_Index      : constant Integer := Integer_Of ("Oculars.selectedLensIndex");
      Ocular_Index    : constant Integer := Integer_Of ("Oculars.selectedOcularIndex");
      Telescope_Index : constant Integer := Integer_Of ("Oculars.selectedTelescopeIndex");

      Not_Available : constant Integer := -1;

    begin
      Ada.Text_IO.Put_Line ("CCD_Index:" & CCD_Index'image);
      Ada.Text_IO.Put_Line ("Lens_Index:" & Lens_Index'image);
      Ada.Text_IO.Put_Line ("Occular_Index:" & Ocular_Index'image);
      Ada.Text_IO.Put_Line ("Telescope_Index:" & Telescope_Index'image);

      return (Rotation  => Angle.Degrees(Long_Float_Of ("Oculars.selectedCCDRotationAngle")),
              Camera    => Camera (CCD_Index),
              Lens      => (if Lens_Index = Not_Available then No_Lens_Data else Lens (Lens_Index)),
              Ocular    => (if Ocular_Index = Not_Available then No_Ocular_Data else Ocular (Ocular_Index)),
              Telescope => Telescope (Telescope_Index));
    end;
  exception
  when No_Data =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise;
  end Get;

end Stellarium.Client;
