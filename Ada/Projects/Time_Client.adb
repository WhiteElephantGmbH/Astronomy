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
with Text;
with Traces;

package body Time_Client is

  package Log is new Traces (Id);

  package JS renames GNATCOLL.JSON;


  function Get (Command   : String;
                Parameter : String := "") return AWS.Response.Data is

    Host     : constant String := Network.Image_Of (The_Client_Address);
    Port     : constant String := Text.Trimmed (The_Client_Port'image);
    Address  : constant String := "http://" & Host & ":" & Port & "?" & Command;
    Url      : constant String := (if Parameter = "" then Address else Address & "=" & Parameter);
    Response : constant AWS.Response.Data := AWS.Client.Get (Url);
    Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);

    use type AWS.Messages.Status_Code;

  begin
    if Status /= AWS.Messages.S200 then
      Log.Error ("Actual_Information: " & Status'image);
      raise Server_Not_Available;
    end if;
    return Response;
  end Get;


  function Actual_Information return Time.Server.Information is

    Result : constant String := AWS.Response.Message_Body (Get (Time.Server.Get_Information));
    Value  : constant JS.JSON_Value := JS.Read (Result);

    Clock_Set          : constant JS.JSON_Value := Value.Get (Time.Server.Clock_Set);
    Clock_Set_From_Pc  : constant JS.JSON_Value := Value.Get (Time.Server.Clock_Set_From_Pc);
    Clock_Synchronized : constant JS.JSON_Value := Value.Get (Time.Server.Clock_Synchronized);
    Clock_Time         : constant JS.JSON_Value := Value.Get (Time.Server.Clock_Time);
    Mount_Connected    : constant JS.JSON_Value := Value.Get (Time.Server.Mount_Connected);
    Mount_Synchronized : constant JS.JSON_Value := Value.Get (Time.Server.Mount_Synchronized);

    The_Information : Time.Server.Information;

  begin
    The_Information.Clock_Set := Clock_Set.Get;
    The_Information.Clock_Set_From_Pc := Clock_Set_From_Pc.Get;
    The_Information.Clock_Synchronized := Clock_Synchronized.Get;
    The_Information.Clock_Time := Time.JD'value(String'(Clock_Time.Get));
    The_Information.Mount_Connected := Mount_Connected.Get;
    The_Information.Mount_Synchronized := Mount_Synchronized.Get;
    return The_Information;
  end Actual_Information;


  function Execute (Command   : String;
                    Parameter : String := "") return Boolean is

    Response : constant AWS.Response.Data := Get (Command, Parameter);
    Result   : constant String := AWS.Response.Message_Body (Response);

  begin
    if Result = Time.Server.Response_Ok then
      return True;
    elsif Result = Time.Server.Response_Failed then
      return False;
    else
      Log.Error (Command & ": " & Result);
      return False;
    end if;
  end Execute;


  function Synchronize_Mount return Boolean is
  begin
    Log.Write ("Synchronize_Mount");
    return Execute (Time.Server.Synchronize_Mount);
  end Synchronize_Mount;


  function Set (Item : Time.Unix_JD) return Boolean is
    Julian_Date : constant String := Text.Trimmed (Item'image);
  begin
    Log.Write ("Set - Julian Date: " & Item'image);
    return Execute (Time.Server.Set_Date_Time, Julian_Date);
  end Set;


  procedure Shutdown is
  begin
    Log.Write ("Shutdown");
    if Execute (Time.Server.Shutdown) then
      null;
    end if;
  end Shutdown;

end Time_Client;
