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

with AWS.Client;
with AWS.Messages;
with AWS.Response;
with GNATCOLL.JSON;
with Text;
with Traces;

package body Focuser_Client is

  package Log is new Traces (Id);

  package JS renames GNATCOLL.JSON;


  function Server_Exists return Boolean is (The_Server_Exists);


  function Get (Command   : String;
                Parameter : String := "") return AWS.Response.Data is

    Host    : constant String := Network.Image_Of (The_Client_Address);
    Port    : constant String := Text.Trimmed (The_Client_Port'image);
    Address : constant String := "http://" & Host & ":" & Port & "/" & Command;
    Url     : constant String := (if Parameter = "" then Address else Address & "=" & Parameter);
  begin
    return AWS.Client.Get (Url);
  end Get;


  function Get (Command   : String;
                Parameter : String := "") return Focuser.Data is

    Response : constant AWS.Response.Data := Get (Command, Parameter);
    Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);

    use type AWS.Messages.Status_Code;

  begin
    if Status /= AWS.Messages.S200 then
      Log.Error ("Get Status: " & Status'image);
      raise Server_Not_Available;
    end if;
    declare
      Result : constant String := AWS.Response.Message_Body (Response);
    begin
      declare
        Value : constant JS.JSON_Value := JS.Read (Result);
      begin
        declare
          Exists   : constant JS.JSON_Value := Value.Get ("exists");
          Moving   : constant JS.JSON_Value := Value.Get ("moving");
          Position : constant JS.JSON_Value := Value.Get ("position");
          Backlash : constant JS.JSON_Value := Value.Get ("backlash");
          Speed    : constant JS.JSON_Value := Value.Get ("speed");
          The_Data : Focuser.Data;
        begin
          The_Data.Exists := Exists.Get;
          The_Data.Moving := Moving.Get;
          The_Data.Position := Position.Get;
          The_Data.Backlash := Focuser.Lash(Natural'(Backlash.Get));
          The_Data.Speed := Speed.Get;
          return The_Data;
        end;
      end;
    end;
  end Get;


  function Actual_Data return Focuser.Data is
  begin
    return Get (Command => Focuser.Get_Data_Command);
  end Actual_Data;


  function Execute (Command : Focuser.Command) return Focuser.Data is
  begin
    Log.Write ("Execute: " & Command'image);
    return Get (Command   => Focuser.Execute_Command,
                Parameter => Text.Trimmed (Focuser.Command'pos(Command)'image));
  end Execute;


  function Move_To (Position : Focuser.Distance) return Focuser.Data is
  begin
    Log.Write ("Move_To:" & Position'image);
    return Get (Command   => Focuser.Move_To_Command,
                Parameter => Text.Trimmed (Position'image));
  end Move_To;


  function Set (Backlash : Focuser.Lash) return Focuser.Data is
  begin
    Log.Write ("Set Backlash:" & Backlash'image);
    return Get (Command   => Focuser.Set_Lash_Command,
                Parameter => Text.Trimmed (Backlash'image));
  end Set;


  procedure Shutdown is
    Response : constant AWS.Response.Data := Get (Focuser.Shutdown_Command);
    Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);
    use type AWS.Messages.Status_Code;
  begin
    Log.Write ("Shutdown");
    if Status /= AWS.Messages.S200 then
      Log.Error ("Shutdown: " & Status'image);
    end if;
  end Shutdown;

end Focuser_Client;
