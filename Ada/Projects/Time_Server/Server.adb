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

with AWS.Config.Set;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Clock;
with Control;
with GNATCOLL.JSON;
with Time.Server;
with Traces;

package body Server is

  package Log is new Traces ("Server");

  package JS renames GNATCOLL.JSON;


  function Response return String is

    Information : constant JS.JSON_Value := JS.Create_Object;

    procedure Set_Information is
      Data      : constant Clock.Data := Clock.Information;
      Date_Time : constant String := Time.Image_Of (Time.Universal);
    begin
      JS.Set_Field (Information, Time.Server.Clock_Exists, JS.Create (Data.Exists));
      JS.Set_Field (Information, Time.Server.Clock_Synchronized, JS.Create (Data.Is_Synchronized));
      JS.Set_Field (Information, Time.Server.Date_Time, JS.Create (Date_Time));
      JS.Set_Field (Information, Time.Server.Mount_Connected, JS.Create (Data.Mount_Connected));
      JS.Set_Field (Information, Time.Server.Mount_Synchronized, JS.Create (Data.Mount_Synchronized));
      Log.Write ("Clock Exists       : " & Data.Exists'image);
      Log.Write ("Clock Synchronized : " & Data.Is_Synchronized'image);
      Log.Write ("Date Time          : " & Date_Time);
      Log.Write ("Mount Connected    :"  & Data.Mount_Connected'image);
      Log.Write ("Mount Synchronized :"  & Data.Mount_Synchronized'image);
    end Set_Information;

  begin -- Information
    Set_Information;
    return JS.Write (Information);
  end Response;


  function Callback (Data : AWS.Status.Data) return AWS.Response.Data is
     The_Parameters : AWS.Parameters.List;
  begin
    The_Parameters := AWS.Status.Parameters (Data);
    declare
      Action : constant String := The_Parameters.Get_Name;
    begin
      Log.Write ("Callback - Action: " & Action);
      if Action = Time.Server.Shutdown then
        Control.Shutdown;
        return AWS.Response.Acknowledge (AWS.Messages.S200, Time.Server.Response_Ok);
      elsif Action = Time.Server.Synchronize_Mount then
        if Clock.Synchronize_Mount_Started then
          return AWS.Response.Acknowledge (AWS.Messages.S200, Time.Server.Response_Ok);
        else
          return AWS.Response.Acknowledge (AWS.Messages.S200, Time.Server.Response_Failed);
        end if;
      elsif Action = Time.Server.Get_Information then
        return  AWS.Response.Acknowledge (AWS.Messages.S200, Response);
      else
        return AWS.Response.Acknowledge (AWS.Messages.S400, "unknown command");
      end if;
    end;
  exception
  when Item: others =>
    Log.Termination (Item);
    return AWS.Response.Acknowledge (AWS.Messages.S400, "exception in callback");
  end Callback;


  The_Server : AWS.Server.HTTP; -- uses AWS.Config.Get_Current

  procedure Start is
    The_Config : AWS.Config.Object := AWS.Config.Get_Current;
  begin
    Log.Write ("Start");
    AWS.Config.Set.Server_Name (The_Config, "Time Server");
    AWS.Config.Set.Server_Host (The_Config, "");
    AWS.Config.Set.Server_Port (The_Config, Time.Server.Port);
    AWS.Config.Set.Security (The_Config, False);
    AWS.Config.Set.Session (The_Config, False);
    AWS.Config.Set.Reuse_Address (The_Config, True);
    AWS.Config.Set.Case_Sensitive_Parameters (The_Config, True);
    AWS.Server.Start (The_Server, Callback'access, The_Config);
  end Start;


  procedure Shutdown is
  begin
    Log.Write ("Shutdown");
    AWS.Server.Shutdown (The_Server);
  exception
  when others =>
    null;
  end Shutdown;

end Server;
