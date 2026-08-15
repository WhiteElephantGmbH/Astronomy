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
with Ten_Micron;
with Time.Server;
with Traces;

package body Server is

  package Log is new Traces ("Server");

  package JS renames GNATCOLL.JSON;
  package TS renames Time.Server;


  function Response return String is

    Information : constant JS.JSON_Value := JS.Create_Object;

    procedure Set_Information is
      Time_Image         : constant String  := Time.Image_Of (Time.Julian_Date);
      Clock_Set          : constant Boolean := Time.Is_Set;
      Clock_Set_From_Pc  : constant Boolean := Clock.Is_Set_From_Pc;
      Clock_Synchronized : constant Boolean := Clock.Is_Synchronized;
      Mount_Connected    : constant Boolean := Ten_Micron.Connected;
      Mount_Synchronized : constant Boolean := Ten_Micron.Is_Synchronized;
    begin
      JS.Set_Field (Information, TS.Clock_Time, JS.Create (Time_Image));
      JS.Set_Field (Information, TS.Clock_Set, JS.Create (Clock_Set));
      JS.Set_Field (Information, TS.Clock_Set_From_Pc, JS.Create (Clock_Set_From_Pc));
      JS.Set_Field (Information, TS.Clock_Synchronized, JS.Create (Clock_Synchronized));
      JS.Set_Field (Information, TS.Mount_Connected, JS.Create (Mount_Connected));
      JS.Set_Field (Information, TS.Mount_Synchronized, JS.Create (Mount_Synchronized));
      Log.Write ("Clock Time         : " & Time_Image);
      Log.Write ("Clock Set          : " & Clock_Set'image);
      Log.Write ("Clock Set from PC  : " & Clock_Set_From_Pc'image);
      Log.Write ("Clock Synchronized : " & Clock_Synchronized'image);
      Log.Write ("Mount Connected    : " & Mount_Connected'image);
      Log.Write ("Mount Synchronized : " & Mount_Synchronized'image);
    end Set_Information;

  begin -- Information
    Set_Information;
    return JS.Write (Information);
  end Response;


  function Callback (Data : AWS.Status.Data) return AWS.Response.Data is

    Ok     : constant String := TS.Response_Ok;
    Failed : constant String := TS.Response_Failed;

    The_Parameters : AWS.Parameters.List;

  begin
    The_Parameters := AWS.Status.Parameters (Data);
    declare
      Action : constant String := The_Parameters.Get_Name;
    begin
      Log.Write ("Callback - Action: " & Action);
      if Action = TS.Shutdown then
        Control.Shutdown;
        return AWS.Response.Acknowledge (AWS.Messages.S200, Ok);
      elsif Action = TS.Synchronize_Mount then
        if Clock.Synchronized_Mount then
          return AWS.Response.Acknowledge (AWS.Messages.S200, Ok);
        else
          return AWS.Response.Acknowledge (AWS.Messages.S200, Failed);
        end if;
      elsif Action = TS.Set_Date_Time then
        declare
          Value : constant String := The_Parameters.Get_Value;
        begin
          Clock.Set (Pc_Time => Value);
          return AWS.Response.Acknowledge (AWS.Messages.S200, (if Clock.Is_Set_From_Pc then Ok else Failed));
        end;
      elsif Action = TS.Get_Information then
        return AWS.Response.Acknowledge (AWS.Messages.S200, Response);
      else
        Log.Error ("Unknown Action: """ & Action & '"');
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
    AWS.Config.Set.Server_Port (The_Config, TS.Port);
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
