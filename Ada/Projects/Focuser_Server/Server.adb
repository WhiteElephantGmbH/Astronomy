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

with AWS.Messages;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Control;
with GNATCOLL.JSON;
with Protected_Storage;
with Text;
with Traces;

package body Server is

  package Log is new Traces ("Server");

  package Protected_Focuser is new Protected_Storage (Focuser.Data);

  package JS renames GNATCOLL.JSON;


  function Response return String is

    Information : constant JS.JSON_Value := JS.Create_Object;

    procedure Set_Information is
      Data : constant Focuser.Data := Protected_Focuser.Data;
    begin
      JS.Set_Field (Information, "exists", JS.Create (Data.Exists));
      JS.Set_Field (Information, "moving", JS.Create (Data.Moving));
      JS.Set_Field (Information, "position", JS.Create (Data.Position));
      JS.Set_Field (Information, "backlash", JS.Create (Natural(Data.Backlash)));
      JS.Set_Field (Information, "speed", JS.Create (Data.Speed));
      Log.Write ("Focuser Exists   : " & Data.Exists'image);
      Log.Write ("Focuser Moving   : " & Data.Moving'image);
      Log.Write ("Focuser Position :"  & Data.Position'image);
      Log.Write ("Focuser Backlash :"  & Data.Backlash'image);
      Log.Write ("Focuser Speed    :"  & Data.Speed'image);
    end Set_Information;

  begin -- Information
    Set_Information;
    return JS.Write (Information);
  end Response;


  function Callback (Data : AWS.Status.Data) return AWS.Response.Data is

    Error : exception;

    Error_Message : Text.String;

    procedure Raise_Error (Message : String) with No_Return is
    begin
      Log.Error (Message);
      Error_Message := [Message];
      raise Error;
    end Raise_Error;

    Uri    : constant String := AWS.Status.URI (Data);
    Parts  : constant Text.Strings := Text.Strings_Of (Uri, Separator => '/', Symbols => "=");

    Action : constant String := Parts(1);

    function Parameter return Natural is
    begin
      if Parts(2) /= "=" then
        Raise_Error ("expected separator '=' in parameter");
      end if;
      return Natural'value(Parts(3));
    end Parameter;

  begin -- Callback
    Log.Write ("Callback - URI: " & Uri);
    if Action = Focuser.Get_Data_Command then
      return AWS.Response.Acknowledge (AWS.Messages.S200, Response);
    elsif Action = Focuser.Execute_Command then
      declare
        Command_Number : constant Natural := Parameter;
      begin
        Focuser.Execute (Focuser.Command'val(Command_Number));
        return AWS.Response.Acknowledge (AWS.Messages.S200, Response);
      exception
      when others =>
        Raise_Error ("unknown focuser command:" & Command_Number'image);
      end;
    elsif Action = Focuser.Move_To_Command then
      declare
        Position : constant Natural := Parameter;
      begin
        Focuser.Move_To (Position);
        return AWS.Response.Acknowledge (AWS.Messages.S200, Response);
      exception
      when others =>
        Raise_Error ("unknown focuser position:" & Position'image);
      end;
    elsif Action = Focuser.Set_Lash_Command then
      declare
        Backlash : constant Natural := Parameter;
      begin
        Focuser.Set (Focuser.Lash(Backlash));
        return AWS.Response.Acknowledge (AWS.Messages.S200, Response);
      exception
      when others =>
        Raise_Error ("focuser backlash:" & Backlash'image);
      end;
    elsif Action = Focuser.Shutdown_Command then
      Control.Shutdown;
      return AWS.Response.Acknowledge (AWS.Messages.S200, "Ok");
    else
      Raise_Error ("unknown action: " & Action);
    end if;
  exception
  when Error =>
    Focuser.Execute (Focuser.Stop);
    return AWS.Response.Acknowledge (AWS.Messages.S400, Error_Message.To_String);
  when Item: others =>
    Focuser.Execute (Focuser.Stop);
    Log.Termination (Item);
    return AWS.Response.Acknowledge (AWS.Messages.S400, "exception in response handling");
  end Callback;


  The_Server : AWS.Server.HTTP;

  procedure Start is
  begin
    Log.Write ("Start");
    AWS.Server.Start (Web_Server => The_Server,
                      Name       => "Skytracker",
                      Callback   => Callback'access,
                      Port       => Celestron.Focuser.Default_Port_Number);
  end Start;


  procedure Update (Data : Focuser.Data) is
  begin
    Protected_Focuser.Set (Data);
  end Update;


  procedure Shutdown is
  begin
    Log.Write ("Shutdown");
    AWS.Server.Shutdown (The_Server);
  exception
  when others =>
    null;
  end Shutdown;

end Server;
