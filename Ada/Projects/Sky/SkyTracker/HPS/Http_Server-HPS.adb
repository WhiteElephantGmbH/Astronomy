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
with AWS.Status;
with Focuser_Client;
with Input;
with Protected_Storage;

package body Http_Server.HPS is

  package Protected_Mount is new Protected_Storage (Mount_Data);

  package Protected_Moving_Speed is new Protected_Storage (Angle.Value);

  package Protected_Focuser is new Protected_Storage (Focuser.Data);


  procedure Execute (The_Command : Focuser.Command) is
  begin
    if Focuser_Client.Server_Exists then
      Set (Focuser_Client.Execute (The_Command));
    end if;
  exception
  when Focuser_Client.Server_Not_Available =>
    Log.Warning ("Focuser server not available");
  end Execute;


  function Moving_Speed return String is

    Arc_Delta : constant := 0.01;
    type Arc is delta Arc_Delta range 0.0 .. 100.0 - Arc_Delta with Small => Arc_Delta;

    use type Angle.Degrees;
    use type Angle.Value;

    Speed : Angle.Degrees := +Protected_Moving_Speed.Data;

  begin
    if Speed = 0.0 then
      return "";
    elsif Speed >= 1.0 then
      return Text.Trimmed (Arc(Speed)'image & Angle.Degree & "/s");
    else
      Speed := @ * 60.0;
      if Speed >= 1.0 then
        return Text.Trimmed (Arc(Speed)'image & "'/s");
      else
        return Text.Trimmed (Natural(Speed * 60.0)'image & """/s");
      end if;
    end if;
  end Moving_Speed;


  function Information return String is

    Info : constant JS.JSON_Value := JS.Create_Object;

    procedure Set_Mount_Values is
      Mount  : constant JS.JSON_Value := JS.Create_Object;
      Data   : constant Mount_Data    := Protected_Mount.Data;
      Speed  : constant String        := Moving_Speed;
    begin
      JS.Set_Field (Mount, "speed", JS.Create (Speed));
      JS.Set_Field (Mount, "exists", JS.Create (Data.Exists));
      JS.Set_Field (Info, "mount", Mount);
      Log.Write ("Mount Speed  : " & Speed);
      Log.Write ("Mount Exists : " & Data.Exists'image);
    end Set_Mount_Values;


    procedure Set_Focuser_Values is
      Value : constant JS.JSON_Value := JS.Create_Object;
      Data  : constant Focuser.Data  := Protected_Focuser.Data;
    begin
      JS.Set_Field (Value, "exists", JS.Create (Data.Exists));
      JS.Set_Field (Value, "moving", JS.Create (Data.Moving));
      JS.Set_Field (Value, "position", JS.Create (Data.Position));
      JS.Set_Field (Value, "rate", JS.Create (Data.Speed));
      JS.Set_Field (Info, "focuser", Value);
      Log.Write ("Focuser Exists   : " & Data.Exists'image);
      Log.Write ("Focuser Moving   : " & Data.Moving'image);
      Log.Write ("Focuser Position :"  & Data.Position'image);
      Log.Write ("Focuser Speed    :"  & Data.Speed'image);
    end Set_Focuser_Values;

  begin -- Information
    Set_Control_Values (Info);
    Set_Mount_Values;
    Set_Focuser_Values;
    return JS.Write (Info);
  end Information;


  function Callback (Data : AWS.Status.Data) return AWS.Response.Data is
  begin
    Log.Write ("Callback - URI: " & AWS.Status.URI (Data));
    declare
      Uri       : constant String := AWS.Status.URI (Data);
      Parts     : constant Text.Strings := Text.Strings_Of (Uri, Separator => '/');
      Subsystem : constant String := Parts(1);
    begin
      if Subsystem in "mount" then
        declare
          Command_Image : constant String := Parts(2);
        begin
          declare
            Command : constant Input.Command := Input.Command'value(Parts(2));
          begin
            Input.Put (Command, From => Input.Server);
            return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
          end;
        exception
        when others =>
          Log.Error ("Unknown mount command: " & Command_Image);
          Input.Put (Input.End_Command, From => Input.Server);
          return AWS.Response.Acknowledge (AWS.Messages.S400, "unknown mount command");
        end;
      elsif Subsystem = "focuser" then
        declare
          Command_Image : constant String := Parts(2);
        begin
          declare
            Command : constant Focuser.Command := Focuser.Command'value(Command_Image);
          begin
            Execute (Command);
            return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
          end;
        exception
        when others =>
          Log.Error ("Unknown focuser command: " & Command_Image);
          Execute (Focuser.Stop);
          return AWS.Response.Acknowledge (AWS.Messages.S400, "unknown focuser command");
        end;
      elsif Subsystem = "information" then
        return AWS.Response.Acknowledge (AWS.Messages.S200, Information);
      else
        return AWS.Response.Acknowledge (AWS.Messages.S400, "unknown subsystem");
      end if;
    end;
  exception
  when Item: others =>
    Log.Termination (Item);
    return AWS.Response.Acknowledge (AWS.Messages.S400, "exception in response handling");
  end Callback;


  procedure Start is
  begin
    Start (Callback'access);
  end Start;


  procedure Set_Moving (Speed : Angle.Value) renames Protected_Moving_Speed.Set;

  procedure Set (Data : Mount_Data) renames Protected_Mount.Set;

  procedure Set (Data : Focuser.Data) renames Protected_Focuser.Set;

end Http_Server.HPS;
