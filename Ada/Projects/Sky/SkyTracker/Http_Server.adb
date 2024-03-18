-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with AWS.Status;
with AWS.Server;
with AWS.Response;
with GNATCOLL.JSON;
with Input;
with Lexicon;
with Os.Process;
with Protected_Storage;

package body Http_Server is

  package JS renames GNATCOLL.JSON;

  package Protected_Control is new Protected_Storage (Control_Data);

  package Protected_Mount is new Protected_Storage (Mount_Data);

  package Protected_Mount_State is new Protected_Storage (Text.String);

  package Protected_Moving_Speed is new Protected_Storage (Angle.Value);

  package Protected_Mirror_Position is new Protected_Storage (Mirror_Position);

  package Protected_Focuser is new Protected_Storage (Focuser_Data);

  package Protected_Rotator is new Protected_Storage (Rotator_Data);

  use type Text.String;


  function Image_Of (Item : Degrees) return String is
  begin
    return Text.Trimmed (Item'image);
  end Image_Of;


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


  function Mirror_Direction_Image return String is
  begin
    case Protected_Mirror_Position.Data is
    when Unknown | Between =>
      return "";
    when Ocular =>
      return Lexicon.Image_Of (Lexicon.Ocular);
    when Camera =>
      return Lexicon.Image_Of (Lexicon.Camera);
    end case;
  end Mirror_Direction_Image;


  function Information return String is

    Info : constant JS.JSON_Value := JS.Create_Object;

    procedure Set_Control_Values is
      Control : constant JS.JSON_Value := JS.Create_Object;
      Data    : constant Control_Data  := Protected_Control.Data;
    begin
      JS.Set_Field (Control, "window_minimized", JS.Create (Data.Window_Minimized));
      JS.Set_Field (Info, "control", Control);
    end Set_Control_Values;

    procedure Set_Mount_Values is
      Mount  : constant JS.JSON_Value := JS.Create_Object;
      Data   : constant Mount_Data    := Protected_Mount.Data;
      State  : constant String        := +Protected_Mount_State.Data;
      Speed  : constant String        := Moving_Speed;
      Axis0  : constant String        := Image_Of (Data.Axis0);
      Axis1  : constant String        := Image_Of (Data.Axis1);
      Points : constant String        := Text.Trimmed (Data.Model_Points'image);
    begin
      JS.Set_Field (Mount, "state", JS.Create (State));
      JS.Set_Field (Mount, "speed", JS.Create (Speed));
      JS.Set_Field (Mount, "exists", JS.Create (Data.Exists));
      JS.Set_Field (Mount, "axis0", JS.Create (Axis0));
      JS.Set_Field (Mount, "axis1", JS.Create (Axis1));
      JS.Set_Field (Mount, "points", JS.Create (Points));
      JS.Set_Field (Info, "mount", Mount);
      Log.Write ("Mount State  : " & State);
      Log.Write ("Mount Speed  : " & Speed);
      Log.Write ("Mount Exists : " & Data.Exists'image);
      Log.Write ("Mount axis0  : " & Axis0);
      Log.Write ("Mount axis1  : " & Axis1);
      Log.Write ("Mount Points : " & Points);
    end Set_Mount_Values;

    procedure Set_M3_Values is
      Position  : constant Mirror_Position := Protected_Mirror_Position.Data;
      M3        : constant JS.JSON_Value := JS.Create_Object;
      Exists    : constant Boolean       := Position /= Unknown;
      At_Camera : constant Boolean       := Position = Camera;
    begin
      JS.Set_Field (M3, "exists", JS.Create (Exists));
      JS.Set_Field (M3, "at_camera", JS.Create (At_Camera));
      JS.Set_Field (M3, "position", JS.Create (Mirror_Direction_Image));
      JS.Set_Field (Info, "m3", M3);
      Log.Write ("M3 Exists    : " & Exists'image);
      Log.Write ("M3 At_Camera : " & At_Camera'image);
    end Set_M3_Values;

    procedure Set_Focuser_Values is
      Focuser : constant JS.JSON_Value := JS.Create_Object;
      Data    : constant Focuser_Data  := Protected_Focuser.Data;
    begin
      JS.Set_Field (Focuser, "exists", JS.Create (Data.Exists));
      JS.Set_Field (Focuser, "moving", JS.Create (Data.Moving));
      JS.Set_Field (Focuser, "max_position", JS.Create (Data.Max_Position));
      JS.Set_Field (Focuser, "zoom_size", JS.Create (Data.Zoom_Size));
      JS.Set_Field (Focuser, "position", JS.Create (Data.Position));
      JS.Set_Field (Info, "focuser", Focuser);
      Log.Write ("Focuser Exists        : " & Data.Exists'image);
      Log.Write ("Focuser Moving        : " & Data.Moving'image);
      Log.Write ("Focuser Max Position  :"  & Data.Max_Position'image);
      Log.Write ("Focuser Zoom Size     :"  & Data.Zoom_Size'image);
      Log.Write ("Focuser Position      :"  & Data.Position'image);
    end Set_Focuser_Values;

    procedure Set_Rotator_Values is
      Rotator       : constant JS.JSON_Value := JS.Create_Object;
      Data          : constant Rotator_Data  := Protected_Rotator.Data;
      Field_Angle   : constant String        := Image_Of (Data.Field_Angle);
      Mech_Position : constant String        := Image_Of (Data.Mech_Position);
    begin
      JS.Set_Field (Rotator, "exists", JS.Create (Data.Exists));
      JS.Set_Field (Rotator, "moving", JS.Create (Data.Moving));
      JS.Set_Field (Rotator, "slewing", JS.Create (Data.Slewing));
      JS.Set_Field (Rotator, "field_angle", JS.Create (Field_Angle));
      JS.Set_Field (Rotator, "mech_position", JS.Create (Mech_Position));
      JS.Set_Field (Info, "rotator", Rotator);
      Log.Write ("Rotator Moving        : " & Data.Moving'image);
      Log.Write ("Rotator Slewing       : " & Data.Slewing'image);
      Log.Write ("Rotator Field Angle   : " & Field_Angle);
      Log.Write ("Rotator Mech Position : " & Mech_Position);
    end Set_Rotator_Values;

  begin -- Information
    Set_Control_Values;
    Set_Mount_Values;
    Set_M3_Values;
    Set_Focuser_Values;
    Set_Rotator_Values;
    return JS.Write (Info);
  end Information;


  function Call_Back (Data : AWS.Status.Data) return AWS.Response.Data is
  begin
    Log.Write ("Callback - URI: " & AWS.Status.URI (Data));
    declare
      Uri       : constant String := AWS.Status.URI (Data);
      Parts     : constant Text.Strings := Text.Strings_Of (Uri, Separator => '/');
      Subsystem : constant String := Parts(1);
    begin
      if Subsystem in "mount" | "m3" then
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
          Log.Error ("Unknown Command: " & Command_Image);
          Input.Put (Input.End_Command, From => Input.Server);
          return AWS.Response.Acknowledge (AWS.Messages.S400, "unknown command");
        end;
      elsif Subsystem = "focuser" then
        if Parts(2) /= "set" then
          raise Program_Error;
        end if;
        declare
          Value_Image : constant String := AWS.Status.Parameter (Data, "position");
        begin
          if Value_Image = "" then
            return AWS.Response.Acknowledge (AWS.Messages.S400, "no position parameter");
          end if;
          Log.Write ("Focuser.Set_Position: " & Value_Image);
          Protected_Focuser.Data.Set_Position (Microns'value(Value_Image));
          return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
        end;
      elsif Subsystem = "rotator" then
        declare
          Command_Image : constant String := Parts(2);
        begin
          if Command_Image = "goto_field" then
            declare
              Value_Image : constant String := AWS.Status.Parameter (Data, "angle");
            begin
              if Value_Image = "" then
                return AWS.Response.Acknowledge (AWS.Messages.S400, "no field angle parameter");
              end if;
              Log.Write ("Rotator.Goto_Field_Angle: " & Value_Image);
              Protected_Rotator.Data.Goto_Field_Angle (Degrees'value(Value_Image));
              return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
            end;
          elsif Command_Image = "goto_mech" then
            declare
              Value_Image : constant String := AWS.Status.Parameter (Data, "position");
            begin
              if Value_Image = "" then
                return AWS.Response.Acknowledge (AWS.Messages.S400, "no mech position parameter");
              end if;
              Log.Write ("Rotator.Goto_Mech_Position: " & Value_Image);
              Protected_Rotator.Data.Goto_Mech_Position (Degrees'value(Value_Image));
              return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
            end;
          elsif Command_Image = "goto" then
            declare
              Value_Image : constant String := AWS.Status.Parameter (Data, "offset");
            begin
              if Value_Image = "" then
                return AWS.Response.Acknowledge (AWS.Messages.S400, "no offset parameter");
              end if;
              Log.Write ("Rotator.Goto_Offset: " & Value_Image);
              Protected_Rotator.Data.Goto_Offset (Degrees'value(Value_Image));
              return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
            end;
          else
            raise Program_Error;
          end if;
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
  end Call_Back;


  The_Server : AWS.Server.HTTP;

  procedure Start  is
  begin
    if Text.Is_Null (The_Client_Filename) then
      Log.Warning ("No GUI client");
    else
      declare
        Client_Filename : constant String := +The_Client_Filename;
      begin
        Log.Write ("Start " & Client_Filename);
        AWS.Server.Start (Web_Server => The_Server,
                          Name       => "Skytracker",
                          Callback   => Call_Back'access,
                          Port       => Natural(The_Server_Port));
        Os.Process.Create (Client_Filename);
      exception
      when others =>
        Log.Error ("GUI client not started");
      end;
    end if;
  end Start;


  procedure Set (Data : Control_Data) renames Protected_Control.Set;

  procedure Set_State (Image : String) is
  begin
    Protected_Mount_State.Set ([Image]);
  end Set_State;

  procedure Set_Moving (Speed : Angle.Value) renames Protected_Moving_Speed.Set;

  procedure Set (Data : Mount_Data) renames Protected_Mount.Set;

  procedure Set (Position : Mirror_Position) renames Protected_Mirror_Position.Set;

  procedure Set (Data : Focuser_Data) renames Protected_Focuser.Set;

  procedure Set (Data : Rotator_Data) renames Protected_Rotator.Set;


  procedure Shutdown is
  begin
    if not Text.Is_Null (The_Client_Filename) then
      Log.Write ("Shutdown");
      AWS.Server.Shutdown (The_Server);
    end if;
  exception
  when others =>
    null;
  end Shutdown;

end Http_Server;
