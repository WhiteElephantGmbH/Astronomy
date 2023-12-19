-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;
with AWS.Messages;
with AWS.Status;
with AWS.Server;
with AWS.Response;
with Cdk_700;
with GNATCOLL.JSON;
with Device;
with Lexicon;
with Parameter;
with Strings;
with Telescope;
with Traces;
with User.Input;

package body Http_Server is

  package Log is new Traces ("Http_Server");

  package JS renames GNATCOLL.JSON;


  function Image_Of (Item : Angle.Value) return String is

    Arc_Delta : constant := 0.01;
    type Arc is delta Arc_Delta range 0.0 .. 100.0 - Arc_Delta with Small => Arc_Delta;

    use type Angle.Degrees;
    use type Angle.Value;

    Speed : Angle.Degrees := +Item;

  begin
    if Speed >= 1.0 then
      return Strings.Trimmed (Arc(Speed)'image & Angle.Degree & "/s");
    else
      Speed := @ * 60.0;
      if Speed >= 1.0 then
        return Strings.Trimmed (Arc(Speed)'image & "'/s");
      else
        return Strings.Trimmed (Natural(Speed * 60.0)'image & """/s");
      end if;
    end if;
  end Image_Of;


  function Image_Of (Item : Device.M3.Position) return String is
    use all type Device.M3.Position;
  begin
    case Item is
    when Unknown =>
      return "";
    when Between =>
      return "";
    when Ocular =>
      return Lexicon.Image_Of (Lexicon.Ocular);
    when Camera =>
      return Lexicon.Image_Of (Lexicon.Camera);
    end case;
  end Image_Of;


  At_Camera_Simulation : Boolean := False;

  function Information return String is

    Is_Simulation : constant Boolean := Cdk_700.Is_Simulated;

    Data : constant Telescope.Data := Telescope.Information;
    Info : constant JS.JSON_Value := JS.Create_Object;

    Mount_Exists : constant Boolean := not (Data.Status in Telescope.Startup_State);
    At_Camera    : Boolean;

  begin
    declare
      Mount  : constant JS.JSON_Value := JS.Create_Object;
      Exists : constant JS.JSON_Value := JS.Create (Mount_Exists);
      Speed  : constant JS.JSON_Value := JS.Create (Image_Of (Data.Moving_Speed));
    begin
      JS.Set_Field (Mount, "exists", Exists);
      JS.Set_Field (Mount, "speed", Speed);
      JS.Set_Field (Info, "mount", Mount);
      Log.Write ("Mount Exists: " & Mount_Exists'image);
    end;
    declare
      use type Device.M3.Position;
      M3        : constant JS.JSON_Value := JS.Create_Object;
      Exists    : Boolean;
      Position  : Device.M3.Position;
    begin
      if Is_Simulation then
        Exists := Mount_Exists;
        At_Camera := At_Camera_Simulation;
        Position := (if At_Camera then Device.M3.Camera else Device.M3.Ocular);
      else
        Exists := Data.M3.Exists;
        At_Camera := Data.M3.Position = Device.M3.Camera;
        Position := Data.M3.Position;
      end if;
      JS.Set_Field (M3, "exists", JS.Create (Exists));
      JS.Set_Field (M3, "at_camera", JS.Create (At_Camera));
      JS.Set_Field (M3, "position", JS.Create (Image_Of (Position)));
      JS.Set_Field (Info, "m3", M3);
      Log.Write ("M3 Exists: " & Exists'image);
      Log.Write ("M3 At_Camera: " & At_Camera'image);
    end;
    declare
      Focuser   : constant JS.JSON_Value := JS.Create_Object;
      Connected : constant Boolean := Data.Focuser.Connected;
      Position  : constant Integer := Integer(Data.Focuser.Position);
      Moving    : constant Boolean := Data.Focuser.Moving;
      Exists   : Boolean;
    begin
      if Is_Simulation then
        Exists := True;
      else
        Exists := Data.Focuser.Exists;
      end if;
      JS.Set_Field (Focuser, "exists", JS.Create (Exists));
      JS.Set_Field (Focuser, "connected", JS.Create (Connected));
      JS.Set_Field (Focuser, "moving", JS.Create (Moving));
      JS.Set_Field (Focuser, "position", JS.Create (Position));
      JS.Set_Field (Info, "focuser", Focuser);
      Log.Write ("Focuser Exists: " & Exists'image);
      Log.Write ("Focuser Connected: " & Connected'image);
      Log.Write ("Focuser Moving: " & Moving'image);
      Log.Write ("Focuser Position:" & Position'image);
    end;
    return JS.Write (Info);
  end Information;


  function Call_Back (Data : AWS.Status.Data) return AWS.Response.Data is
  begin
    Log.Write ("Callback - URI: " & AWS.Status.URI (Data));
    declare
      Uri       : constant String := AWS.Status.URI (Data);
      Parts     : constant Strings.Item := Strings.Item_Of (Uri, Separator => '/');
      Subsystem : constant String := Parts(1);
    begin
      Log.Write ("Subsystem: " & Subsystem);
      if Subsystem in "mount" | "m3" then
        declare
          Command_Image : constant String := Parts(2);
        begin
          declare
            Command : constant Device.Command := Device.Command'value(Parts(2));
            use type Device.Command;
          begin
            User.Input.Put (Command, From => User.Input.Server);
            if Command = Device.Rotate then
              At_Camera_Simulation := not At_Camera_Simulation;
            end if;
            return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
          end;
        exception
        when others =>
          Log.Error ("Unknown Command: " & Command_Image);
          User.Input.Put (Device.End_Command, From => User.Input.Server);
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
          Telescope.Focuser_Goto (Device.Microns'value(Value_Image));
          return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
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
    Log.Write ("Start");
    AWS.Server.Start (Web_Server => The_Server,
                      Name       => "Skytracker",
                      Callback   => Call_Back'access,
                      Port       => Natural(Parameter.Server_Port));
  end Start;


  procedure Shutdown is
  begin
    Log.Write ("Shutdown");
    AWS.Server.Shutdown (The_Server);
  exception
  when others =>
    null;
  end Shutdown;

end Http_Server;
