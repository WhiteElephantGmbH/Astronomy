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
with Device;
with Strings;
with Telescope;
with Traces;
with User.Input;

package body Http_Server is

  package Log is new Traces ("Http_Server");

  Arc_Minutes_Delta : constant := 0.001;

  type Arc_Minutes is delta Arc_Minutes_Delta range 0.0 .. 1000.0 - Arc_Minutes_Delta with Small => Arc_Minutes_Delta;


  function Image_Of (Item : Angle.Value) return String is
    use type Angle.Degrees;
    use type Angle.Value;
  begin
    return Strings.Trimmed (Arc_Minutes(Angle.Degrees'(+Item) * 60.0)'image & "'/s");
  end Image_Of;


  function Information return String is
    Data : constant Telescope.Data := Telescope.Information;
  begin
    return "Mount.Moving_Speed=" & Image_Of (Data.Moving_Speed);
  end Information;


  function Call_Back (Data : AWS.Status.Data) return AWS.Response.Data is
  begin -- Call_Back
    Log.Write ("Callback - URI: " & AWS.Status.URI (Data));
    declare
      Uri       : constant String := AWS.Status.URI (Data);
      Parts     : constant Strings.Item := Strings.Item_Of (Uri, Separator => '/');
      Subsystem : constant String := Parts(1);
    begin
      Log.Write ("Subsystem: " & Subsystem);
      if Subsystem = "mount" then
        declare
          Command_Image : constant String := Parts(2);
        begin
          declare
            Command : constant Device.Command := Device.Command'value(Parts(2));
          begin
            User.Input.Put (Command, From => User.Input.Server);
            return AWS.Response.Acknowledge (AWS.Messages.S200, "ok");
          end;
        exception
        when others =>
          Log.Error ("Unknown Command: " & Command_Image);
          User.Input.Put (Device.End_Command, From => User.Input.Server);
          return AWS.Response.Acknowledge (AWS.Messages.S400, "Unknown Command");
        end;
      elsif Subsystem = "information" then
        return AWS.Response.Acknowledge (AWS.Messages.S200, Information);
      else
        return AWS.Response.Acknowledge (AWS.Messages.S400, "Unknown Subsystem");
      end if;
    end;
  exception
  when others =>
    return AWS.Response.Acknowledge (AWS.Messages.S400, "Unknown URI");
  end Call_Back;


  The_Server : AWS.Server.HTTP;

  procedure Start is
  begin
    Log.Write ("Start");
    AWS.Server.Start (Web_Server => The_Server, Name => "Skytracker", Callback => Call_Back'access, Port => 9000);
    Log.Write ("Start complete");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Start;


  procedure Shutdown is
  begin
    Log.Write ("Shutdown");
    AWS.Server.Shutdown (The_Server);
    Log.Write ("Shutdown complete");
  end Shutdown;

end Http_Server;
