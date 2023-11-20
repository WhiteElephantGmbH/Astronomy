-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with AWS.Messages;
with AWS.Status;
with AWS.Server;
with AWS.Response;
with Traces;

package body Http_Server is

  package Log is new Traces ("Http_Server");


  function Call_Back (Data : AWS.Status.Data) return AWS.Response.Data is

    function Parameter_For (Name : String) return String is
      Value : constant String := AWS.Status.Parameter (Data, Name);
    begin
      Log.Write ("Callback - " & Name & ": " & Value);
      return Value;
    end Parameter_For;

  begin -- Call_Back
    Log.Write ("Callback - Host: " & AWS.Status.Host (Data));
    Log.Write ("Callback - Protocol: " & AWS.Status.Protocol (Data)'image);
    Log.Write ("Callback - Methode: " & AWS.Status.Method (Data));
    Log.Write ("Callback - URI: " & AWS.Status.URI (Data));
    Log.Write ("Callback - URL: " & AWS.Status.URL (Data));
    Log.Write ("Callback - User Agent: " & AWS.Status.User_Agent (Data));
    declare
      P1 : constant String := Parameter_For ("Direction");
      P2 : constant String := Parameter_For ("State");
    begin
      Log.Write ("Direction: " & P1 & " - State: " & P2);
      declare
        Reply : constant String := "Hello World";
      begin
        return AWS.Response.Acknowledge (AWS.Messages.S200, Reply);
      end;
    end;
  end Call_Back;


  The_Server : AWS.Server.HTTP;

  procedure Start is
  begin
    Log.Write ("Start");
    AWS.Server.Start (Web_Server => The_Server, Name => "Skytracker", Callback => Call_Back'access, Port => 4242);
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
