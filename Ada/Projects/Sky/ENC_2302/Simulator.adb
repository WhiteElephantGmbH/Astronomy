-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with AWS.Messages;
with AWS.Status;
with AWS.Server;
with AWS.Response;
with ENC_2302_Client;
with Exceptions;
with Log;
with Response;

package body Simulator is

  package ENC renames ENC_2302_Client;

  package IO renames Ada.Text_IO;

  The_Switches : ENC.Switches := ENC.All_Off;


  function Call_Back (Data : AWS.Status.Data) return AWS.Response.Data is

    function Parameter_For (Name : String) return String is
      Value : constant String := AWS.Status.Parameter (Data, Name);
    begin
      IO.Put_Line ("Callback - " & Name & ": " & Value);
      return Value;
    end Parameter_For;

  begin -- Call_Back
    IO.Put_Line ("Callback - Host: " & AWS.Status.Host (Data));
    IO.Put_Line ("Callback - Protocol: " & AWS.Status.Protocol (Data)'image);
    IO.Put_Line ("Callback - Methode: " & AWS.Status.Method (Data));
    IO.Put_Line ("Callback - URI: " & AWS.Status.URI (Data));
    IO.Put_Line ("Callback - URL: " & AWS.Status.URL (Data));
    IO.Put_Line ("Callback - User Agent: " & AWS.Status.User_Agent (Data));
    declare
      Components : constant String := Parameter_For ("components");
      Cmd        : constant String := Parameter_For ("cmd") with Unreferenced;
      P          : constant String := Parameter_For ("p");
      S          : constant String := Parameter_For ("s");
    begin
      if P /= "" and S /= "" then
        The_Switches(ENC.Port'val(Integer'value(P) - 1)) := ENC.Switch'val(Integer'value(S));
      end if;
      declare
        Reply : constant String := Response.Item (The_Switches, Components);
      begin
        IO.Put_Line ("<<<" & Reply & ">>>");
        return AWS.Response.Acknowledge (AWS.Messages.S200, Reply);
      end;
    end;
  end Call_Back;


  procedure Server is
    Http : AWS.Server.HTTP;
  begin
    AWS.Server.Start (Web_Server => Http, Name => "ENC 2302", Callback => Call_Back'access, Port => 80);
    AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
  exception
  when Item: others =>
    Log.Write (Item);
  end Server;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      IO.Put_Line ("ENC 2302 Simulator.");
      Server;
    else
      IO.Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    IO.Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Simulator;
