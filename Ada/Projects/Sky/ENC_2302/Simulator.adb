-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Log;
with AWS.Messages;
with AWS.Parameters;
with AWS.Status;
with AWS.Server;
with AWS.Response;
with Response;

package body Simulator is

  procedure Put_Line (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put_Line;


  function Call_Back (Data : AWS.Status.Data) return AWS.Response.Data is
    Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Data);
  begin
    Put_Line ("Callback - Methode: " & AWS.Status.Method (Data));
    Put_Line ("Callback - Parameters: " & AWS.Parameters.URI_Format (Parameters));
    return AWS.Response.Acknowledge (AWS.Messages.S200, Response.Item);
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
      Put_Line ("ENC 2302 Simulator.");
      Server;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Simulator;
