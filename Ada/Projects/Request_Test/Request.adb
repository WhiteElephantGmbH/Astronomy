-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with AWS.Client;
with AWS.Response;
with Traces;

package body Request is

  package Log is new Traces ("Request");

  procedure Send (Item : String) is

    The_Response : AWS.Response.Data;

  begin -- Send
    Ada.Text_IO.Put_Line ("Request: " & Item);
    Log.Write ("Send " & Item);
    The_Response := AWS.Client.Get ("http://217.160.64.198:5000/ZzRW8sYHdHrgZGG3?tele=apo&target=" & Item);
    Ada.Text_IO.Put_Line ("Response: " & AWS.Response.Message_Body (The_Response));
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Send;

end Request;
