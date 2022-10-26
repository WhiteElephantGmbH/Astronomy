-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with AWS.Client;
with AWS.Response;
with Log;

package body Work is

  procedure Put_Line (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put_Line;


  procedure Client (Ip : String) is

    Url : constant String := "http://user:secret@" & Ip & "/statusjsn.js?components=513&cmd=1&p=3&s=1";

    Result : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));

  begin -- Client
    Log.Write ("URL<<<" & Url & ">>>");
    Log.Write ("GOT<<<" & Result & ">>>");
    Put_Line (Result);
  end Client;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Client ("192.168.10.160");
    elsif Nr_Of_Arguments = 1 then
      Client (Ada.Command_Line.Argument (1));
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Log.Write (Event);
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Work;
