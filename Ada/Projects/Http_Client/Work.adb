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


  procedure Client (Url : String) is

    Result : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));

  begin -- Client
    Log.Write ("URL<<<" & Url & ">>>");
    Log.Write ("GOT<<<" & Result & ">>>");
    Put_Line (Result);
  end Client;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 1 then
      declare
        Url : constant String := Ada.Command_Line.Argument (1);
      begin
        Client (Url);
      end;
    else
      Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event : others =>
    Log.Write (Event);
    Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Work;
