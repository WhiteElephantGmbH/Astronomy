-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Exceptions;
with AWS.Client;
with AWS.Response;
--with GNATCOLL.JSON;
--with Strings;

package body Client is

  package IO renames Ada.Text_IO;

  procedure Execute is
  begin
    loop
      IO.Put (">");
      declare
        Data    : constant String := IO.Get_Line;
        Url     : constant String := "http://127.0.0.1:4242/key?" & Data;
        Result  : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));
        --Data    : constant JS.JSON_Value := JS.Read (Result);
        --Outputs : constant JS.JSON_Value := Data.Get ("outputs");
      begin
        exit when Data = "";
        IO.Put_Line ("URL: <<<" & Url & ">>>");
        IO.Put_Line ("Got: <<<" & Result & ">>>");
      end;
    end loop;
  exception
  when Event: others =>
    IO.Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Client;
