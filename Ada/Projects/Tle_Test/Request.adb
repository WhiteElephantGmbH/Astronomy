-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Command_Line;
with Ada.Text_IO;
with AWS.Client;
with AWS.Response;
with Exceptions;
with Text;
with Time;

package body Request is

  The_Format : Text.String := ["TLE"];

  procedure Handle (Target : String) is

    Url : constant String := "https://celestrak.org/NORAD/elements/gp.php?GROUP=" & Target & "&FORMAT=" & The_Format.S;

  begin -- Result_Of_Get_With
    Ada.Text_IO.Put_Line ("URL<<<" & Url & ">>>");
    declare
      Data   : constant AWS.Response.Data := AWS.Client.Get (Url);
      Result : constant String := AWS.Response.Message_Body (Data);
    begin
      Ada.Text_IO.Put_Line ("GOT<<<" & Result & ">>>");
    end;
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line (Exceptions.Information_Of (Occurrence));
  end Handle;


  procedure Work is
  begin
    if Ada.Command_Line.Argument_Count = 1 then
      The_Format := [Text.Uppercase_Of (Ada.Command_Line.Argument(1))];
    elsif Ada.Command_Line.Argument_Count > 1 then
      Ada.Text_IO.Put_Line ("*** too many parameters ***");
      return;
    end if;
    if not (The_Format.S in "CSV" | "JSON" | "JSON-PRETTY" | "TLE" | "XML") then
      Ada.Text_IO.Put_Line ("*** format CSV, JSON, JSON-PRETTY, TLE or XML expected ***");
      return;
    end if;
    Handle ("IRIDIUM");
    Time.Wait (0.5);
    Handle ("STATIONS");
    Time.Wait (0.5);
    Handle ("VISUAL");
  end Work;

end Request;
