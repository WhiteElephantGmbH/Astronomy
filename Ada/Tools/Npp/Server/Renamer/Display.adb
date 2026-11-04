-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Log;
with Text;

package body Display is

  package Io  renames Ada.Text_IO;

  The_Error_Count : Natural := 0;

  procedure Show (Item  : String) is
  begin
    Io.Put_Line (Item);
  end Show;

  procedure Rename (From, To : String) is
  begin
    Show ("Renamed " & From & " to " & To);
    Log.Write ("Renamed " & From & " to " & To);
  end Rename;

  procedure Error (Message : String) is
  begin
    The_Error_Count := The_Error_Count + 1;
    Show ("### " & Message);
    Log.Write ("### " & Message);
  end Error;

  procedure Show_Result is
  begin
    if The_Error_Count = 0 then
      Show ("No errors found");
    else
      Show (Text.Trimmed (The_Error_Count'img) & " error" & (if The_Error_Count > 1 then "s" else "") & " found");
    end if;
  end Show_Result;

end Display;
