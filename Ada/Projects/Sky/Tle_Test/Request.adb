-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Command_Line;
with Ada.Text_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Exceptions;

package body Request is

  procedure Handle (Target : String) is

    Url : constant String := "https://celestrak.org/NORAD/elements/gp.php?GROUP=" & Target & "&FORMAT=TLE";

  begin -- Result_Of_Get_With
    Ada.Text_IO.Put_Line ("URL<<<" & Url & ">>>");
    declare
      Response : constant AWS.Response.Data := AWS.Client.Get (Url);
      Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);
      use type AWS.Messages.Status_Code;
    begin
      Ada.Text_IO.Put_Line ("=> " & Status'image);
      if Status = AWS.Messages.S200 then
        declare
          Result : constant String := AWS.Response.Message_Body (Response);

          Last  : Natural := Result'first;
          Count : Natural := 0;

          function Next_Line return String is
            First : constant Natural := Last;
          begin
            while Last <= Result'last and then not (Result(Last) in Ascii.Cr | Ascii.Lf) loop
              Last := @ + 1;
            end loop;
            return Dummy : constant String := Result(First .. Last - 1) do
              Last := @ + 1;
              if Last <= Result'last and then Result(Last) in Ascii.Cr | Ascii.Lf then
                Last := @ + 1;
              end if;
            end return;
          end Next_Line;

        begin
          Ada.Text_IO.Put_Line ("TLE DATA");
          Ada.Text_IO.Put_Line ("========");
          while Last <= Result'last loop
            declare
              Name : constant String := Next_Line;
              Tle1 : constant String := Next_Line;
              Tle2 : constant String := Next_Line;
            begin
              Ada.Text_IO.Put_Line ("Name: " & Name);
              Ada.Text_IO.Put_Line ("Tle1: " & Tle1);
              Ada.Text_IO.Put_Line ("Tle2: " & Tle2);
              Count := @ + 1;
            end;
          end loop;
          Ada.Text_IO.Put_Line ("=>" & Count'image & " records");
        end;
      end if;
    end;
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line (Exceptions.Information_Of (Occurrence));
  end Handle;


  procedure Work is
  begin
    if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line ("*** expected parameters: <group name> ***");
    elsif Ada.Command_Line.Argument_Count > 1 then
      Ada.Text_IO.Put_Line ("*** too many parameters ***");
      return;
    end if;
    Handle (Ada.Command_Line.Argument(1));
  end Work;

end Request;
