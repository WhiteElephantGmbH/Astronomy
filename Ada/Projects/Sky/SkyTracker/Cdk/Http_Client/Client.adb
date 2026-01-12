-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with AWS.Client;
with AWS.Response;
with Exceptions;
with Text;

package body Client is

  package IO renames Ada.Text_IO;

  procedure Execute is
  begin
    loop
      IO.Put (">");
      declare
        Data    : constant String := Text.Trimmed (IO.Get_Line);
        The_Url : Text.String := ["http://127.0.0.1:9000"];
        use type Text.String;
      begin
        exit when Data = "";
        case Data(Data'first) is
        when 'i' =>
          The_Url := The_Url & "/information";
        when 'l' =>
          The_Url := The_Url & "/mount/move_left";
        when 'r' =>
          The_Url := The_Url & "/mount/move_right";
        when 'u' =>
          The_Url := The_Url & "/mount/move_up";
        when 'd' =>
          The_Url := The_Url & "/mount/move_down";
        when 'e' =>
          The_Url := The_Url & "/mount/end_command";
        when 'n' =>
          The_Url := The_Url & "/mount/next_speed";
        when 'p' =>
          The_Url := The_Url & "/mount/previous_speed";
        when 'b' =>
          The_Url := The_Url & "/mount/back";
        when 's' =>
          The_Url := The_Url & "/mount/stop";
        when others =>
          The_Url := The_Url & Data;
        end case;
        declare
          Url    : constant String := The_Url.S;
        begin
          IO.Put_Line ("URL: <<<" & Url & ">>>");
          declare
            Result : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));
          begin
            IO.Put_Line ("Got: <<<" & Result & ">>>");
          end;
        end;
      end;
    end loop;
  exception
  when Event: others =>
    IO.Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Client;
