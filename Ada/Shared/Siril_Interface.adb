-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Os.Pipe;
with Os.Process;
with Text;
with Traces;

package body Siril_Interface is

  package Log is new Traces ("Siril_Interface");

  Cli_Application : constant String := "C:\Program Files\SiriL\bin\siril-cli.exe";

  Command_Pipe_Name  : constant String := "siril_command.in";
  Response_Pipe_Name : constant String := "siril_command.out";

  Max_Pipe_Length : constant := 65536;
  Startup_Timeout : constant Os.Pipe.Timer := 10.0;

  Command_Pipe  : Os.Pipe.Handle;
  Response_Pipe : Os.Pipe.Handle;

  Pipe_Buffer : String (1 .. Max_Pipe_Length);

  Is_Open : Boolean := False;

  The_Progress_Handler : Progress_Handler;


  procedure Send (Command : String) is
    Item : constant String := Command & Ascii.Lf;
  begin
    Log.Write ("> " & Command);
    Os.Pipe.Write (To_Pipe => Command_Pipe,
                   Data    => Item'address,
                   Length  => Item'length);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise Fatal_Error;
  end Send;


  procedure Open (Progress: Progress_Handler := null) is
  begin
    if Is_Open then
      raise Fatal_Error with "Already open";
    end if;
    begin
      The_Progress_Handler := Progress;
      Os.Process.Create (Executable => Cli_Application,
                         Parameters => "-p",
                         Console    => Os.Process.Invisible);
      Os.Pipe.Open (The_Pipe  => Response_Pipe,
                    Name      => Response_Pipe_Name,
                    Kind      => Os.Pipe.Client,
                    Mode      => Os.Pipe.Outbound,
                    Size      => Max_Pipe_Length,
                    Wait_Time => Startup_Timeout);
      Os.Pipe.Open (The_Pipe  => Command_Pipe,
                    Name      => Command_Pipe_Name,
                    Kind      => Os.Pipe.Client,
                    Mode      => Os.Pipe.Inbound,
                    Size      => Max_Pipe_Length,
                    Wait_Time => Startup_Timeout);
      Is_Open := True;
    exception
    when Occurrence: others =>
      Log.Termination (Occurrence);
      raise Resource_Error;
    end;
  end Open;


  procedure Execute (Command : String) is

    procedure Raise_Protocol_Error (Message : String) with No_Return is
    begin
      Log.Error (Message);
      raise Protocol_Error with Message;
    end Raise_Protocol_Error;


    procedure Wait_For_Status is

      The_Length : Natural;

      type State is (Waiting, Started);

      The_State   : State := Waiting;
      The_Command : Text.String;

      function Handle_Response_Complete return Boolean is

        Response : constant String := Pipe_Buffer (1 .. The_Length - 1); -- removed CR

        function Progress_Of (Image : String) return Percent is
          Finish : constant Natural := Text.Location_Of ("%", Image);
        begin
          if Finish = Text.Not_Found then
            raise Protocol_Error with "Invalid progress response: " & Response;
          end if;
          declare
            Value : constant Percent := Percent'value (Response (Image'first .. Finish - 1));
          begin
            if Value = Percent'first then
              return Percent'first + Percent'delta;
            elsif Value = Percent'last then
              return Percent'last - Percent'delta;
            end if;
            return Value;
          end;
        exception
        when Constraint_Error =>
          raise Protocol_Error with "Invalid progress value: " & Response;
        end Progress_Of;

        procedure Progress_Handling (Value : Percent) is
        begin
          if The_Progress_Handler /= null then
            The_Progress_Handler (The_Command.S, Value);
          end if;
        end Progress_Handling;

        Tokens : constant Text.Strings := Text.Strings_Of (Response, Separator => ' ');

      begin -- Handle_Response
        if Response = "ready" then
          Log.Write ("< " & Response);
          return False;
        elsif Tokens.Count > 1 then
          declare
            Keyword : constant String := Tokens(1);
            Value   : constant String := Tokens(2);
          begin
            if Keyword = "status:" then
              declare
                Subject : constant String := Tokens(3);
              begin
                if Value = "success" then
                  if The_Command.Matches (Subject) then
                    Progress_Handling (Percent'last);
                    The_State := Waiting;
                    Log.Write ("< " & Response);
                    return True;
                  else
                    Raise_Protocol_Error ("not started: " & Response);
                  end if;
                elsif Value in "starting" then
                  if The_State = Waiting then
                    The_Command := [Subject];
                    Progress_Handling (Percent'first);
                    The_State := Started;
                  else
                    Raise_Protocol_Error ("already started: " & Response);
                  end if;
                  Log.Write ("< " & Response);
                  return False;
                end if;
              end;
            elsif Keyword = "log:" then
              Log.Write ("< " & Response);
              return False;
            elsif Keyword = "progress:" then
              if The_State = Started then
                Progress_Handling (Progress_Of (Value));
              else
                Log.Warning ("Command is not started: " & Response);
              end if;
              return False;
            end if;
          end;
        end if;
        Raise_Protocol_Error (Response);
      end Handle_Response_Complete;

    begin -- Wait_For_Status
      loop
        Os.Pipe.Read (From_Pipe => Response_Pipe,
                      Data      => Pipe_Buffer'address,
                      Length    => The_Length);
        exit when Handle_Response_Complete;
      end loop;
    end Wait_For_Status;

  begin -- Execute
    if not Is_Open then
      raise Fatal_Error with "not open";
    end if;
    Send (Command);
    Wait_For_Status;
  exception
  when Fatal_Error | Protocol_Error =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise Fatal_Error;
  end Execute;


  procedure Close is
  begin
    if not Is_Open then
      raise Fatal_Error with "not open";
    end if;
    Send ("close");
    Send ("exit");
    Os.Pipe.Close (Command_Pipe);
    Os.Pipe.Close (Response_Pipe);
    Is_Open := False;
  exception
  when Fatal_Error =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    raise Fatal_Error;
  end Close;

end Siril_Interface;
