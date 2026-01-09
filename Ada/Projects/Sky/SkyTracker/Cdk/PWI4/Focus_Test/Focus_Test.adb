-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_White_Elephant;

pragma Build (Description => "Focus test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Camera.Parameter;
with Exceptions;
with Exposure;
with Focus;
with Focuser.PWI4;
with Sensitivity;
with Text;

procedure Focus_Test is

  package IO renames Ada.Text_IO;

  Delay_Time : constant Duration := 0.5;

  procedure Error (Message : String) is
  begin
    IO.New_Line;
    IO.Put_Line ("### " & Message & " ###");
  end Error;

begin -- Camera_Test
  IO.Put_Line ("Focus Test");
  IO.Put_Line ("==========");
  Camera.Start;
  Focus.Start (Focuser.PWI4.New_Device);
  delay 1.0; -- wait for started;
  loop
    declare
      function State return Focus.Status is (Focus.Actual_State);
    begin
      case State is
      when Focus.No_Focuser =>
        Error ("No focuser connected");
        exit;
      when Focus.Undefined =>
        IO.New_Line;
        IO.Put ("cmd> ");
        declare
          Parameters : constant Text.Strings := Text.Strings_Of (IO.Get_Line, Separator => ' ');
        begin
          if Parameters.Count = 0 then
            exit;
          elsif Parameters.Count > 3 then
            raise Constraint_Error;
          end if;
          declare
            Command  : constant String        := Parameters(1);
            Exp_Time : constant Exposure.Item := (if Parameters.Count = 1
                                                  then Exposure.From_Camera
                                                  else Exposure.Value(Parameters(2)));
            Parameter : constant Sensitivity.Item := (if Parameters.Count in 1 | 2
                                                      then Sensitivity.Default
                                                      else Sensitivity.Value (Parameters(3)));
          begin
            Camera.Parameter.Define (Exp_Time, Parameter);
            case Text.Uppercase_Of (Command(Command'first)) is
            when 'F' =>
              Focus.Evaluate;
            when others =>
              raise Constraint_Error;
            end case;
          end;
        exception
        when others =>
          IO.Put_Line ("### Unknown - expexted: ['F' [<time> [ '[' <gain> ',' <offset> ']']] ###");
        end;
      when Focus.Positioning =>
        IO.Put ('p');
      when Focus.Capturing =>
        IO.Put ('c');
      when Focus.Evaluated =>
        IO.New_Line;
        declare
          Result : constant Focus.Result := Focus.Evaluation_Result;
        begin
          IO.Put ("Half Flux:" & Result.Half_Flux'image);
          IO.Put ("HFD      :" & Result.HFD'image);
          IO.Put ("Position :" & Result.Position'image);
        end;
      when Focus.Failed =>
        Error (Focus.Error_Message);
        exit;
      end case;
    end;
    delay Delay_Time;
  end loop;
  Focus.Finish;
  Camera.Finish;

exception
when Item: others =>
  IO.Put_Line (Exceptions.Information_Of (Item));
  Focus.Finish;
  Camera.Finish;
end Focus_Test;
