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

pragma Build (Description => "Half Flux Diameter Test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Camera;
with Exceptions;
with Exposure;
with HFD;
with Sensitivity;
with Text;

procedure HFD_Test is

  package IO renames Ada.Text_IO;

  Delay_Time : constant Duration := 0.2;
  Timeout    : constant Duration := 15.0;
  Counter    : constant Natural := Natural(Timeout / Delay_Time);

  The_Count : Integer;

begin -- Camera_Test
  IO.Put_Line ("HFD Test");
  IO.Put_Line ("========");
  Camera.Start;
  loop
    declare
      function Info return Camera.Information is (Camera.Actual_Information);
    begin
      case Info.State is
      when Camera.Idle =>
        The_Count := Counter;
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
            Time      : constant Exposure.Item := Exposure.Value(Parameters(2));
            Parameter : constant Sensitivity.Item := (if Parameters.Count = 2 then Sensitivity.Default
                                                                              else Sensitivity.Value (Parameters(3)));
          begin
            case Text.Uppercase_Of (Parameters(1)(1)) is
            when 'G' =>
              Camera.Capture (1000, Time, Parameter);
            when others =>
              raise Constraint_Error;
            end case;
          end;
        exception
        when others =>
          IO.Put_Line ("### Illegal Command (expexted: ['G' <time> [<iso> | '[' <gain> ',' <offset> ']']]) ###");
        end;
      when Camera.Connecting =>
        IO.Put ("o");
      when Camera.Connected =>
        IO.New_Line;
        IO.Put_Line ("Connnected " & Info.Camera'image);
      when Camera.Capturing =>
        IO.Put ("c");
      when Camera.Captured =>
        IO.Put ("C");
      when Camera.Downloading =>
        IO.Put ("d");
      when Camera.Cropping =>
        IO.Put ("r");
      when Camera.Cropped =>
        IO.New_Line;
        HFD.Evaluate (Camera.Captured);
      when Camera.Stopping =>
        IO.Put ("s");
      when Camera.Error =>
        IO.New_Line;
        IO.Put_Line ("### " & Camera.Error_Message & " ###");
      end case;
    end;
    delay Delay_Time;
    The_Count := @ - 1;
    if The_Count = 0 then
      IO.Put ("a");
      Camera.Stop;
    end if;
  end loop;
  Camera.Finish;

exception
when Item: others =>
  IO.Put_Line (Exceptions.Information_Of (Item));
end HFD_Test;
