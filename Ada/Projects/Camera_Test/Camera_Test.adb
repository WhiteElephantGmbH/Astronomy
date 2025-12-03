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

pragma Build (Description => "GID test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Camera;
with Exceptions;
with Exposure;
with Sensitivity;

procedure Camera_Test is
  package IO renames Ada.Text_IO;
begin
  IO.Put_Line ("Camera Test");
  IO.Put_Line ("===========");
  Camera.Start;
  IO.Put_Line ("Started");
  loop
    declare
      Info : constant Camera.Information := Camera.Actual_Information;
    begin
      case Info.State is
      when Camera.Connected =>
        IO.Put ("Connected " & Info.Camera'image & " - enter exposure> ");
        declare
          Command : constant String := IO.Get_Line;
        begin
          if Command = "" then
            exit;
          end if;
          begin
            declare
              Tv : constant Exposure.Item := Exposure.Value(Command);
            begin
              Camera.Capture ("D:\Temp\Picture.CR2", Tv, Sensitivity.Value(12800));
              IO.Put ("Capturing.");
            end;
          exception
          when others =>
            IO.Put_Line ("Exposure """ & Command & """ not supported !!!");
          end;
        end;
      when Camera.Capturing =>
        IO.Put (".");
      when Camera.Captured =>
        IO.New_Line;
        IO.Put_Line ("Captured");
      when Camera.Disconnected =>
        IO.Put_Line ("Disconnected");
        delay 1.0;
      end case;
    end;
    delay 0.3;
  end loop;
  Camera.Finish;
  IO.Put_Line ("Stopped");

exception
when Item: others =>
  IO.Put_Line (Exceptions.Information_Of (Item));
end Camera_Test;
