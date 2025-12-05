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
  Delay_Time : constant Duration := 0.1;
  Timeout    : constant Duration := 10.0;
  Counter    : constant Natural := Natural(Timeout / Delay_Time);
  The_Count  : Integer;
begin
  IO.Put_Line ("Camera Test");
  IO.Put_Line ("===========");
  Camera.Start;
  IO.Put ("Started");
  loop
    declare
      Info : constant Camera.Information := Camera.Actual_Information;
    begin
      case Info.State is
      when Camera.Idle =>
        The_Count := Counter;
        IO.New_Line;
        IO.Put_Line ("Idle");
        IO.Put ("enter exposure> ");
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
              The_Count := Counter;
              Camera.Capture ("D:\Temp\Picture.CR2", Tv, Sensitivity.Value(125));
            end;
          exception
          when others =>
            IO.Put_Line ("Exposure """ & Command & """ not supported !!!");
          end;
        end;
      when Camera.Connected =>
        IO.Put_Line ("Connected " & Info.Camera'image);
      when Camera.Capturing | Camera.Captured =>
        IO.Put ("c");
      when Camera.Downloading =>
        IO.Put ("d");
      when Camera.Stopping =>
        IO.Put ("s");
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
  IO.Put_Line ("Stopped");

exception
when Item: others =>
  IO.Put_Line (Exceptions.Information_Of (Item));
end Camera_Test;
