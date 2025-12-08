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
with Text;

procedure Camera_Test is

  package IO renames Ada.Text_IO;

  procedure Show_Grid is
    Grid : constant Camera.Green_Grid := Camera.Captured;
  begin
    for Row in Grid'range(1) loop
      declare
        Row_Image : constant String := "  " & Row'image;
      begin
        IO.Put (Row_Image(Row_Image'last - 3 .. Row_Image'last) & ":");
        for Column in Grid'range(2) loop
          declare
            Column_Image : constant String := "     " & Grid(Row, Column)'image;
          begin
            IO.Put (Column_Image(Column_Image'last - 5 .. Column_Image'last));
          end;
        end loop;
        IO.New_Line;
      end;
    end loop;
  end Show_Grid;

  Delay_Time : constant Duration := 0.2;
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
        IO.Put ("cmd> ");
        declare
          Parameters : constant Text.Strings := Text.Strings_Of (IO.Get_Line, Separator => ' ');
        begin
          if Parameters.Count = 0 then
            exit;
          elsif Parameters.Count /= 2 then
            raise Constraint_Error;
          end if;
          declare
            Tv : constant Exposure.Item := Exposure.Value(Parameters(2));
          begin
            case Text.Uppercase_Of (Parameters(1)(1)) is
            when 'C' =>
              Camera.Capture ("D:\Temp\Picture.CR2", Tv, Sensitivity.Value(125));
            when 'G' =>
              Camera.Capture (10, Tv);
            when others =>
              raise Constraint_Error;
            end case;
          end;
        exception
        when others =>
          IO.Put_Line ("### Illegal Command (expexted: ['C' | 'G'] <exposure>]) ###");
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
        Show_Grid;
        IO.Put_Line ("Image Height:" & Camera.Image_Height'image);
        IO.Put_Line ("Image Width :" & Camera.Image_Width'image);
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
  IO.Put_Line ("Stopped");

exception
when Item: others =>
  IO.Put_Line (Exceptions.Information_Of (Item));
end Camera_Test;
