-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

pragma Build (Description => "Camera test",
              Version     => (1, 0, 0, 4),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNAT\14.2");

with Ada.Text_IO;
with Camera;
with Exceptions;
with Exposure;
with Os.System;
with Sensitivity;
with Rt;
with Text;
with Traces;

procedure Camera_Test is

  package Log is new Traces ("Test");

  package IO renames Ada.Text_IO;

  procedure Show (Message : String) is
  begin
    Log.Write (Message);
    IO.Put_Line (Message);
  end Show;

  procedure Show_Grid is
    Grid : constant Camera.Raw_Grid := Camera.Captured;
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
  Timeout    : constant Duration := 15.0;
  Counter    : constant Natural := Natural(Timeout / Delay_Time);

  The_Count : Integer;

begin -- Camera_Test
  Show ("Camera Test");
  IO.Put_Line ("===========");
  Camera.Start;

  Camera.Detect_Model;
  if Camera.Available then
    IO.Put_Line ("Model: " & Camera.Model_Image);
  else
    Show ("No Camera Available");
    Camera.Finish;
    return;
  end if;

  Show ("Started");
  loop
     declare
      function Info return Camera.Information is (Camera.Actual_Information);
    begin
      case Info.State is
      when Camera.Idle =>
        The_Count := Counter;
        IO.New_Line;
        Show ("Idle");
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
            Log.Write ("Command : " & Command);
            Log.Write ("Exposure: " & Exp_Time'image);
            Log.Write ("Sensitivity: " & Parameter'image);
            case Text.Uppercase_Of (Command(Command'first)) is
            when 'C' =>
              Camera.Capture (Os.System.Temp_Path & "Picture", Exp_Time, Parameter);
            when 'G' =>
              Camera.Capture (10, Exp_Time, Parameter);
            when others =>
              Show ("### Unknown - expexted: [('C' | 'G') [<time> [<iso> | '[' <gain> ',' <offset> ']']]] ###");
            end case;
          end;
        exception
        when Item: others =>
          Show (Exceptions.Information_Of (Item));
        end;
      when Camera.Connecting =>
        IO.Put ("o");
      when Camera.Connected =>
        IO.New_Line;
        Show ("Connnected " & Info.Camera'image);
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
        Show ("Image Height:" & Info.Height'image);
        Show ("Image Width :" & Info.Width'image);
      when Camera.Stopping =>
        IO.Put ("s");
      when Camera.Failed =>
        IO.New_Line;
        Show ("### " & Camera.Error_Message & " ###");
      end case;
    end;
    Rt.Wait (Delay_Time);
    if The_Count > 0 then
      The_Count := @ - 1;
      if The_Count = 0 then
        IO.Put ("a");
        Camera.Stop;
      end if;
    end if;
  end loop;
  Camera.Finish;
  Show ("Stopped");

exception
when Item: others =>
  Show (Exceptions.Information_Of (Item));
end Camera_Test;
