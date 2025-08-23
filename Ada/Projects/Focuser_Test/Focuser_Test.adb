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

pragma Build (Description => "Focuser test",
              Version     => (1, 0, 0, 2),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64", "COLL64"),
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Celestron.Focuser;
with Focuser_Client;
with Exceptions;
with Handbox;

procedure Focuser_Test is

  package Focuser renames Celestron.Focuser;

  package IO renames Ada.Text_IO;

  The_Data : Focuser.Data;

  Is_Executing : Boolean := True;

  procedure Execute (The_Command: Handbox.Command) is
    use all type Focuser.Command;
  begin
    case The_Command is
    when Handbox.Up_Pressed =>
      The_Data := Focuser_Client.Execute (Increase_Rate);
    when Handbox.Down_Pressed =>
      The_Data := Focuser_Client.Execute (Decrease_Rate);
    when Handbox.Left_Pressed =>
      The_Data := Focuser_Client.Execute (Move_In);
    when Handbox.Right_Pressed =>
      The_Data := Focuser_Client.Execute (Move_Out);
    when Handbox.Up_Released =>
      null;
    when Handbox.Down_Released =>
      null;
    when Handbox.Left_Released | Handbox.Right_Released =>
      The_Data := Focuser_Client.Execute (Stop);
    when Handbox.Center_Pressed | Handbox.Stop =>
      The_Data := Focuser_Client.Execute (Stop);
    when Handbox.Center_Released =>
      Is_Executing := False;
    end case;
  end Execute;

  Has_Moved        : Boolean := False;
  Was_Disconnected : Boolean := False;

begin
  IO.Put_Line ("Focuser Test");
  IO.Put_Line ("============");
  Handbox.Start (Execute'access);
  while Is_Executing loop
    delay 1.0;
    The_Data := Focuser_Client.Actual_Data;
    IO.Put_Line ("Rate:" & The_Data.Speed'image &
                 " - Backlash:" & The_Data.Backlash'image &
                 " - Position:" & The_Data.Position'image);
  end loop;
  loop
    if The_Data.Exists then
      if The_Data.Moving then
        IO.Put_Line ("Focuser Moving");
        Has_Moved := True;
      else
        IO.Put_Line ("Focuser Stopped at " & The_Data.Position'image);
        if Has_Moved then
          exit when Was_Disconnected;
        else
          if Was_Disconnected then
            The_Data := Focuser_Client.Move_To (10000);
          else
            The_Data := Focuser_Client.Move_To (9000);
          end if;
        end if;
      end if;
    else
      Has_Moved := False;
      Was_Disconnected := True;
      IO.Put_Line ("Focuser Disconnected");
    end if;
    delay 1.0;
    The_Data := Focuser_Client.Actual_Data;
  end loop;
  IO.Put_Line ("Close Handbox");
  Handbox.Close;
  IO.Put_Line ("Complete");
exception
when Item: others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
  Handbox.Close;
end Focuser_Test;
