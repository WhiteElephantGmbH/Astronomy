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
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Celestron.Focuser;
with Exceptions;
with Handbox;

procedure Focuser_Test is

  package Focuser renames Celestron.Focuser;

  Is_Executing : Boolean := True;

  procedure Execute (The_Command: Handbox.Command) is
  begin
    case The_Command is
    when Handbox.Up_Pressed =>
      Focuser.Execute (Focuser.Increase_Rate);
    when Handbox.Down_Pressed =>
      Focuser.Execute (Focuser.Decrease_Rate);
    when Handbox.Left_Pressed =>
      Focuser.Execute (Focuser.Move_In);
    when Handbox.Right_Pressed =>
      Focuser.Execute (Focuser.Move_Out);
    when Handbox.Up_Released =>
      null;
    when Handbox.Down_Released =>
      null;
    when Handbox.Left_Released | Handbox.Right_Released =>
      Focuser.Execute (Focuser.Stop);
    when Handbox.Center_Pressed | Handbox.Stop =>
      Focuser.Execute (Focuser.Stop);
    when Handbox.Center_Released =>
      Is_Executing := False;
    end case;
  end Execute;

  package TIO renames Ada.Text_IO;

  Has_Moved        : Boolean := False;
  Was_Disconnected : Boolean := False;

begin
  TIO.Put_Line ("Focuser Test");
  TIO.Put_Line ("============");
  Focuser.Start;
  Handbox.Start (Execute'access);
  while Is_Executing loop
    delay 1.0;
    TIO.Put_Line ("Rate:" & Focuser.Speed'image &
                  " - Backlash:" & Focuser.Backlash'image &
                  " - Position:" & Focuser.Position'image);
  end loop;
  loop
    if Focuser.Exists then
      if Focuser.Moving then
        TIO.Put_Line ("Focuser Moving");
        Has_Moved := True;
      else
        TIO.Put_Line ("Focuser Stopped at " & Focuser.Position'image);
        if Has_Moved then
          exit when Was_Disconnected;
        else
          if Was_Disconnected then
            Focuser.Move_To (10000);
          else
            Focuser.Move_To (9000);
          end if;
        end if;
      end if;
    else
      Has_Moved := False;
      Was_Disconnected := True;
      TIO.Put_Line ("Focuser Disconnected");
    end if;
    delay 1.0;
  end loop;
  TIO.Put_Line ("Close Handbox");
  Handbox.Close;
  TIO.Put_Line ("Close Focuser");
  Focuser.Close;
  TIO.Put_Line ("Complete");
exception
when Item: others =>
  TIO.Put_Line ("Exception: " & Exceptions.Name_Of (Item));
  Handbox.Close;
  Focuser.Close;
end Focuser_Test;
