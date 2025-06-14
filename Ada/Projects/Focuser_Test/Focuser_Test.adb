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
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Ada.Exceptions;
with Ada.Text_IO;
with Focuser;

procedure Focuser_Test is
  Has_Moved        : Boolean := False;
  Was_Disconnected : Boolean := False;
begin
  Ada.Text_IO.Put_Line ("Focuser Test");
  Ada.Text_IO.Put_Line ("============");
  Focuser.Start;
  loop
    Ada.Text_IO.Put(">");
    begin
      declare
        Command : constant String := Ada.Text_IO.Get_Line & "2";
        Speed   : constant Focuser.Speed := Focuser.Speed'value(Command(Command'first + 1 .. Command'first + 1));
      begin
        case Command(Command'first) is
        when 'i' =>
          Focuser.Move_In (Speed);
        when 'o' =>
          Focuser.Move_Out (Speed);
        when 's' =>
          Focuser.Stop;
        when 'e' =>
          Focuser.Close;
          return;
        when 'c' =>
          exit;
        when others =>
          raise Constraint_Error;
        end case;
      end;
    exception
    when others =>
      Ada.Text_IO.Put("expected: i{1..3}, o{1..3}, c or e");
    end;
  end loop;
  loop
    case Focuser.State is
    when Focuser.Disconnected =>
      Has_Moved := False;
      Was_Disconnected := True;
      Ada.Text_IO.Put_Line ("Focuser Disconnected");
    when Focuser.Moving =>
      Ada.Text_IO.Put_Line ("Focuser Moving");
      Has_Moved := True;
    when Focuser.Stopped =>
      Ada.Text_IO.Put_Line ("Focuser Stopped at " & Focuser.Position'image);
      if Has_Moved then
        exit when Was_Disconnected;
      else
        if Was_Disconnected then
          Focuser.Move (To => 10000);
        else
          Focuser.Move (To => 9000);
        end if;
      end if;
    end case;
    delay 2.0;
  end loop;
  Focuser.Close;
  Ada.Text_IO.Put_Line ("Complete");
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Item));
end Focuser_Test;
