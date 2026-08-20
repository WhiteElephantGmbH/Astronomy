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

pragma Build (Description => "Time Server Test",
              Version     => (1, 0, 0, 1),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS", "GNATCOLL"),
              Compiler    => "GNAT\14.2");

with Ada.Text_IO;
with Exceptions;
with Time_Client;
with Time.Server;

procedure Time_Server_Test is

  package IO renames Ada.Text_IO;

  The_Information : Time.Server.Information;

begin
  IO.Put_Line ("Time Server Test");
  IO.Put_Line ("================");
  loop
    The_Information := Time_Client.Actual_Information;
    IO.Put_Line ("Information:" & The_Information'image);
    IO.Put_Line ("Date Time " & Time.Image_Of (Time.Ut_Of (The_Information.Clock_Time)));
    if The_Information.Clock.Is_Synchronized then
      if not Time_Client.Synchronize_Mount then
        IO.Put_Line ("Synchronize Mount Failed!!!");
      end if;
    end if;
    if The_Information.Mount.Is_Synchronized then
      if not Time_Client.Set (Time.Julian_Date) then
        IO.Put_Line ("Set Date Time Failed!!!");
      end if;
    end if;
    exit when The_Information.Clock.Is_Set_From_Pc and The_Information.Mount.Is_Synchronized;
    Time.Wait (3.0);
  end loop;
  Time_Client.Shutdown;
exception
when Time_Client.Server_Not_Available =>
  IO.Put_Line ("Time Server not avalable");
when Item: others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Time_Server_Test;
