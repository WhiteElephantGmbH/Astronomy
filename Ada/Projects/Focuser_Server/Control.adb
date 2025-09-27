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

with Celestron.Focuser;
with Handbox.Client;
with Server;
with Traces;

package body Control is

  package Log is new Traces ("Control");

  package Focuser renames Celestron.Focuser;

  task Manager is
    entry Start;
    entry Shutdown;
  end Manager;


  procedure Start is
  begin
    Focuser.Start;
    Handbox.Start (Handbox.Client.Handle'access);
    Server.Start;
    Manager.Start;
  end Start;


  procedure Shutdown is
  begin
    Manager.Shutdown;
  end Shutdown;


  task body Manager is
    The_Data : Focuser.Data;
  begin
    accept Start;
    Log.Write ("Manager started");
    loop
      The_Data.Exists := Focuser.Exists;
      if The_Data.Exists then
        The_Data.Position := Focuser.Position;
        The_Data.Moving := Focuser.Moving;
        The_Data.Home := Focuser.Home_Position;
        The_Data.Backlash := Focuser.Backlash;
        The_Data.Speed := Focuser.Speed;
      else
        The_Data := Focuser.No_Data;
      end if;
      Server.Update (The_Data);
      select
        accept Shutdown;
        exit;
      or
        delay 0.2;
      end select;
    end loop;
    Log.Write ("Manager terminating");
    Server.Shutdown;
    Handbox.Close;
    Focuser.Close;
    Log.Write ("Manager end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Manager;

end Control;
