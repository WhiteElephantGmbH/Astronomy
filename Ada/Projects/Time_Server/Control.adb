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

with Ada.Text_IO;
with Clock;
with Server;
with Ten_Micron;
with Traces;

package body Control is

  package Log is new Traces ("Control");

  task Manager is
    entry Start;
    entry Shutdown;
  end Manager;


  procedure Start is
  begin
    Manager.Start;
  end Start;


  procedure Shutdown is
  begin
    Manager.Shutdown;
  end Shutdown;


  task body Manager is
  begin
    accept Start;
    Log.Write ("Manager started");
    Server.Start;
    accept Shutdown;
    Log.Write ("Manager terminating");
    Ada.Text_IO.Put_Line ("Manager terminating");
    Server.Shutdown;
    Clock.Shutdown;
    Ten_Micron.Shutdown;
    Log.Write ("Manager end");
    Ada.Text_IO.Put_Line ("Manager end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Manager;

end Control;
