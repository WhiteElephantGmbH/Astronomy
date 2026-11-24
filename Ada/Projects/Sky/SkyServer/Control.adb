-- *********************************************************************************************************************
-- *                               (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Application;
with Collector;
with Error;
with Gui;
with Os.Application;
with Os.Process;
with Renderer;
with Traces;
with User;

package body Control is

  package Log is new Traces ("Control");

  task type Manager is
    entry Close;
    entry Termination;
  end Manager;


  task body Manager is
  begin
    Log.Write ("manager start");
    Renderer.Start;
    Collector.Start;
    accept Close;
    Collector.Close;
    Renderer.Close;
    Log.Write ("manager end");
    accept Termination;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
    Collector.Close;
    Gui.Close;
    accept Termination;
  end Manager;


  procedure Start is

    The_Manager : access Manager;

    procedure Startup is
    begin
      The_Manager := new Manager;
    end Startup;

    procedure Finish is
    begin
      The_Manager.Close;
    end Finish;

    procedure Finalize is
    begin
      The_Manager.Termination;
    end Finalize;

  begin -- Start
    if (not Os.Is_Osx) and then (not Os.Application.Is_First_Instance) then
      --
      -- Note: This test is to prevent this application from being run more than once concurrently.
      --       If we mandate that this application is always run from within an OSX .app bundle then
      --       OSX will enforce this and therefore this test is not required.
      --       In this case it is better not to attempt detecting first instance because if the application
      --       is terminated by force quit the mutex is not released but remains until the host is rebooted.
      --
      Error.Raise_With (Application.Name & " already running");
    end if;
    Os.Process.Set_Priority_Class (Os.Process.Realtime);
    User.Execute (Startup'access,
                  Finish'access,
                  Finalize'access);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Control;
