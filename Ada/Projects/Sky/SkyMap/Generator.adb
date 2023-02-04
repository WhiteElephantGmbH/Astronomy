-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Application;
with Error;
with Os.Application;
with Traces;
with User;

package body Generator is

  package Log is new Traces ("Generator");

  procedure Start is
  begin
    if (not Os.Is_Osx) and then (not Os.Application.Is_First_Instance) then
    --
    -- Note: This test is to prevent this application from being run more than once concurrently.
    --       If we mandate that this application is always run from within an OSX .app bundle then
    --       OSX will enforce this and therefore this test is not required.
    --       In this case it is better not to attempt detecting first instance because if the application
    --       is terminated by force quit the mutex is not released but remains until the host is rebooted.
    --
      User.Show_Error (Application.Name & " already running");
      return;
    end if;
    User.Execute;
  exception
  when Error.Occurred =>
    User.Show_Error (Error.Message);
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Generator;
