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
pragma Style_White_Elephant;

with Celestron.Focuser;
with Focuser_Client;

package body Focuser.HPS is

  function New_Device return Object_Access is (new Device);


  overriding
  function State (Unused : Device) return Status is
    Data : constant Celestron.Focuser.Data := Focuser_Client.Actual_Data;
  begin
    if Data.Exists then
      if Data.Moving then
        return Moving;
      else
        return Stopped;
      end if;
    end if;
    return Disconnected;
  end State;


  overriding
  function Name (Unused : Device) return String is ("Celestron");


  function Actual_Position (Unused : Device) return Distance is
    Data : constant Celestron.Focuser.Data := Focuser_Client.Actual_Data;
  begin
    return Data.Position;
  end Actual_Position;


  overriding
  procedure Move_To (Unused   : Device;
                     Position : Distance) is
    Unused_Data : Celestron.Focuser.Data;
  begin
    Unused_Data := Focuser_Client.Move_To (Position);
  end Move_To;


  overriding
  procedure Stop (Unused : Device) is
    Unused_Data : Celestron.Focuser.Data;
  begin
    Unused_Data := Focuser_Client.Execute (Celestron.Focuser.Stop);
  end Stop;

end Focuser.HPS;
