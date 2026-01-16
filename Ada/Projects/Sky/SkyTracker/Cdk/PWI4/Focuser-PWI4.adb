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

with Traces;

package body Focuser.PWI4 is

  package Log is new Traces ("Focuser.PWI4");

  function New_Device return Object_Access is (new Device);

  The_State     : Status := Stopped;
  The_Increment : Natural;
  The_Position  : Natural := 11000;
  The_Goal      : Natural;

  overriding
  function State (Unused : Device) return Status is
  begin
    case The_State is
    when Moving =>
      The_Position := @ + The_Increment;
      if abs (The_Position - The_Goal) < abs (The_Increment) then
        The_State := Stopped;
        The_Position := The_Goal;
      end if;
      Log.Write ("Position:" & The_Position'image);
    when others =>
      null;
    end case;
    return The_State;
  end State;


  overriding
  function Name (Unused : Device) return String is ("IRF90");


  overriding
  function Actual_Position (Unused : Device) return Distance is
  begin
    Log.Write ("Actual_Position:" & The_Position'image);
    return The_Position;
  end Actual_Position;


  overriding
  procedure Move_To (Unused   : Device;
                     Position : Distance) is
  begin
    The_Goal := Natural(Position);
    The_Increment := The_Goal - The_Position;
    if (abs The_Increment) > 5 then
      The_Increment := @ / 5;
    end if;
    The_State := Moving;
  end Move_To;


  overriding
  procedure Stop (Unused : Device) is
  begin
    The_State := Stopped;
  end Stop;

end Focuser.PWI4;
