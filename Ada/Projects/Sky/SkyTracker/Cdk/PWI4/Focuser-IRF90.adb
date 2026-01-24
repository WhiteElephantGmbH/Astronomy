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

with Device;
with PWI4;
with Traces;

package body Focuser.IRF90 is

  package Log is new Traces ("Focuser.IRF90");

  package PWI4_Focuser renames Device.Focuser;

  function New_Device return Object_Access is (new Item);


  overriding
  function State (Unused : Item) return Status is
    Actual_State : constant PWI4_Focuser.State := PWI4_Focuser.Actual_State;
  begin
    case Actual_State is
    when PWI4_Focuser.Moving =>
      return Moving;
    when PWI4_Focuser.Connected =>
      return Stopped;
    when others =>
      return Disconnected;
    end case;
  end State;


  overriding
  function Name (Unused : Item) return String is ("IRF90");


  overriding
  function Actual_Position (Unused : Item) return Distance is
    Position : constant Distance := Distance(PWI4_Focuser.Actual_Position / PWI4.Microns_Delta);
  begin
    Log.Write ("Actual_Position:" & Position'image);
    return Position;
  end Actual_Position;


  overriding
  procedure Move_To (Unused   : Item;
                     Position : Distance) is
  begin
    Log.Write ("Move_To:" & Position'image);
    PWI4_Focuser.Go_To (PWI4.Microns(Position) * PWI4.Microns_Delta);
  end Move_To;


  overriding
  procedure Stop (Unused : Item) is
  begin
    Log.Write ("Stop");
    PWI4_Focuser.Stop;
  end Stop;

end Focuser.IRF90;
