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

with Objects;
with Traces;

package body Star is

  package Log is new Traces ("Star");

  The_List : List(Number);
  The_Last : Natural := 0;

  The_Directions : array (Number) of Direction;


  function Data_List return List is
  begin
    return The_List(Number'first .. Number(The_Last));
  end Data_List;


  function Location_Of (Id : Number) return Direction is
  begin
    return The_Directions (Id);
  end Location_Of;


  procedure Read (Ut : Time.Ut) is

    The_Direction : Earth.Direction;

  begin -- Read
    The_Directions := [others => Earth.Unknown_Direction];
    The_Last := 0;
    Object.Set (Ut);
    for The_Id in Number loop
      The_Direction := Objects.Direction_Of (Object.Star.Direction_Of (The_Id), Time.Lmst_Of (Ut));
      if not Earth.Is_Below_Horizon (The_Direction) then
        The_Last := @ + 1;
        The_List(Number(The_Last)) := (Id  => The_Id,
                                       Mag => Magnitude(Object.Star.Magnitude_Of (The_Id)),
                                       Loc => The_Direction);
      end if;
      The_Directions(The_Id) := The_Direction;
    end loop;
    Log.Write ("Read" & The_Last'image & " visible stars");
  exception
  when Item : others =>
    Log.Termination (Item);
  end Read;

end Star;