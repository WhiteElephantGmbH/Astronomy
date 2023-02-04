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

with Data;
with Objects;
with Time;
with Traces;

package body Star is

  package Log is new Traces ("Star");

  The_List : List(Number);
  The_Last : Natural := 0;

  The_Directions : array (Number) of Direction;


  function Data_List return List is
  begin
    return The_List(1..The_Last);
  end Data_List;


  function Location_Of (Id : Number) return Direction is
  begin
    return The_Directions (Id);
  end Location_Of;


  procedure Read (At_Time : Ada.Calendar.Time) is

    Ut : constant Time.Ut := Time.Universal_Of (At_Time);

    The_Id : Natural := 0;

    function Next return Boolean is
    begin
      The_Id := Data.Next_Of (The_Id, Data.Hr);
      return True;
    exception
    when Data.End_Of_List =>
      return False;
    end Next;

    The_Object    : Data.Object;
    The_Direction : Earth.Direction;

  begin -- Read
    The_Directions := [others => Earth.Unknown_Direction];
    The_Last := 0;
    Log.Write ("Read - Time => " & Time.Image_Of (Ut));
    while Next loop
      The_Object := Data.Object_Of (The_Id, Data.Hr);
      The_Direction := Objects.Direction_Of (Data.Direction_Of (The_Object, Ut), Time.Lmst_Of (Ut));
      if not Earth.Is_Below_Horizon (The_Direction) then
        The_Last := @ + 1;
        Log.Write ("Read - visible HR Id => " & The_Id'image);
        The_List(The_Last) := (Id  => The_Id,
                               Mag => Magnitude(Data.Magnitude_Of (The_Object)),
                               Loc => The_Direction);
        The_Directions(The_Id) := The_Direction;
      end if;
    end loop;
  exception
  when Item : others =>
    Log.Termination (Item);
  end Read;

end Star;
