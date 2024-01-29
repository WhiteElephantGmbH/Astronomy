-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Object.Catalog;
with Object.Target;
with Objects;
with Traces;

package body Star is

  package Log is new Traces ("Star");


  The_Stars : Stars.List;

  The_Directions : array (Id) of Direction;


  function Data_List return List is
  begin
    return The_Stars;
  end Data_List;


  function Location_Of (Item : Id) return Direction is
  begin
    return The_Directions (Item);
  end Location_Of;


  function "<" (Left, Right : Information) return Boolean is
    use type Parallax;
  begin
    return Left.Plx < Right.Plx;
  end "<";

  package Parallax_Data is new Stars.Generic_Sorting;


  procedure Read (Ut : Time.Ut) is

    The_Direction : Earth.Direction;

  begin -- Read
    The_Directions := [others => Earth.Unknown_Direction];
    Object.Set (Ut);
    The_Stars.Clear;
    for The_Id in Id loop
      if Object.Catalog.Type_Of (The_Id) in Object.Star then
        The_Direction := Objects.Direction_Of (Object.Target.Direction_Of (The_Id), Time.Lmst_Of (Ut));
        if not Earth.Is_Below_Horizon (The_Direction) then
          The_Stars.Append ((Ident => The_Id,
                             Mag   => Object.Catalog.Magnitude_Of (The_Id),
                             Class => Object.Catalog.Spec_Class_Of (The_Id),
                             Plx   => Object.Catalog.Parallax_Of (The_Id),
                             Loc   => The_Direction));
        end if;
        The_Directions(The_Id) := The_Direction;
      end if;
    end loop;
    Parallax_Data.Sort (The_Stars);
    Log.Write ("Read" & The_Stars.Length'image & " visible stars");
  exception
  when Item : others =>
    Log.Termination (Item);
  end Read;

end Star;
