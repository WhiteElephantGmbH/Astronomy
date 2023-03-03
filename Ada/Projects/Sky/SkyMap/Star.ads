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

with Object.Star;
with Earth;
with Time;

package Star is

  subtype Number is Object.Star.Id;

  subtype Magnitude is Object.Magnitude;

  subtype Direction is Earth.Direction;

  type Information is record
    Id  : Number;
    Mag : Magnitude;
    Loc : Direction;
  end record;

  type List is array (Number range <>) of Information;

  function Data_List return List;

  function Location_Of (Id : Number) return Direction;

  procedure Read (Ut : Time.Ut);

end Star;
