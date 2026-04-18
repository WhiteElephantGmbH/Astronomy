-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Earth;
with Name;
with Space;
with Time;

package Neo is

  function Read return Boolean; -- if false show Error.Message

  function Is_Arriving (Item : Name.Id) return Boolean;

  function Direction_Of (Item : Name.Id;
                         Ut   : Time.Ut) return Space.Direction;

  function Tracking_Period_Of (Item : Name.Id) return Time.Period;

  function Wrap_Location_Of (Item : Name.Id) return Earth.Direction;

  function Name_Of (Number : Natural) return String; -- empty if Read returnd false

end Neo;
