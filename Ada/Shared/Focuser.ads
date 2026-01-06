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

package Focuser is

  type Status is (Disconnected, Moving, Stopped);

  subtype Distance is Natural range 0 .. 2**24 - 1;

  type Object is interface;

  type Object_Access is access all Object'class;

  function State (Item : Object) return Status is abstract;

  function Name (Item : Object) return String is abstract;

  function Actual_Position (Item : Object) return Distance is abstract;

  procedure Move_To (Item     : Object;
                     Position : Distance) is abstract;

  procedure Stop (Item : Object) is abstract;

end Focuser;
