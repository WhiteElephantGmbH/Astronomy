-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Finalization;
with Definite_Doubly_Linked_Lists;

generic
  Name : String;
  type Kind is private;
  with package Definite_List is new Definite_Doubly_Linked_Lists (Kind);
package Persistent_Definite_Doubly_Linked_Lists is

  type Data is new Ada.Finalization.Limited_Controlled with record
    List : Definite_List.Item;
  end record;

  overriding procedure Initialize (The_Data : in out Data);

  overriding procedure Finalize (The_Data : in out Data);

end Persistent_Definite_Doubly_Linked_Lists;
