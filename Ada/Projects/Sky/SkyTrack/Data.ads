-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Astro;
with Catalog;
with Space;
with Time;

package Data is

  type Kind is (Favorites, Caldwell, Hip, Hr, Messier, Neo, Ngc, Ocl, Quasars);

  procedure Apparent (Ra  : in out Astro.REAL;
                      Dec : in out Astro.REAL);

  subtype Object is Catalog.Object;

  Undefined : constant := Catalog.Undefined;

  type Object_Type is new Catalog.Object_Type;

  function Value_Of (Item : String) return Natural;

  function Object_Of (Item     : Positive;
                      The_Kind : Kind) return Object;

  End_Of_List : exception;

  function Next_Of (Item     : Natural;
                    The_Kind : Kind) return Natural;

  function Name_Of (Id : Object) return String;

  function Descriptor_Of (Id : Object) return String;

  function Direction_Of (Id : Object;
                         Ut : Time.Ut) return Space.Direction;

  function Magnitude_Of (Id : Object) return Float;

  function Type_Of (Id : Object) return Object_Type;

  function New_Object_For (Name        : String;
                           Description : String;
                           The_Type    : Object_Type := Star;
                           Direction   : Space.Direction := Space.Unknown_Direction) return Object;

  function New_Neo_Object_For (Name        : String;
                               Description : String;
                               The_Type    : Object_Type) return Positive;

  function Neo_Object_Of (Name : String) return Object;

  function Neo_Index_Of (Id : Object) return Positive;

end Data;
