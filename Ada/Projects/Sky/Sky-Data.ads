-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Space;
with Time;

package Sky.Data is

  type Element is record
    Catalog : Catalogs := Catalogs'first; -- any catalog
    Number  : Natural := 0;               -- default not in catalogs
  end record;

  No_Element : constant Element := (others => <>);

  function Find_Element (From_Direction : Space.Direction;
                         With_Tolerance : Space.Distance) return Element;

  subtype Index is Object range First_Object .. Object'last;

  function Object_Of (Item     : Positive;
                      The_Kind : Extended_Catalogs) return Object;

  function Next_Of (Item     : Natural;
                    The_Kind : Extended_Catalogs) return Natural;
  No_More : constant := 0;

  function Name_Of (Id : Index) return String;

  function Name_Of (Item     : Positive;
                    The_Kind : Extended_Catalogs) return String;

  function Descriptor_Of (Id : Index) return String;

  function Object_Type_Of (Id : Index) return Object_Type;

  function Direction_Of (Id       : Index;
                         Ut       : Time.Ut;
                         Is_J2000 : Boolean := False) return Space.Direction;

  function Magnitude_Of (Id : Index) return Sky.Magnitude;

  function New_Object_For (Item        : String;
                           Description : String;
                           Object_Kind : Object_Type := Unknown;
                           Direction   : Space.Direction := Space.Unknown_Direction) return Index;

  function Neo_Object_Of (Item : String) return Index;

  function New_Neo_Object_For (Item        : String;
                               Description : String) return Positive;

  function Neo_Index_Of (Id : Index) return Positive;

  procedure Apparent (Ra  : in out Astro.REAL;
                      Dec : in out Astro.REAL);

end Sky.Data;
