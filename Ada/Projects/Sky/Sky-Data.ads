-- *********************************************************************************************************************
-- *                       (c) 2024 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Astro;
with Sky.Catalog;
with Space;
with Time;

package Sky.Data is

  type Element is record
    Catalog : Catalogs := Catalogs'first; -- any catalog
    Number  : Natural := 0;               -- default not in catalogs
  end record;

  No_Element : constant Element := (others => <>);

  function Find_Element (From_Direction : Space.Direction;
                         With_Tolerance : Space.Distance;
                         Is_Jear_2000   : Boolean := False) return Element;

  subtype Index is Object range First_Object .. Object'last;

  First_Moon_Feature : constant Object := Catalog.Number'last + 1;

  Last_Moon_Feature : constant Object := First_Moon_Feature + Moon_Feature'pos(Moon_Feature'last);

  subtype Moon_Object is Object range First_Moon_Feature .. Last_Moon_Feature;

  Number_Of_Extension_Objects : constant := 1000;

  First_Extension : constant Object := Last_Moon_Feature + 1;

  subtype Extension_Object is Object range First_Extension .. First_Extension + Number_Of_Extension_Objects - 1;

  subtype Static_Object is Object range First_Object .. Extension_Object'last;

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

  function Moon_Feature_Of (Id : Object) return Moon_Feature;

  function Moon_Feature_Number_Of (Item : String) return Positive;

  function Moon_Feature_Name_Of (Item : Moon_Feature) return String;

  function Moon_Stellarium_Feature_Name_Of (Item : Moon_Feature) return String;

  function New_Object_For (Item        : String;
                           Description : String;
                           Object_Kind : Object_Type := Unknown;
                           Direction   : Space.Direction := Space.Unknown_Direction) return Index;

  function Neo_Object_Of (Number : Natural) return Object; -- returns Unknown if not found

  function New_Neo_Object_For (Item   : String;
                               Number : Natural) return Positive;

  function Neo_Index_Of (Id : Index) return Positive;

  function Neo_Number_Of (Id : Index) return Natural;

  procedure Apparent (Ra  : in out Astro.REAL;
                      Dec : in out Astro.REAL);

end Sky.Data;
