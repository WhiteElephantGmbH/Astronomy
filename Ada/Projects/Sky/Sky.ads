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

with Database.Moon;

package Sky is

  type Catalog_Id is (Favorites, Moon, Caldwell, Name, Star, Hd, Hip, Hr, Messier, Ngc, Ic, Ocl, Quasars, Neo);

  subtype Catalogs is Catalog_Id range Caldwell .. Quasars;

  subtype Constellation is Database.Constellation;

  subtype Extended_Catalogs is Catalog_Id range Moon .. Neo;

  subtype Moon_Feature is Database.Moon.Feature_Name;

  subtype Moon_Feature_Type is Database.Moon_Feature_Type;

  subtype Greek_Letter is Database.Greek_Letter;

  subtype Simbad_Catalog is Catalog_Id range Star .. Ocl;

  type Magnitude is delta 0.001 digits 5;

  Unknown_Magnitude : constant Magnitude := Magnitude'last;

  type Object is new Natural;

  type Object_Type is (Moon, Star, Double, Stars, Cluster, Galaxy, Nebula, Quasar, Satellite, Unknown);

  Undefined    : constant := 0;
  First_Object : constant := 1;

  Max_Search_Distance : constant := 1000; -- maximum allowed offset to the result of Sky.Catalog.Found_Index_For

end Sky;
