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

with Angle;
with Simbad;

package Sky.Catalog is

  subtype Number is Object range Undefined .. Object(Simbad.Number'last);

  subtype Index is Number range Number(Simbad.Index'first) .. Number'last;

  subtype Kind is Object_Type range Object_Type'first .. Quasar;

  function Object_Of (Item : String) return Object;

  function Main_Name_Of (Item : Index) return String;

  function Object_Image_Of (Item     : Index;
                            New_Name : String := "") return String;
  -- returns more than one name

  function Name_Of (Item     : Positive;
                    The_Kind : Catalogs) return String;

  function Object_Of (Item     : Positive;
                      The_Kind : Catalogs) return Object;

  function Image_Of (Id : Catalog_Id) return String;

  function Position_Of (Id   : Simbad_Catalog;
                        Item : Index) return Positive;

  function Caldwell_Id (Item : Positive) return Object with Inline;

  function Name_Id (Item : Positive) return Object with Inline;

  function Star_Id (Item : Positive) return Object with Inline;

  function Hd_Id (Item : Positive) return Object with Inline;

  function Hip_Id (Item : Positive) return Object with Inline;

  function Hr_Id (Item : Positive) return Object with Inline;

  function Messier_Id (Item : Positive) return Object with Inline;

  function Ngc_Id (Item : Positive) return Object with Inline;

  function Ocl_Id (Item : Positive) return Object with Inline;

  function Quasars_Id (Item : Positive) return Object with Inline;

  function Name_Of (Item : Index) return String;

  function Descriptor_Of (Item : Index) return String;

  function Main_Catalog_Of (Item : Index) return Simbad_Catalog with Inline;

  function Kind_Of (Item : Index) return Kind;

  function Ra_J2000_Of (Item : Index) return Angle.Degrees with Inline;

  function Dec_J2000_Of (Item : Index) return Angle.Degrees with Inline;

  function Ra_Motion_Of (Item : Index) return Angle.Degrees with Inline;

  function Dec_Motion_Of (Item : Index) return Angle.Degrees with Inline;

  function Magnitude_Of (Item : Index) return Sky.Magnitude with Inline;

  function Found_For (Dec : Angle.Degrees) return Index with Inline;

end Sky.Catalog;
