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

package Catalog is

  type Object is new Natural;

  Undefined : constant := 0;

  type Object_Type is (Landmark, Star, Double, Stars, Cluster, Galaxy, Nebula, Quasar, Satellite, Unknown);

  type Degrees   is delta 0.00000001 digits 12;
  type Motion    is delta 0.01       digits 7;
  type Magnitude is delta 0.001      digits 5;

  type Information is record
    Name        : access constant String := new String'("");
    Descriptor  : access constant String := new String'("");
    Ra_J2000    : Degrees     := 0.0;
    Dec_J2000   : Degrees     := 0.0;
    Ra_Motion   : Motion      := 0.0;
    Dec_Motion  : Motion      := 0.0;
    Vmag        : Magnitude   := 0.0;
    Kind        : Object_Type := Unknown;
  end record;

  function Table_Of (Id : Object) return Information with Inline;

  function Last_Index return Object with Inline;

  function Caldwell_Id (Item : Positive) return Object with Inline;

  function Hip_Id (Item : Positive) return Object with Inline;

  function Hr_Id (Item : Positive) return Object with Inline;

  function Messier_Id (Item : Positive) return Object with Inline;

  function Ngc_Id (Item : Positive) return Object with Inline;

  function Ocl_Id (Item : Positive) return Object with Inline;

  function Quasars_Id (Item : Positive) return Object with Inline;

private

  type Objects is array (Object range <>) of Information;

end Catalog;
