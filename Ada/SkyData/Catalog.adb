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

with Catalog.Base;
with Catalog.Caldwell;
with Catalog.Hr;
with Catalog.Hip;
with Catalog.Ocl;
with Catalog.Messier;
with Catalog.Ngc;
with Catalog.Quasars;

package body Catalog is

  function Table_Of (Id : Object) return Information is
  begin
    return Base.Table(Id);
  end Table_Of;


  function Caldwell_Id (Item : Positive) return Object is
  begin
    return Caldwell.Id (Item);
  end Caldwell_Id;


  function Hip_Id (Item : Positive) return Object is
  begin
    return Hip.Id (Item);
  end Hip_Id;


  function Hr_Id (Item : Positive) return Object is
  begin
    return Hr.Id (Item);
  end Hr_Id;


  function Messier_Id (Item : Positive) return Object is
  begin
    return Messier.Id (Item);
  end Messier_Id;


  function Ngc_Id (Item : Positive) return Object is
  begin
    return Ngc.Id (Item);
  end Ngc_Id;


  function Ocl_Id (Item : Positive) return Object is
  begin
    return Ocl.Id (Item);
  end Ocl_Id;


  function Quasars_Id (Item : Positive) return Object is
  begin
    return Quasars.Id (Item);
  end Quasars_Id;


  function Last_Index return Object is
  begin
    return Base.Object_Index'last;
  end Last_Index;

end Catalog;
