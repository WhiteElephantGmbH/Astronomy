-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Persistent;

package body Site is

  package Persistent_Site is new Persistent (Data, "Site");

  The_Site : Persistent_Site.Data;

  Defined : Boolean := not Persistent_Site.Storage_Is_Empty;


  procedure Define (Item : Data) is
  begin
    Defined := False;
    The_Site.Storage := Item;
    Defined := True;
  end Define;


  procedure Check_Defined is
  begin
    if not Defined then
      raise Not_Defined;
    end if;
  end Check_Defined;


  function Is_Defined return Boolean is
  begin
    return Defined;
  end Is_Defined;


  function Latitude return Angle.Value is
  begin
    Check_Defined;
    return The_Site.Storage.Latitude;
  end Latitude;


  function Longitude return Angle.Value is
  begin
    Check_Defined;
    return The_Site.Storage.Longitude;
  end Longitude;


  function Elevation return Integer is
  begin
    Check_Defined;
    return The_Site.Storage.Elevation;
  end Elevation;

end Site;
