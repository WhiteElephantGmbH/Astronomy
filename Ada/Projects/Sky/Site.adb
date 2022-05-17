-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Traces;

package body Site is

  package Log is new Traces ("Site");

  type Element is record
    Location   : Data;
    Is_Defined : Boolean := False;
  end record;

  package Persistent_Site is new Persistent (Element, "Site");

  The_Data : Persistent_Site.Data;

  The_Site : Data    renames The_Data.Storage.Location;
  Defined  : Boolean renames The_Data.Storage.Is_Defined;


  procedure Clear is
  begin
    Defined := False;
  end Clear;


  procedure Define (Item : Data) is
  begin
    The_Site := Item;
    Log.Write ("Latitude  := " & Angle.Image_Of (The_Site.Latitude));
    Log.Write ("Longitude := " & Angle.Image_Of (The_Site.Longitude));
    Log.Write ("Elevation :="  & Integer'image (The_Site.Elevation) & 'm');
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
    return The_Site.Latitude;
  end Latitude;


  function Longitude return Angle.Value is
  begin
    Check_Defined;
    return The_Site.Longitude;
  end Longitude;


  function Elevation return Integer is
  begin
    Check_Defined;
    return The_Site.Elevation;
  end Elevation;

end Site;
