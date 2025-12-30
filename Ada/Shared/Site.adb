-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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


  function Verified (Item : Data) return Boolean is
    use type Angle.Value;
    Delta_Latitude  : constant Angle.Degrees := Item.Latitude - The_Site.Latitude;
    Delta_Longitude : constant Angle.Degrees := Item.Longitude - The_Site.Longitude;
    Delta_Elevation : constant Integer       := Item.Elevation - The_Site.Elevation;
    use type Angle.Degrees;
    Max_Angle_Error     : constant Angle.Degrees := 1.0 / 36000.0; -- a 10th of an arc second
    Max_Elevation_Error : constant Integer       := 1; -- meter
  begin
    Log.Write ("Delta_Latitude  :" & Delta_Latitude'image);
    Log.Write ("Delta_Longitude :" & Delta_Longitude'image);
    Log.Write ("Delta_Elevation :" & Delta_Elevation'image);
    if abs Delta_Latitude > Max_Angle_Error then
      Log.Error ("Incorrect Latitude : " & Angle.Signed_Degrees_Image_Of (Item.Latitude));
      return False;
    elsif abs Delta_Longitude > Max_Angle_Error then
      Log.Error ("Incorrect Longitude : " & Angle.Signed_Degrees_Image_Of (Item.Longitude));
      return False;
    elsif abs Delta_Elevation > Max_Elevation_Error then
      Log.Error ("Incorrect Elevation :" & Item.Elevation'image);
      return False;
    end if;
    return True;
  end Verified;

end Site;
