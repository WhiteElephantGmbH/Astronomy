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
with Lexicon;

package body Moon.Data is

  subtype Feature_Name is Database.Moon.Feature_Name;

  function Feature_Name_Of (Id : Name.Id) return Feature_Name is
    The_Word : Lexicon.Word;
  begin
    The_Word := Lexicon.Word_Of (Name.Image_Of (Id));
    return Feature_Name'value(The_Word'image);
  end Feature_Name_Of;


  function Direction_Of (Id : Name.Id;
                         UT : Time.Ut) return Space.Direction is
  begin
    declare
      Feature : constant Feature_Name := Feature_Name_Of (Id);
      Item    : Database.Moon.Feature renames Database.Moon.List(Feature);
      use type Angle.Value;
    begin
      return Moon.Direction_Of (UT, (Longitude => +Angle.Degrees(Item.Longitude),
                                      Latitude => +Angle.Degrees(Item.Latitude)));
    end;
  exception
  when others =>
    return Moon.Direction_Of (UT);
  end Direction_Of;


  function Feature_Kind_Of (Id : Name.Id) return String is
  begin
    declare
      Feature : constant Feature_Name := Feature_Name_Of (Id);
      Item : Database.Moon.Feature renames Database.Moon.List(Feature);
    begin
      return Lexicon.Image_Of (Lexicon.Word'value(Item.Kind'image));
    end;
  exception
  when others =>
    return "";
  end Feature_Kind_Of;

end Moon.Data;
