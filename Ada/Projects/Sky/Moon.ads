-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Name;
with Sky;
with Space;
with Time;

package Moon is

  procedure Define (UT: Time.Ut);

  function Direction_Of (Id               : Name.Id;
                         Check_Visibility : Boolean := True) return Space.Direction;
  -- Precondition : Define must have been called;
  -- Postcondition: returns undefined direction if feature is not visible and checks are enabled

  function Direction_Of (UT : Time.Ut) return Space.Direction;

  function Direction_Of (Id : Name.Id;
                         UT : Time.Ut) return Space.Direction; -- no visibility checks

  subtype Feature is Sky.Moon_Feature;

  subtype Feature_Type is Sky.Moon_Feature_Type;
  
  function Has_Feature (Id   :     Name.Id;
                        Item : out Feature;
                        Kind : out Feature_Type) return Boolean;

  function Image_Of (Item : Feature_Type) return String;

  function Feature_Kind_Of (Id : Name.Id) return String;

  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut);

  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut;
                           Libration : out Angle.Degrees);
private

  Id : constant String := "Moon";

  procedure Define (Min_Sun_Altitude : Angle.Degrees;
                    Max_Sun_Altitude : Angle.Degrees);

end Moon;
