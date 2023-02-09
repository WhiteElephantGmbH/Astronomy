-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Space;
with Time;

package Moon is

  function Direction_Of (Id : Name.Id := Name.No_Id;
                         UT : Time.Ut) return Space.Direction;

  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut);

  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut;
                           Libration : out Angle.Degrees);
end Moon;
