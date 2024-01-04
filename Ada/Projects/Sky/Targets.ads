-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Name;
with Space;

package Targets is

  Maximum_Search_Tolerance : constant := 0.5; -- degrees

  type Selection is (All_Objects, Solar_System, Clusters, Open_Clusters, Nebulas, Galaxies, Stars, Multiple_Stars,
                     Near_Earth_Objects);

  subtype Objects is Selection range Selection'succ(Selection'first) .. Selection'last;
  
  type Arriving_Handling is access function (Id : Name.Id) return Boolean;

  procedure Start (Clear    : access procedure;
                   Define   : access procedure (List : Name.Id_List_Access);
                   Update   : access procedure;
                   Arriving : Arriving_Handling := null);

  procedure Define_Catalog;

  procedure Set (The_Selection : Selection);

  procedure Update_List;

  procedure Get_For (Target_Name :     String;
                     Target_Id   : out Name.Id);

  procedure Get_For (The_Direction :     Space.Direction;
                     Tolerance     :     Space.Distance;
                     Target_Id     : out Name.Id);

  procedure Stop;
  
  function J2000_Direction_Of (Id : Name.Id) return Space.Direction;

  function Text_Of (Visible_In : Duration) return String;

end Targets;
