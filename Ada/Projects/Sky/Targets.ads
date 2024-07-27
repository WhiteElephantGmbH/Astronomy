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

private with Angle;

with Name;
with Space;
with Time;

package Targets is

  Maximum_Search_Tolerance : constant := 0.5; -- degrees

  type Selection is (All_Objects, Solar_System, Clusters, Open_Clusters, Nebulas, Galaxies, Stars, Multiple_Stars,
                     Near_Earth_Objects);

  subtype Deep_Sky_Selection is Selection range Clusters .. Multiple_Stars;

  subtype Object_Kind is Selection range Selection'succ(Selection'first) .. Selection'last;

  function Image_Of (The_Selection : Selection) return String;

  function Selection_Of (Image : String) return Selection;

  type Moon_Feature_Selection is (All_Features,       --
                                  Bays,      -- Sinus (Buchten)
                                  Cliffs,    -- Rupes (Klippen)
                                  Craters,   -- Craters & Catenas (Krater und Kraterketten)
                                  Rilles,    -- Rimas (Rillen)
                                  Lakes,     -- Lacus (Seeen)
                                  Mountains, -- Mons & Promontorium (Gebirge und Vorgebirge)
                                  Oceans,    -- Oceanus (Ozeane)
                                  Seas,      -- Mares (Meere)
                                  Swamps,    -- Palus (Sümpfe)
                                  Swirls,    -- Swirl (Wirbel)
                                  Valleys);  -- Vallis (Täler)

  subtype Feature_Kind is Moon_Feature_Selection
    range Moon_Feature_Selection'succ(Moon_Feature_Selection'first) .. Moon_Feature_Selection'last;

  function Image_Of (The_Feature_Selection : Moon_Feature_Selection) return String;

  function Feature_Of (Image : String) return Moon_Feature_Selection;

  type Arriving_Handling is access function (Id : Name.Id) return Boolean;

  procedure Start (Clear    : access procedure;
                   Define   : access procedure (List : Name.Id_List_Access);
                   Update   : access procedure;
                   Arriving : Arriving_Handling := null);

  procedure Define_Catalog;

  procedure Set (The_Selection : Selection);

  procedure Set (The_Feature : Moon_Feature_Selection);

  procedure Update_List;

  procedure Get_For (Target_Name :     String;
                     Target_Id   : out Name.Id);

  procedure Get_For (The_Direction :     Space.Direction;
                     Tolerance     :     Space.Distance;
                     Target_Id     : out Name.Id);

  procedure Stop;

  function J2000_Direction_Of (Id : Name.Id) return Space.Direction;

  function Text_Of (Visible_In : Duration) return String;

  function Moon_Direction_Of (Id : Name.Id := Name.No_Id;
                              UT : Time.Ut) return Space.Direction;

  function Solar_System_Direction_Of (Item : Name.Id;
                                      Ut   : Time.Ut) return Space.Direction;

  function Description_Of (Id : Name.Id) return String;

private

  type Az_Range is record
    From : Angle.Value := Angle.Zero; -- all directions
    To   : Angle.Value := Angle.Zero;
  end record;

  procedure Set (The_Range : Az_Range);

  type Switch is (Off, On);

  procedure Set (Sorted : Switch);

end Targets;
