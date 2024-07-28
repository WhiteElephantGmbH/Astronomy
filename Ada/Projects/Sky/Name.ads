-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Earth;
with Sky;
with Space;
with Time;

package Name is

  type Object_Kind is (Axis_Position, Landmark,
                       Moon, Sun, Planet, Near_Earth_Object, Small_Solar_System_Body, Sky_Object);

  subtype Solar_Object is Object_Kind range Moon .. Planet;

  type Id (Data_Id : Sky.Catalog_Id := Sky.Favorites) is private;

  No_Id : constant Id;

  function "=" (Left, Right : Id) return Boolean;

  function "<" (Left, Right : Id) return Boolean;

  type Id_List (Kind : Sky.Catalog_Id := Sky.Catalog_Id'last) is private;

  type Id_List_Access is access all Id_List;

  type Get_Space_Access is access function (Item : Id;
                                            Ut   : Time.Ut) return Space.Direction;

  function "=" (Left, Right : Id_List) return Boolean;

  type Neo_Exists_Handler is access function (Item : String) return Boolean;

  procedure Read_Favorites (Enable_Axis_Positions : Boolean;
                            Enable_Land_Marks     : Boolean;
                            Neo_Existing          : Neo_Exists_Handler := null);

  procedure Define (Catalog : Sky.Catalog_Id);

  procedure Sort (The_List : in out Id_List);

  function Actual_List return Id_List;

  type Id_Access is access constant Id;

  procedure For_All (In_List : in out Id_List;
                     Handle  : access procedure (Item : in out Id));

  procedure Update (The_Targets : Id_List_Access;
                    Remove      : access procedure (Index : Natural);
                    Insert      : access procedure (Item  : Id_Access; Index : Natural));

  function Visibility_Changed_For (Item    : in out Id;
                                   Visible :        Boolean) return Boolean;

  procedure Clear_History_For (The_Targets : in out Id_List);

  function Item_Of (List  : Id_List;
                    Image : String) return Id;

  function Item_Of (List             : Id_List;
                    Direction        : Space.Direction;
                    Search_Tolerance : Space.Distance) return Id;

  function Is_Known (Item : Id) return Boolean;

  function Is_Visible (Item : Id) return Boolean;

  function Image_Of (Item : Id) return String;

  function Matches (Item   : Id;
                    Number : Natural) return Boolean;

  function Kind_Of (Item : Id) return Object_Kind;

  function Object_Of (Item : Id) return Sky.Object;

  function Type_Of (Item : Id) return Sky.Object_Type;

  function Direction_Of (Item : Id;
                         Ut   : Time.Ut) return Space.Direction;

  function Direction_Of (Item : Id) return Space.Direction;

  function Direction_Of (Item : Id) return Earth.Direction;

  function Magnitude_Of (Item : Id) return Sky.Magnitude;

private

  type Data_Kind is (Axis_Position, Landmark, Sky_Object);

  type Element_Data (Item : Data_Kind);

  type Element_Access is access Element_Data;

  type Id (Data_Id : Sky.Catalog_Id := Sky.Favorites) is record
    Was_Visible : Boolean := False;
    Is_Visible  : Boolean := False;
    case Data_Id is
    when Sky.Favorites =>
      Element : Element_Access;
    when others =>
      Element_Number : Natural := 0;
    end case;
  end record;

  No_Id : constant Id := (others => <>);

  package Names is new Ada.Containers.Doubly_Linked_Lists (Id);

  type Id_List (Kind : Sky.Catalog_Id := Sky.Catalog_Id'last) is record
    Ids : Names.List;
  end record;

end Name;
