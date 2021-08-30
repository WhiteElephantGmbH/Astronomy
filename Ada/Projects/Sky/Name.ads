-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Data;
with Earth;
with Space;
with Time;

package Name is

  type Selector is (Enumerated, Caldwell, Messier);

  type Object_Kind is (Landmark, Moon, Sun, Planet, Near_Earth_Object, Small_Solar_System_Body, Sky_Object);

  type Id is private;

  No_Id : constant Id;

  type Id_Access is access all Id;

  function "=" (Left, Right : Id) return Boolean;

  function "<" (Left, Right : Id) return Boolean;

  type Id_Range is new Natural range 0 .. 10000;

  subtype Id_Index is Id_Range range 1 .. Id_Range'last - 1;

  type Id_Array is array (Id_Index) of aliased Id;

  type Id_List is record
    Kind : Data.Kind := Data.Kind'last;
    Ids  : Id_Array;
    Last : Id_Range := 0;
  end record;

  type Id_List_Access is access all Id_List;

  type Get_Space_Access is access function (Item : Id;
                                            Ut   : Time.Ut) return Space.Direction;

  function "=" (Left, Right : Id_List) return Boolean;

  procedure Read_Favorites (Enable_Neos        : Boolean := True;
                            Enable_Land_Marks  : Boolean := True);

  procedure Define (List : Data.Kind);

  function Actual_List return Id_List;

  procedure Update (Targets : Id_List_Access;
                    Remove  : access procedure (Index : Natural);
                    Insert  : access procedure (Item  : Id_Access; Index : Natural));

  function Visibility_Changed_For (Item    : in out Id;
                                   Visible :        Boolean) return Boolean;

  procedure Clear_History_For (Targets : in out Id_List);

  function Item_Of (List  : Id_List;
                    Image : String) return Id;

  function Item_Of (List             : Id_List;
                    Direction        : Space.Direction;
                    Search_Tolerance : Space.Distance) return Id;

  function Is_Known (Item : Id) return Boolean;

  function Is_Visible (Item : Id) return Boolean;

  function Image_Of (Item : Id) return String;

  function Matches (Item      : Id;
                    Number_Id : Selector;
                    Number    : Natural) return Boolean;

  function Prefix_Of (Item : Id) return String;

  function Kind_Of (Item : Id) return Object_Kind;

  function Object_Of (Item : Id) return Data.Object;

  function Type_Of (Item : Id) return Data.Object_Type;

  function Direction_Of (Item : Id;
                         Ut   : Time.Ut) return Space.Direction;

  function Direction_Of (Item : Id) return Earth.Direction;

  function Magnitude_Of (Item : Id) return Float;

private

  type Element_Data;

  type Element_Access is access Element_Data;

  type Id (Data_Id : Data.Kind := Data.Favorites) is record
    Was_Visible : Boolean := False;
    Is_Visible  : Boolean := False;
    case Data_Id is
    when Data.Favorites =>
      Element : Element_Access;
    when others =>
      Element_Number : Natural := 0;
    end case;
  end record;

  No_Id : constant Id := (others => <>);

end Name;
