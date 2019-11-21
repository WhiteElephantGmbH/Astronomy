-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Definite_Doubly_Linked_Lists;
with String_List;

package Configuration is

  type File_Handle is limited private;

  type Section_Handle is private;

  function Handle_For (File_Name : String) return File_Handle;

  function Handle_For (For_Handle   : File_Handle;
                       Section_Name : String) return Section_Handle;

  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : String := "") return String;

  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : String_List.Item := String_List.Empty) return String_List.Item;

  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : Integer := 0) return Integer;

  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : Boolean := False) return Boolean;

private

  type Item_Data (Key_Length  : Natural;
                  Value_Length : Natural) is
  record
    Key   : String (1..Key_Length);
    Value : String (1..Value_Length);
  end record;

  type Item is access Item_Data;

  package Item_List is new Definite_Doubly_Linked_Lists (Item);

  type Section_Handle is access Item_List.Item;

  type Section_Data (Name_Length : Natural) is record
    Name  : String (1..Name_Length);
    Items : Section_Handle;
  end record;

  type Section is access Section_Data;

  package Section_List is new Definite_Doubly_Linked_Lists (Section);

  type File_Handle is access Section_List.Item;

end Configuration;
