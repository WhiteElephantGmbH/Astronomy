-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Directories;
with Ada.Iterator_Interfaces;
with Text;

package Directory is

  function Exists (Name : String) return Boolean;

  procedure Create (Path : String); -- creates the whole directory path

  procedure Delete (Name : String); -- including contents
  -- no exception if no existance

  function Is_Leaf (Name       : String;
                    Exceptions : Text.Strings := Text.None) return Boolean;

  procedure Iterate_Over_Leafs (From_Directory : String;
                                Iterator       : access procedure (Leaf_Directory : String);
                                Exceptions     : Text.Strings := Text.None);

  function Found (Name         : String;
                  In_Directory : String) return String;

  Not_Found : exception;

  Name_Error : exception renames Ada.Directories.Name_Error;
  Use_Error  : exception renames Ada.Directories.Use_Error;


  -----------------------------
  -- Directory Iterator Loop --
  -----------------------------
  -- Example:
  --          for The_Directory of Directory.Iterator_For ("C:\Program_Files") loop
  --            Log.Write (The_Directory);
  --          end loop;

  type Item (Name_Length : Natural) is tagged limited private
  with
    Constant_Indexing => Constant_Reference,
    Default_Iterator  => Iterate,
    Iterator_Element  => String;

  function Iterator_For (Name : String) return Item;

  type Cursor is private;

  function Has_More (Data : Cursor) return Boolean;

  package List_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_More);

  function Iterate (The_Item : Item) return List_Iterator_Interfaces.Forward_Iterator'class;

  function Constant_Reference (The_Item     : aliased Item;
                               Unused_Index : Cursor) return String with Inline;
private

  type Cursor_Data is record
    Has_More : Boolean := False;
    Position : Ada.Directories.Search_Type;
  end record;

  type Cursor is access all Cursor_Data;

  type Item (Name_Length : Natural) is tagged limited record
    Name   : String(1..Name_Length);
    Actual : Ada.Directories.Directory_Entry_Type;
    Data   : aliased Cursor_Data;
  end record;

end Directory;
