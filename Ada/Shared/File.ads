-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar;
with Ada.Directories;

private with Ada.Iterator_Interfaces;

package File is

  subtype Size is Ada.Directories.File_Size;

  type Folder is new String;

  function Folder_Separator return Character;

  function "+" (Directory : String) return Folder;

  function "+" (Left, Right : String) return Folder;

  function "+" (Left  : Folder;
                Right : String) return Folder;

  function Composure (Directory : Folder;
                      Filename  : String;
                      Extension : String) return String;

  function Composure (Directory : String;
                      Filename  : String;
                      Extension : String) return String;
  -- no exception

  function Base_Name_Of (Name : String) return String renames Ada.Directories.Base_Name;

  function Extension_Of (Name : String) return String renames Ada.Directories.Extension;

  function Containing_Directory_Of (Name : String) return String renames Ada.Directories.Containing_Directory;

  function Exists (Name : String) return Boolean;
  -- no exception

  function Size_Of (Name : String) return Size renames Ada.Directories.Size;

  function Directory_Exists (Name : String) return Boolean;
  -- no exception

  function Modification_Time_Of (Name : String) return Ada.Calendar.Time renames Ada.Directories.Modification_Time;

  function Is_Newer (The_Name  : String;
                     Than_Name : String) return Boolean;

  procedure Delete (Name : String);
  -- no exception if no existance

  procedure Create_Directory (Path : String); -- creates the whole directory path

  procedure Delete_Directory (Name : String); -- including contents
  -- no exception if no existance

  procedure Rename (Old_Name : String;
                    New_Name : String) renames Ada.Directories.Rename;

  procedure Copy (Source_Name   : String;
                  Target_Name   : String;
                  Form          : String := "") renames Ada.Directories.Copy_File;

  function Is_Leaf_Directory (Directory : String) return Boolean;

  procedure Iterate_Over_Leaf_Directories (From_Directory : String;
                                           Iterator       : access procedure (Leaf_Directory : String));

  function Found_Directory (Simple_Name  : String;
                            In_Directory : String) return String;

  Not_Found : exception;

  Name_Error : exception renames Ada.Directories.Name_Error;
  Use_Error  : exception renames Ada.Directories.Use_Error;


  ------------------------
  -- File Iterator Loop --
  ------------------------
  -- Example:
  --          for The_Filename of File.Iterator_For ("C:\Program_Files") loop
  --            Log.Write (The_Filename);
  --          end loop;

  type Item (Name_Length : Natural) is tagged limited private
  with
    Constant_Indexing => Constant_Reference,
    Default_Iterator  => Iterate,
    Iterator_Element  => String;

  function Iterator_For (Name : String) return Item;

private

  type Cursor_Data;

  type Cursor is access all Cursor_Data;

  function Has_More (Data : Cursor) return Boolean;

  package List_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_More);

  function Constant_Reference (The_Item     : aliased Item;
                               Unused_Index : Cursor) return String with Inline;

  function Iterate (The_Item : Item) return List_Iterator_Interfaces.Forward_Iterator'class;

  type Cursor_Data is record
    Has_More : Boolean := False;
    Position : Ada.Directories.Search_Type;
  end record;

  type Item (Name_Length : Natural) is tagged limited record
    Name   : String(1..Name_Length);
    Actual : Ada.Directories.Directory_Entry_Type;
    Data   : aliased Cursor_Data;
  end record;

end File;
