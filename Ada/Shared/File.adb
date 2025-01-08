-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body File is

  package FS renames Ada.Directories;

  The_Folder_Separator : constant Character
    with
      Import        => True,
      Convention    => C,
      External_Name => "__gnat_dir_separator";


  function Folder_Separator return Character is
  begin
    return The_Folder_Separator;
  end Folder_Separator;


  function "+" (Directory : String) return Folder is
  begin
    return Folder(Directory);
  end "+";


  function "+" (Left, Right : String) return Folder is
  begin
    return Folder(Left & The_Folder_Separator & Right);
  end "+";


  function "+" (Left  : Folder;
                Right : String) return Folder is
  begin
    return Folder(String(Left) & The_Folder_Separator & Right);
  end "+";


  function Composure (Directory : String;
                      Filename  : String;
                      Extension : String) return String renames FS.Compose;

  function Composure (Directory : Folder;
                      Filename  : String;
                      Extension : String) return String is
  begin
    return Composure (String(Directory), Filename, Extension);
  end Composure;


  function Composure (Directory               : Folder;
                      Filename_With_Extension : String) return String is
  begin
    return String(Directory) & The_Folder_Separator & Filename_With_Extension;
  end Composure;


  function Exists (Name : String) return Boolean is
    use type FS.File_Kind;
  begin
    return FS.Kind (Name) = FS.Ordinary_File;
  exception
  when others =>
    return False;
  end Exists;


  function Is_Newer (The_Name  : String;
                     Than_Name : String) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return Modification_Time_Of (The_Name) > Modification_Time_Of (Than_Name);
  end Is_Newer;


  procedure Delete (Name : String) is
  begin
    FS.Delete_File (Name);
  exception
  when Name_Error =>
    null;
  end Delete;


  procedure X_Copy (Source      : String;
                    Destination : String;
                    Pattern     : String := "*") is

    procedure Copy_Entry (Directory_Entry : FS.Directory_Entry_Type) is
      Full_Name      : constant String := FS.Full_Name(Directory_Entry);
      Dest_Full_Name : constant String := Destination & Full_Name(Full_Name'first + Source'length .. Full_Name'last);
    begin
      case FS.Kind (Directory_Entry) is
      when FS.Directory =>
        if FS.Simple_Name(Directory_Entry) /= "." and FS.Simple_Name(Directory_Entry) /= ".." then
          FS.Create_Path (Dest_Full_Name);
          FS.Search (Directory => Full_Name,
                     Pattern   => Pattern,
                     Process   => Copy_Entry'access);
        end if;
      when FS.Ordinary_File =>
        FS.Copy_File (Full_Name, Dest_Full_Name);
      when others =>
        null;
      end case;
    end Copy_Entry;

  begin -- X_Copy
    FS.Create_Path (Destination);
    FS.Search (Directory => Source,
               Pattern   => Pattern,
               Process   => Copy_Entry'access);
  end X_Copy;


  ------------------------
  -- File Iterator Loop --
  ------------------------

  function Iterator_For (Name : String) return Item is
  begin
    return (Name_Length => Name'length,
            Name        => Name,
            others      => <>);
  end Iterator_For;


  function Has_More (Data : Cursor) return Boolean is
  begin
    if Data.Has_More then
      return True;
    else
      FS.End_Search (Data.Position);
      return False;
    end if;
  end Has_More;


  function Constant_Reference (The_Item     : aliased Item;
                               Unused_Index : Cursor) return String is
  begin
    return FS.Full_Name (The_Item.Actual);
  end Constant_Reference;


  type Item_Access is access all Item;
  for Item_Access'storage_size use 0;

  type Iterator is new List_Iterator_Interfaces.Forward_Iterator with record
    Container : Item_Access;
  end record;

  overriding
  function First (Object : Iterator) return Cursor;

  overriding
  function Next (Object       : Iterator;
                 Unused_Index : Cursor) return Cursor;


  function Iterate (The_Item : Item) return List_Iterator_Interfaces.Forward_Iterator'class is
  begin
    return Iterator'(Container => The_Item'unrestricted_access);
  end Iterate;


  function First (Object : Iterator) return Cursor is
  begin
    FS.Start_Search (Search    => Object.Container.Data.Position,
                     Directory => Object.Container.Name,
                     Pattern   => "",
                     Filter    => [FS.Ordinary_File => True, others => False]);
    return Next (Object, null);
  end First;


  function Next (Object       : Iterator;
                 Unused_Index : Cursor) return Cursor is
  begin
    Object.Container.Data.Has_More := FS.More_Entries (Object.Container.Data.Position);
    if Object.Container.Data.Has_More then
      FS.Get_Next_Entry (Object.Container.Data.Position, Object.Container.Actual);
    end if;
    return Object.Container.Data'access;
  end Next;

end File;
