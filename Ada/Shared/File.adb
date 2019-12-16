-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Strings;

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


  function Directory_Exists (Name : String) return Boolean is
    use type FS.File_Kind;
  begin
    return FS.Kind (Name) = FS.Directory;
  exception
  when others =>
    return False;
  end Directory_Exists;


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


  procedure Create_Directory (Path : String) is
  begin
    Ada.Directories.Create_Path (Path);
  end Create_Directory;


  procedure Delete_Directory (Name : String) is
  begin
    FS.Delete_Tree (Name);
  exception
  when Name_Error =>
    null;
  end Delete_Directory;


  function Is_Leaf_Directory (Directory : String) return Boolean is
    Handle : FS.Search_Type;
  begin
    FS.Start_Search (Search    => Handle,
                     Directory => Directory,
                     Pattern   => "",
                     Filter    => (FS.Directory => True, others => False));
    return FS.More_Entries (Handle);
  exception
  when others =>
    return False;
  end Is_Leaf_Directory;


  procedure Iterate_Over_Leaf_Directories (From_Directory : String;
                                           Iterator       : access procedure (Leaf_Directory : String)) is
    The_Count : Natural := 0;

    procedure Iterate_For (Directory_Entry : FS.Directory_Entry_Type) is
    begin
      declare
        Name      : constant String := FS.Simple_Name(Directory_Entry);
        Directory : constant String := FS.Full_Name(Directory_Entry);
      begin
        if Name(Name'first) /= '.' then
          The_Count := The_Count + 1;
          Iterate_Over_Leaf_Directories (Directory, Iterator);
        end if;
      end;
    exception
    when others =>
      null;
    end Iterate_For;

  begin -- Iterate_Over_Leaf_Directories
    FS.Search (Directory => From_Directory,
               Pattern   => "",
               Filter    => (FS.Directory => True, others => False),
               Process   => Iterate_For'access);
    if The_Count = 0 then
      Iterator (From_Directory);
    end if;
  end Iterate_Over_Leaf_Directories;


  function Found_Directory (Simple_Name  : String;
                            In_Directory : String) return String is

    The_Handle : FS.Search_Type;
    The_Entry  : FS.Directory_Entry_Type;

  begin
    FS.Start_Search (Search    => The_Handle,
                     Directory => In_Directory,
                     Pattern   => "",
                     Filter    => (FS.Directory => True, others => False));
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Name      : constant String := FS.Simple_Name (The_Entry);
        Directory : constant String := FS.Full_Name (The_Entry);
      begin
        if Strings.Is_Equal (Name, Simple_Name) then
          return Directory;
        elsif Name(Name'first) = '.' then
          null;
        else
          begin
            return Found_Directory (Simple_Name  => Simple_Name,
                                    In_Directory => Directory);
          exception
          when others =>
            null;
          end;
        end if;
      end;
    end loop;
    raise Not_Found;
  end Found_Directory;


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
                     Filter    => (FS.Ordinary_File => True, others => False));
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
