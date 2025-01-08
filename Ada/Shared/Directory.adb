-- *********************************************************************************************************************
-- *                       (c) 2024 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Directory is

  package FS renames Ada.Directories;

  function Exists (Name : String) return Boolean is
    use type FS.File_Kind;
  begin
    return FS.Kind (Name) = FS.Directory;
  exception
  when others =>
    return False;
  end Exists;


  procedure Create (Path : String) is
  begin
    Ada.Directories.Create_Path (Path);
  end Create;


  procedure Delete (Name : String) is
  begin
    FS.Delete_Tree (Name);
  exception
  when Name_Error =>
    null;
  end Delete;


  function Is_Leaf (Name       : String;
                    Exceptions : Text.Strings := Text.None) return Boolean is

    The_Count : Natural := 0;

    procedure Iterate_For (Directory_Entry : FS.Directory_Entry_Type) is
    begin
      declare
        Simple_Name : constant String := FS.Simple_Name(Directory_Entry);
      begin
        if Simple_Name(Simple_Name'first) /= '.' and then not Exceptions.Contains (Simple_Name) then
          The_Count := The_Count + 1;
        end if;
      end;
    exception
    when others =>
      null;
    end Iterate_For;

  begin -- Is_Leaf_Directory
    FS.Search (Directory => Name,
               Pattern   => "",
               Filter    => [FS.Directory => True, others => False],
               Process   => Iterate_For'access);
    return The_Count = 0;
  exception
  when others =>
    return False;
  end Is_Leaf;


  procedure Iterate_Over_Leafs (From_Directory : String;
                                Iterator       : access procedure (Leaf_Directory : String);
                                Exceptions     : Text.Strings := Text.None) is
    The_Count : Natural := 0;

    procedure Iterate_For (Directory_Entry : FS.Directory_Entry_Type) is
    begin
      declare
        Name      : constant String := FS.Simple_Name(Directory_Entry);
        Full_Name : constant String := FS.Full_Name(Directory_Entry);
      begin
        if Name(Name'first) /= '.' and then not Exceptions.Contains (Name) then
          The_Count := The_Count + 1;
          Iterate_Over_Leafs (Full_Name, Iterator, Exceptions);
        end if;
      end;
    exception
    when others =>
      null;
    end Iterate_For;

  begin -- Iterate_Over_Leaf_Directories
    FS.Search (Directory => From_Directory,
               Pattern   => "",
               Filter    => [FS.Directory => True, others => False],
               Process   => Iterate_For'access);
    if The_Count = 0 then
      Iterator (From_Directory);
    end if;
  end Iterate_Over_Leafs;


  function Found (Name         : String;
                  In_Directory : String) return String is

    The_Handle : FS.Search_Type;
    The_Entry  : FS.Directory_Entry_Type;

  begin
    FS.Start_Search (Search    => The_Handle,
                     Directory => In_Directory,
                     Pattern   => "",
                     Filter    => [FS.Directory => True, others => False]);
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Simple_Name : constant String := FS.Simple_Name (The_Entry);
        Full_Name   : constant String := FS.Full_Name (The_Entry);
      begin
        if Text.Matches (Simple_Name, Name) then
          return Full_Name;
        elsif Simple_Name(Simple_Name'first) = '.' then
          null;
        else
          begin
            return Found (Name         => Name,
                          In_Directory => Full_Name);
          exception
          when others =>
            null;
          end;
        end if;
      end;
    end loop;
    raise Not_Found;
  end Found;


  -----------------------------
  -- Directory Iterator Loop --
  -----------------------------

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
                     Filter    => [FS.Directory => True, others => False]);
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

end Directory;
