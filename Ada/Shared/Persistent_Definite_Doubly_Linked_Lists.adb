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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Application;
with File;
with Log;

package body Persistent_Definite_Doubly_Linked_Lists is

  package IO renames Ada.Streams.Stream_IO;

  Data_Directory : constant String := Application.Composure ("Data");

  Filename : constant String := File.Composure (Data_Directory, Name, "bin");


  procedure Initialize (The_Data : in out Data) is
    The_File   : IO.File_Type;
    The_Stream : IO.Stream_Access;
  begin
    begin
      IO.Open (File => The_File,
               Name => Filename,
               Mode => IO.In_File);
    exception
    when IO.Name_Error =>
      return;
    end;
    The_Stream := IO.Stream (The_File);
    begin
      The_Data.List := Definite_List.Item'input(The_Stream);
      IO.Close (The_File);
      Log.Write (Name & " data initialized");
    exception
    when others => -- old structure
      IO.Close (The_File);
      File.Delete (Filename);
    end;
  exception
  when Item: others =>
    Log.Write ("Persistent_Definite_Doubly_Linked_Lists.Initialize", Item);
  end Initialize;


  procedure Finalize (The_Data : in out Data) is
    The_File   : IO.File_Type;
    The_Stream : IO.Stream_Access;
  begin
    begin
      IO.Open (File => The_File,
               Name => Filename,
               Mode => IO.Out_File);
    exception
    when others =>
      Ada.Directories.Create_Path (Data_Directory);
      IO.Create (File => The_File,
                 Name => Filename,
                 Mode => IO.Out_File);
    end;
    IO.Reset (The_File);
    The_Stream := IO.Stream (The_File);
    Definite_List.Item'output(The_Stream, The_Data.List);
    IO.Close (The_File);
    Log.Write (Name & " data finalized");
  exception
  when Item: others =>
    Log.Write ("Persistent_Definite_Doubly_Linked_Lists.Finalize", Item);
  end Finalize;

end Persistent_Definite_Doubly_Linked_Lists;
