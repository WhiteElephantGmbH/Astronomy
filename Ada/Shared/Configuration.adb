-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Text_IO;
with Text;

package body Configuration is

  function Found_Section (Name    : String;
                          In_File : File_Handle) return Section is
  begin
    for The_Section of In_File.all loop
      if Text.Is_Equal (The_Section.Name, Name) then
        return The_Section;
      end if;
    end loop;
    return null;
  exception
  when others =>
    return null;
  end Found_Section;


  function Found_Item (Key        : String;
                       In_Section : Section_Handle) return Item is
  begin
    for The_Item of In_Section.all loop
      if Text.Is_Equal (The_Item.Key, Key) then
        return The_Item;
      end if;
    end loop;
    return null;
  exception
  when others =>
    return null;
  end Found_Item;


  function Handle_For (File_Name : String) return File_Handle is

    package IO renames Ada.Text_IO;

    The_File : IO.File_Type;

    The_Handle  : constant File_Handle := new Sections.List;
    The_Section : Section;

  begin
    IO.Open (File => The_File,
             Mode => IO.In_File,
             Name => File_Name);
    declare
      Has_Bom : constant Boolean := Strings.Has_Skipped_Bom_8 (The_File);
    begin
      while not IO.End_Of_File (The_File) loop
        declare
          Trimmed_Line : constant String := Strings.Trimmed (Ada.Text_IO.Get_Line (The_File));
          Line         : constant String := (if Has_Bom then Trimmed_Line else Strings.Utf8_Of (Trimmed_Line));
        begin
          if Line'length > 1 then
            case Line(Line'first) is
            when '[' =>
              if Line(Line'last) = ']' then -- section
                declare
                  Section_Name : constant String := Line (Line'first + 1 .. Line'last - 1);
                begin
                  The_Section := Found_Section (Section_Name, The_Handle);
                  if The_Section = null then
                    The_Section := new Section_Data'(Name_Length => Section_Name'length,
                                                     Name        => Section_Name,
                                                     Items       => new Items.List);
                    The_Handle.Append (The_Section);
                  end if;
                end;
              else
                exit;
              end if;
            when ';' | '#' =>
              null;
            when others =>
              if The_Section = null then
                The_Section := new Section_Data'(Name_Length => No_Section'length,
                                                 Name        => No_Section,
                                                 Items       => new Items.List);
                The_Handle.Append (The_Section);
              end if;
              declare
                Separator_Index : constant Natural := Text.Location_Of ('=', Line);
              begin
                if Separator_Index = Text.Not_Found then
                  exit;
                else
                  declare
                    Key   : constant String := Strings.Trimmed (Line(Line'first .. Separator_Index - 1));
                    Value : constant String := Strings.Trimmed (Line(Separator_Index + 1 .. Line'last));
                  begin
                    if Found_Item (Key, The_Section.Items) = null then
                      The_Section.Items.Append (new Item_Data'(Key_Length   => Key'length,
                                                               Key          => Key,
                                                               Value_Length => Value'length,
                                                               Value        => Value));
                    end if;
                  end;
                end if;
              end;
            end case;
          end if;
        end;
      end loop;
    exception
    when others =>
      null;
    end;
    IO.Close (The_File);
    return The_Handle;
  exception
  when others =>
    return The_Handle;
  end Handle_For;


  function Handle_For (For_Handle   : File_Handle;
                       Section_Name : String := No_Section) return Section_Handle is
    The_Section : Section;
  begin
    The_Section := Found_Section (Section_Name, For_Handle);
    return The_Section.Items;
  exception
  when others =>
    return null;
  end Handle_For;


  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : String := "") return String is
  begin
    declare
      Image : constant String := Found_Item (Key, The_Section).Value;
    begin
      if Image'length > 1 and then Image(Image'first) = '"' and then Image(Image'last) = '"' then
        return Image(Image'first + 1 ..Image'last - 1);
      end if;
      return Image;
    end;
  exception
  when others =>
    return Default;
  end Value_Of;


  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : List := []) return List is
  begin
    return Strings.Item_Of (Found_Item (Key, The_Section).Value, Separator => ',', Purge => True).To_Trimmed_List;
  exception
  when others =>
    return Default;
  end Value_Of;


  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : Integer := 0) return Integer is
  begin
    return Integer'value(Found_Item (Key, The_Section).Value);
  exception
  when others =>
    return Default;
  end Value_Of;


  function Value_Of (The_Section : Section_Handle;
                     Key         : String;
                     Default     : Boolean := False) return Boolean is
  begin
    return Boolean'value(Found_Item (Key, The_Section).Value);
  exception
  when others =>
    return Default;
  end Value_Of;

end Configuration;
