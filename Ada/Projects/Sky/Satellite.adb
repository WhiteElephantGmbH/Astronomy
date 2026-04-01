-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Ada.Text_IO;
with Ada.Directories;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with Stellarium;
with Traces;
with Text;

package body Satellite is

  package Log is new Traces ("Satellite");

  Json_Filename : constant String := Stellarium.Satellites_Filename;

  function Json_Data return Ada.Strings.Unbounded.Unbounded_String is

    Json_Filesize : constant Natural := Natural(Ada.Directories.Size(Json_Filename));

    The_Data : Ada.Strings.Unbounded.Unbounded_String;

    File : Ada.Text_IO.File_Type;

  begin
    Log.Write ("Filesize:" & Json_Filesize'image);
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Json_Filename);
    while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Strings.Unbounded.Append (The_Data, Ada.Text_IO.Get_Line (File) & Ascii.Cr);
    end loop;
    Ada.Text_IO.Close (File);
    return The_Data;
  end Json_Data;


  type Data is record
    Name    : Text.String;
    Element : Tle;
  end record;

  use type Number;

  package Tle_Data is new Ada.Containers.Ordered_Maps (Key_Type     => Number,
                                                       Element_Type => Data);
  Tle_Map : Tle_Data.Map;

  The_Numbers : Numbers.Set := [];


  procedure Read_Data is
  begin
    if Json_Filename = "" then
      Log.Error ("No data");
    end if;
    declare
      package JS renames GNATCOLL.JSON;

      Js_Data    : constant JS.JSON_Value := JS.Read (Json_Data);
      Creator    : constant JS.JSON_Value := Js_Data.Get ("creator");
      Satellites : constant JS.JSON_Value := Js_Data.Get ("satellites");

      procedure Handle_Satellite (Unused : JS.UTF8_String;
                                  Value  : JS.JSON_Value) is
        Is_Visible : constant Boolean := Value.Get ("visible");
        Groups     : constant JS.JSON_Array := Value.Get ("groups");
        use type Numbers.Set;
      begin
        if Is_Visible then
          for Group of Groups loop
            if Group.Get in Stellarium.Satellite_Group then
              declare
                Item : constant Data := (Name    => [Value.Get ("name")],
                                         Element => [1 => Value.Get ("tle1"),
                                                     2 => Value.Get ("tle2")]);
                Key : constant Number := Norad.Number_Of (Item.Element);
              begin
                if not Tle_Map.Contains (Key) and then not Norad.Is_In_Deep_Space (Item.Element) then
                  Tle_Map.Insert (Key, Item);
                  The_Numbers := @ + Key;
                end if;
              end;
              return;
            end if;
          end loop;
        end if;
      end Handle_Satellite;

    begin
      Log.Write (Creator.Get);
      JS.Map_JSON_Object (Satellites, Handle_Satellite'access);
      Log.Write ("Number of visible satellites:" & Tle_Map.Length'image);
    end;
  exception
  when Item: others =>
    Log.Termination (Item);
  end Read_Data;


  function Targets return Numbers.Set is
  begin
    return The_Numbers;
  end Targets;


  function Tle_Of (Object : Number) return Tle is
  begin
    return Tle_Map.Element (Object).Element;
  end Tle_Of;


  function Tle_Name_Of (Object : Number) return String is
  begin
    return Tle_Map.Element(Object).Name.S;
  end Tle_Name_Of;


  function Name_Of (Object : Number) return String is
  begin
    return Tle_Name_Of (Object) & " #" & Text.Trimmed (Object'image);
  end Name_Of;

end Satellite;
