-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Direct_IO;
with Ada.Directories;
with Ada.Containers.Indefinite_Ordered_Maps;
with GNATCOLL.JSON;
with Stellarium;
with Traces;

package body Satellite is

  package Log is new Traces ("Satellite");

  Json_Filename : constant String := Stellarium.Satellites_Filename;

  Json_Filesize : constant  Natural := (if Json_Filename = "" then 0 else Natural(Ada.Directories.Size(Json_Filename)));

  subtype Json_String is String (1 .. Json_Filesize);

  Json_Data : Json_String;

  package Json_Io is new Ada.Direct_IO (Json_String);


  procedure Read_Json_Data is
    File : Json_Io.File_Type;
  begin
    Json_Io.Open (File, Json_Io.In_File, Json_Filename);
    Json_Io.Read (File, Json_Data);
    Json_Io.Close (File);
  end Read_Json_Data;


  function "=" (Unused_Left, Unused_Right : Tle) return Boolean is
  begin
    return False;
  end "=";


  package Tle_Data is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                                  Element_Type => Tle);
  Tle_Map : Tle_Data.Map;


  procedure Build_Satellite_Data is

    package JS renames GNATCOLL.JSON;

    Data       : constant JS.JSON_Value := JS.Read (Json_Data);
    Creator    : constant JS.JSON_Value := Data.Get ("creator");
    Satellites : constant JS.JSON_Value := Data.Get ("satellites");

    procedure Handle_Satellite (Unused : JS.UTF8_String;
                                Value  : JS.JSON_Value) is
      Is_Visible    : constant Boolean := Value.Get ("visible");
      Groups        : constant JS.JSON_Array := Value.Get ("groups");
      Std_Mag       : constant JS.JSON_Value := Value.Get ("stdMag");
      The_Magnitude : Stellarium.Magnitude;
      use type Stellarium.Magnitude;
    begin
      if Is_Visible then
        case Std_Mag.Kind is
        when JS.JSON_Int_Type =>
          The_Magnitude := Stellarium.Magnitude(Integer'(Std_Mag.Get));
        when JS.JSON_Float_Type =>
          The_Magnitude := Stellarium.Magnitude(Float'(Std_Mag.Get));
        when others =>
          null;
        end case;
        if The_Magnitude <= Stellarium.Magnitude_Maximum then
          for Group of Groups loop
            if Group.Get in Stellarium.Satellite_Group then
              declare
                Name   : constant String := Value.Get ("name");
                Values : constant Tle    := [1 => Value.Get ("tle1"),
                                             2 => Value.Get ("tle2")];
              begin
                if not Tle_Map.Contains (Name) and then not Norad.Is_In_Deep_Space (Values) then
                  Tle_Map.Insert (Name, Values);
                end if;
              end;
              return;
            end if;
          end loop;
        end if;
      end if;
    end Handle_Satellite;

  begin
    Log.Write (Creator.Get);
    JS.Map_JSON_Object (Satellites, Handle_Satellite'access);
    Log.Write ("Number of visible satellites:" & Tle_Map.Length'image);
  end Build_Satellite_Data;


  procedure Read_Stellarium_Data is
  begin
    if Json_Filesize > 0 then
      Log.Write ("Filesize:" & Json_Filesize'image);
      Read_Json_Data;
      Build_Satellite_Data;
    end if;
  end Read_Stellarium_Data;


  function Names return Strings.List is

    The_Names : Strings.List;

    procedure Add (Position : Tle_Data.Cursor) is
    begin
      The_Names.Append (Tle_Data.Key (Position));
    end Add;

  begin -- Names
    Tle_Map.Iterate (Add'access);
    return The_Names;
  end Names;


  function Exists (Name : String) return Boolean is
  begin
    return Tle_Map.Contains (Name);
  end Exists;


  function Tle_Of (Name : String) return Tle is
  begin
    return Tle_Map.Element (Name);
  end Tle_Of;

end Satellite;
