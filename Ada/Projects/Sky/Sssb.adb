-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Angle;
with Application;
with Data;
with Error;
with File;
with Strings;
with Traces;
with Values;

package body Sssb is

  package Log is new Traces ("Sssb");

  type Place is record
    Ut  : Time.Ut       := 0.0;
    Ra  : Angle.Degrees := 0.0;
    Dec : Angle.Degrees := 0.0;
  end record;

  package Places is new Ada.Containers.Doubly_Linked_Lists (Place);

  type Object is record
    Id        : access String;
    Ephemeris : Places.List;
  end record;

  package Objects is new Ada.Containers.Doubly_Linked_Lists (Object);

  The_Objects : Objects.List;


  procedure Read (Target   : String;
                  Filename : String) is

    type Header is (Unused_Universal_Time,
                    Julian_Date,
                    Unused_Solar_Presence,
                    Unused_Lunar_Presence,
                    Right_Ascension,
                    Declination,
                    Unused_Line_End);

    The_File : Ada.Text_IO.File_Type;

    Start_Time : constant Time.Ut := Time.Universal;

    In_Data_Part : Boolean := False;

    The_Place_Before : Place;
    Has_Place_Before : Boolean := False;

    The_Object : Object;

  begin -- Read_Objects
    Log.Write ("Read " & Target & " from file " & Filename);
    Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename);
    while not Ada.Text_IO.End_Of_File (The_File) loop
      declare

        Line  : constant String := Strings.Trimmed (Ada.Text_IO.Get_Line (The_File));
        Parts : constant Strings.Item := Strings.Item_Of (Line, ',');

        function Image_Of (Item : Header) return String is
        begin
          return Strings.Trimmed (Parts(Strings.First_Index + Header'pos(Item)));
        end Image_Of;

      begin
        if Line'length > 1024 then
          Error.Raise_With ("Incorrect line ending in " & Filename);
        elsif Line = "$$SOE" then
          In_Data_Part := True;
        elsif Line = "$$EOE" then
          In_Data_Part := False;
        elsif In_Data_Part and then Parts.Count = (Header'pos(Header'last) + 1) then
          declare
            Ut  : constant Time.Ut       := Time.Ut_Of (Image_Of (Julian_Date));
            Ra  : constant Angle.Degrees := Angle.Degrees'value(Image_Of (Right_Ascension));
            Dec : constant Angle.Degrees := Angle.Degrees'value(Image_Of (Declination));
          begin
            if Ut >= Start_Time then
              if Has_Place_Before then
                The_Object.Ephemeris := [The_Place_Before];
              end if;
              The_Object.Ephemeris.Append (Place'(Ut => Ut, Ra => Ra, Dec => Dec));
            else
              The_Place_Before := (Ut => Ut, Ra => Ra, Dec => Dec);
              Has_Place_Before := True;
            end if;
          end;
        elsif Line'length > 0 and then Line(Line'first) = '>' then
          exit when not Places.Is_Empty (The_Object.Ephemeris);
        end if;
      end;
    end loop;
    The_Object.Id := new String'(Target);
    The_Objects.Append (The_Object);
    Ada.Text_IO.Close (The_File);
  exception
  when Error.Occurred =>
    raise;
  when others =>
    Error.Raise_With ("Incorrect Data in " & Filename);
  end Read;


  function Exists (Target : String) return Boolean is
    Filename : constant String := Application.Composure (Target, "sssb");
  begin
    if File.Exists (Filename) then
      Read (Target, Filename);
      return True;
    end if;
    return False;
  end Exists;


  function Direction_Of (Item : Name.Id;
                         Ut   : Time.Ut) return Space.Direction is

    Item_Name : constant String := Name.Image_Of (Item);

    The_Place_Before : Place;
    Has_Place_Before : Boolean := False;

    Ra  : Angle.Degrees;
    Dec : Angle.Degrees;

  begin
    for The_Object of The_Objects loop
      if Item_Name = The_Object.Id.all then
        for The_Place of The_Object.Ephemeris loop
          if The_Place.Ut > Ut then
            if Has_Place_Before then
              Ra := Values.Interpolation_Of (T  => Ut,
                                             T1 => The_Place_Before.Ut,
                                             T2 => The_Place.Ut,
                                             V1 => The_Place_Before.Ra,
                                             V2 => The_Place.Ra);
              Dec := Values.Interpolation_Of (T  => Ut,
                                              T1 => The_Place_Before.Ut,
                                              T2 => The_Place.Ut,
                                              V1 => The_Place_Before.Dec,
                                              V2 => The_Place.Dec);
            else
              Ra := The_Place.Ra;
              Dec := The_Place.Dec;
            end if;
            Data.Apparent (Ra => Ra, Dec => Dec);
            return Space.Direction_Of (Ra  => Ra,
                                       Dec => Dec);
          else
            The_Place_Before := The_Place;
            Has_Place_Before := True;
          end if;
        end loop;
      end if;
    end loop;
    return Space.Unknown_Direction;
  end Direction_Of;

end Sssb;
