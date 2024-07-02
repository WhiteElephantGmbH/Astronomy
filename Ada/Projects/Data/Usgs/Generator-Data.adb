-- *********************************************************************************************************************
-- *                           (c) 2024  by White Elephant GmbH, Schaffhausen, Switzerland                             *
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
with Database;
with Traces;
with Text;

package body Generator.Data is

  package Log is new Traces ("Generator.Data");

  type Id is (I_AA, I_AL, I_CA, I_DO, I_LC, I_LF, I_ME, I_MO, I_OC, I_PA, I_PL, I_PR, I_RI, I_RU, I_SF, I_SI, I_VA);

  subtype Feature_Type      is Database.Moon_Feature_Type;
  subtype Feature_Latitude  is Database.Feature_Latitude;
  subtype Feature_Longitude is Database.Feature_Longitude;
  subtype Feature_Size      is Database.Feature_Size;

  use all type Feature_Type;

  type Feature is record
    Id        : Text.String;
    Name      : Text.String;
    Kind      : Feature_Type;
    Latitude  : Feature_Latitude;
    Longitude : Feature_Longitude;
    Size      : Feature_Size;
  end record;

  package Feature_Data is new Ada.Containers.Doubly_Linked_Lists (Feature);

  use type Text.String;

  function "<" (Left, Right : Feature) return Boolean is (Left.Id < Right.Id);

  package Features is new Feature_Data.Generic_Sorting ("<" => "<");

  The_Features : Feature_Data.List;

  procedure Read is

    Filename : constant String := Usgs_Folder & "MoonObjects.txt";

    File : Ada.Text_IO.File_Type;

    The_Feature : Feature;


    procedure Generate_Name_Id is

      Ansi_Name : constant String := Text.Ansi_Of_Utf8 (+The_Feature.Name);

      Last_Is_Separator : Boolean := True;

    begin
      The_Feature.Id.Clear;
      for The_Character of Ansi_Name loop
        case The_Character is
        when 'á' =>
          The_Feature.Id.Append ("a");
        when 'ä' =>
          The_Feature.Id.Append ("ae");
        when 'è' | 'é'  =>
          The_Feature.Id.Append ("e");
        when 'î' =>
          The_Feature.Id.Append ("i");
        when 'ö' =>
          The_Feature.Id.Append ("oe");
        when 'ü' =>
          The_Feature.Id.Append ("ue");
        when 'a' .. 'z' | 'A' .. 'Z' =>
          if Last_Is_Separator then
            The_Feature.Id.Append (Text.Uppercase_Of (The_Character));
            Last_Is_Separator := False;
          else
            The_Feature.Id.Append (Text.Lowercase_Of (The_Character));
          end if;
        when '0' .. '9' =>
          if Last_Is_Separator then
            Error ("Illegal Number in " & The_Feature.Name);
          end if;
        when ' ' | ''' | '.' | '-' =>
          if not Last_Is_Separator then
            The_Feature.Id.Append ('_');
          end if;
          Last_Is_Separator := True;
        when '(' =>
          exit;
        when others =>
          Error ("Illegal Name in " & Ansi_Name);
        end case;
      end loop;
      if Last_Is_Separator then
        The_Feature.Id.Delete (The_Feature.Id.Count);
      end if;
    end Generate_Name_Id;


    procedure Add_Next_Feature is

      Line  : constant String := IO.Get_Line (File);
      Index : Natural := Line'first;

      procedure Skip_Until (Item : Character) is
      begin
        while Line(Index) /= Item loop
          Index := @ + 1;
        end loop;
      end Skip_Until;

      function Next_Until (Item : Character) return String is
        First : Natural;
      begin
        Index := @ + 1;
        First := Index;
        while Line(Index) /= Item loop
          Index := @ + 1;
        end loop;
        return Line (First .. Index - 1);
      end Next_Until;

    begin -- Add_Next_Object
      The_Line_Number := @ + 1;
      if Line(Index) = '#' then
        return;
      end if;

      Skip_Until ('"');
      The_Feature.Name := [Next_Until ('"')];
      Skip_Until (Ascii.Ht);

      declare
        Id_Name : constant String := Next_Until (Ascii.Ht);
      begin
        case Id'value("I_" & Id_Name) is
        when I_AA =>
          The_Feature.Kind := Crater;
        when I_AL =>
          The_Feature.Kind := Swirl; -- Albedo Feature
        when I_CA =>
          The_Feature.Kind := Catena;
        when I_LC =>
          The_Feature.Kind := Lacus;
        when I_ME =>
          The_Feature.Kind := Mare;
        when I_MO =>
          The_Feature.Kind := Mons;
        when I_PA =>
          The_Feature.Kind := Palus;
        when I_PR =>
          The_Feature.Kind := Promontorium;
        when I_RI =>
          The_Feature.Kind := Rima;
        when I_RU =>
          The_Feature.Kind := Rupes;
        when I_DO | I_LF | I_OC | I_PL | I_SF =>
          return; -- not supported
        when I_SI =>
          The_Feature.Kind := Sinus;
        when I_VA =>
          The_Feature.Kind := Vallis;
        end case;
      exception
      when others =>
        Error ("Unknown ID: " & Id_Name);
      end;
      Generate_Name_Id;
      if Text.Is_Utf8_Encoded (+The_Feature.Id) then
        Error ("Illegal Name" & The_Feature.Name);
      end if;

      declare
        Latitude : constant String := Next_Until (Ascii.Ht);
      begin
        The_Feature.Latitude := Feature_Latitude'value(Latitude);
      exception
      when others =>
        Error ("Illegal Latitude for " & The_Feature.Name);
      end;

      declare
        Longitude : constant String := Next_Until (Ascii.Ht);
        use type Feature_Latitude;
        use type Feature_Longitude;
        Upper_Limmit : Feature_Longitude;
      begin
        The_Feature.Longitude := Feature_Longitude'value(Longitude);
        if (abs The_Feature.Latitude) > 70.0 then
          Upper_Limmit := 120.0;
        else
          Upper_Limmit := 100.0;
        end if;
        if (The_Feature.Longitude > 100.0) and (The_Feature.Longitude < 360.0 - Upper_Limmit) then
          Log.Warning (String'(+The_Feature.Name) & " not visible - Longitude:" & Longitude);
          return;
        end if;
      exception
      when others =>
        Error ("Illegal Longitude for " & The_Feature.Name);
      end;

      declare
        Size : constant String := Line(Index + 1 .. Line'last);
        use type Feature_Size;
      begin
        The_Feature.Size := Feature_Size'value(Size);
        if The_Feature.Size < Database.Minimum_Feature_Size then
          Log.Warning (String'(+The_Feature.Name) & " too small - Size:" & Size);
          return;
        end if;
      exception
      when others =>
        Error ("!!! Illegal Size for " & The_Feature.Name);
      end;

      The_Features.Append (The_Feature);
    exception
    when Item: others =>
      Log.Termination (Item);
      Error (Line);
    end Add_Next_Feature;

  begin -- Read
    Put_Line ("Generate usgs data from " & Filename);
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
    begin
      while not Ada.Text_IO.End_Of_File (File) loop
        Add_Next_Feature;
      end loop;
      Ada.Text_IO.Close (File);
      Put_Line ("Sort...");
      Features.Sort (The_Features);
      for The_Item of The_Features loop
        Put_Line (String'(+The_Item.Id) & " (" & The_Item.Kind'image & ")");
      end loop;
      Put_Line ("Encountered" & The_Features.Length'image & " features");
    exception
    when Item: others =>
      Ada.Text_IO.Close (File);
      Log.Termination (Item);
    end;
  end Read;

end Generator.Data;
