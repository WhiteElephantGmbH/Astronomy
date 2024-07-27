-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

  Max_Latitude_Length  : constant Natural := Feature_Latitude'last'image'length;
  Max_Longitude_Length : constant Natural := Feature_Longitude'last'image'length;
  Max_Size_Length      : constant Natural := Feature_Size'last'image'length;

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

  The_Features  : Feature_Data.List;
  Max_Kind_Length : Natural := 0;

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
        when I_OC =>
          The_Feature.Kind := Oceanus;
        when I_PA =>
          The_Feature.Kind := Palus;
        when I_PR =>
          The_Feature.Kind := Promontorium;
        when I_RI =>
          The_Feature.Kind := Rima;
        when I_RU =>
          The_Feature.Kind := Rupes;
        when I_DO | I_LF | I_PL | I_SF =>
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

      if The_Feature.Kind'image'length > Max_Kind_Length then
        Max_Kind_Length := The_Feature.Kind'image'length;
      end if;
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
      Put_Line ("Encountered" & The_Features.Length'image & " features");
    exception
    when Item: others =>
      Ada.Text_IO.Close (File);
      Log.Termination (Item);
    end;
  end Read;


  procedure Generate is

    Filename : constant String := Data_Folder & "Database-Moon.ads";

    File : IO.File_Type;

    procedure Output (Line : String := "") is
    begin
      IO.Put_Line (File, Line);
    end Output;


    procedure Put_Header is
    begin
      pragma Style_Checks ("M138");
      Output ("-- *********************************************************************************************************************");
      Output ("-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *");
      Output ("-- *                                               www.white-elephant.ch                                               *");
      Output ("-- *********************************************************************************************************************");
      pragma Style_Checks ("M120");
      Output ("-- Generated Moon feature information, obtained from the web page https://planetarynames.wr.usgs.gov");
      Output;
      Output ("pragma Style_White_Elephant;");
      Output;
      Output ("pragma Restrictions (No_Elaboration_Code);");
      Output;
      Output ("package Database.Moon is");
    end Put_Header;


    procedure Put_Feature_Ids is
    begin
      Output;
      Output ("  type Feature_Name is (");
      for The_Item of The_Features loop
        Output ("    " & The_Item.Id.To_String & (if The_Item.Id = The_Features.Last_Element.Id then ");" else ","));
      end loop;
    end Put_Feature_Ids;


    procedure Put_Data is

      function Feature_Image (The       : Feature;
                              Separator : String) return String is

        function Left_Adjusted (Item       : String;
                                Field_Size : Positive) return String is
          Postfix : constant String(1 .. Field_Size - Item'length) := [others => ' '];
        begin
          return Item & Postfix;
        end Left_Adjusted;

        function Right_Adjusted (Item       : String;
                                 Field_Size : Positive) return String is
          Prefix : constant String(1 .. Field_Size - Item'length) := [others => ' '];
        begin
          return Prefix & Item;
        end Right_Adjusted;

        function Kind return String is
        begin
          return Text.Capitalized_Of (Left_Adjusted (The.Kind'image & ',', Max_Kind_Length + 2));
        end Kind;

        function Latitude return String is
        begin
          return Text.Capitalized_Of (Right_Adjusted (The.Latitude'image , Max_Latitude_Length));
        end Latitude;

        function Longitude return String is
        begin
          return Text.Capitalized_Of (Right_Adjusted (The.Longitude'image , Max_Longitude_Length));
        end Longitude;

        function Size return String is
        begin
          return Text.Capitalized_Of (Right_Adjusted (The.Size'image, Max_Size_Length));
        end Size;

        function Name return String is
        begin
          return The.Name.To_String;
        end Name;

      begin -- Feature_Image
        return Kind & Latitude & ',' & Longitude & ',' & Size & Separator & " -- " & Name;
      end Feature_Image;

    begin -- Put_Data
      Output;
      Output ("  type Feature is record");
      Output ("    Kind      : Moon_Feature_Type;");
      Output ("    Latitude  : Feature_Latitude;");
      Output ("    Longitude : Feature_Longitude;");
      Output ("    Size      : Feature_Size;");
      Output ("  end record;");
      Output;
      Output ("  type Features is array (Feature_Name) of Feature;");
      Output;
      Output ("  type Data is array (Feature_Name) of Feature;");
      Output;
      Output ("  List : constant Data := [");
      Output ("  --   Kind           Latitude    Longitude      Size");
      for Item of The_Features loop
        Output ("    (" & Feature_Image (Item, (if Item.Id = The_Features.Last_Element.Id then ")];" else "), ")));
      end loop;
    end Put_Data;

    procedure Put_Footer is
    begin
      Output;
      Output ("end Database.Moon;");
    end Put_Footer;

  begin -- Generate
    Put_Line ("Create " & Filename);
    IO.Create (File, Name => Filename);
    Put_Header;
    Put_Feature_Ids;
    Put_Data;
    Put_Footer;
    IO.Close (File);
    Put_Line (Filename & " created");
  exception
  when others =>
    IO.Close (File);
    raise;
  end Generate;

end Generator.Data;
