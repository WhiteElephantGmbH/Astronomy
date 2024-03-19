-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.IO_Exceptions;
with Ada.Text_IO;
with Angle;
with Application;
with Error;
with File;
with Lexicon;
with Objects;
with Sky.Catalog;
with Sky.Data;
with Sssb;
with Targets;
with Text;
with Traces;

package body Name is

  package Log is new Traces ("Name");

  Support_Axis_Positions : Boolean := False;
  Support_Land_Marks     : Boolean := False;
  Neo_Exists             : access function (Id : String) return Boolean;

  type Element_Data (Item : Data_Kind) is record
    Kind      : Object_Kind;
    Number    : Natural;
    Name      : Text.String;
    Next      : Element_Access;
    case Item is
    when Axis_Position =>
      Position : Space.Direction;
    when Landmark =>
      Direction : Earth.Direction;
    when others =>
      Object : Sky.Object := Sky.Undefined;
    end case;
  end record;
  use type Text.String;


  function "=" (Left, Right : Id) return Boolean is
    use type Sky.Catalog_Id;
  begin
    if Left.Data_Id /= Right.Data_Id then
      return False;
    elsif Left.Data_Id = Sky.Favorites then
      if Left.Element /= null and  Right.Element /= null then
        return Left.Element.Number = Right.Element.Number;
      else
        return Left.Element = Right.Element;
      end if;
    else
      return Left.Element_Number = Right.Element_Number;
    end if;
  end "=";


  function "<" (Left, Right : Element_Access) return Boolean is
  begin
    return Left.Number < Right.Number;
  end "<";


  function "<" (Left, Right : Id) return Boolean is
    use type Sky.Catalog_Id;
  begin
    if Left.Data_Id = Sky.Favorites then
      return Left.Element < Right.Element;
    else
      return Left.Element_Number < Right.Element_Number;
    end if;
  end "<";


  function Is_Known (Item : Id) return Boolean is
    use type Sky.Catalog_Id;
  begin
    return Item.Data_Id /= Sky.Favorites or else Item.Element /= null;
  end Is_Known;


  function Is_Visible (Item : Id) return Boolean is
  begin
    return Item.Is_Visible;
  end Is_Visible;


  function Image_Of (Item : Id) return String is
  begin
    case Item.Data_Id is
    when Sky.Favorites =>
      return +Item.Element.Name;
    when Sky.Extended_Catalogs =>
      return Sky.Data.Name_Of (Item.Element_Number, Item.Data_Id);
    end case;
  end Image_Of;


  function Matches (Item   : Id;
                    Number : Natural) return Boolean is
  begin
    if Item.Is_Visible then
      return Item.Element.Number = Number;
    end if;
    return False;
  end Matches;


  function Kind_Of (Item : Id) return Object_Kind is
  begin
    case Item.Data_Id is
    when Sky.Favorites =>
      return Item.Element.Kind;
    when Sky.Neo =>
      return Near_Earth_Object;
    when others =>
      return Sky_Object;
    end case;
  end Kind_Of;


  function Object_Of (Item : Id) return Sky.Object is
  begin
    case Item.Data_Id is
    when Sky.Favorites =>
      return Item.Element.Object;
    when others =>
      return Sky.Data.Object_Of (Item.Element_Number, Item.Data_Id);
    end case;
  end Object_Of;


  function Type_Of (Item : Id) return Sky.Object_Type is
  begin
    case Kind_Of (Item) is
    when Sky_Object =>
      return Sky.Data.Object_Type_Of (Object_Of (Item));
    when others =>
      return Sky.Unknown;
    end case;
  end Type_Of;


  function Direction_Of (Item : Id;
                         Ut   : Time.Ut) return Space.Direction is
  begin
    return Sky.Data.Direction_Of (Object_Of(Item), Ut);
  end Direction_Of;


  function Direction_Of (Item : Id) return Space.Direction is
  begin
    return Item.Element.Position;
  end Direction_Of;


  function Direction_Of (Item : Id) return Earth.Direction is
  begin
    return Item.Element.Direction;
  end Direction_Of;


  function Magnitude_Of (Item : Id) return Sky.Magnitude is
  begin
    if Kind_Of (Item) = Sky_Object then
      return Sky.Data.Magnitude_Of (Object_Of(Item));
    else
      return Sky.Unknown_Magnitude;
    end if;
  end Magnitude_Of;


  function Visibility_Changed_For (Item    : in out Id;
                                   Visible :        Boolean) return Boolean is
  begin
    Item.Is_Visible := Visible;
    return Item.Was_Visible /= Visible;
  end Visibility_Changed_For;


  procedure Clear_History_For (The_Targets : in out Id_List) is
  begin
    for The_Id of The_Targets.Ids loop
      The_Id.Was_Visible := False;
    end loop;
  end Clear_History_For;


  function Item_Of (List  : Id_List;
                    Image : String) return Id is
  begin
    for Item of List.Ids loop
      if Image = Image_Of (Item) then
        return Item;
      end if;
    end loop;
    return No_Id;
  end Item_Of;


  function Item_Of (List             : Id_List;
                    Direction        : Space.Direction;
                    Search_Tolerance : Space.Distance) return Id is
  begin
    declare
      Element : constant Sky.Data.Element := Sky.Data.Find_Element (Direction, Search_Tolerance);
      use type Sky.Data.Element;
    begin
      if Element /= Sky.Data.No_Element then
        return Name.Id'(Data_Id        => Element.Catalog,
                        Was_Visible    => True,
                        Is_Visible     => True,
                        Element_Number => Element.Number);
      end if;
    end;

    -- search others
    for Item of List.Ids loop

      declare

        function Found_Item (Direction_Of : Get_Space_Access) return Boolean is
        begin
          if Is_Visible (Item) then
            declare
              List_Direction : constant Space.Direction := Direction_Of (Item, Time.Universal);
            begin
              if Space.Angle_Between (Direction, List_Direction, Smaller_Than => Search_Tolerance) then
                Log.Write ("Found: in list: " & Space.Image_Of (List_Direction));
                Log.Write ("       actual : " & Space.Image_Of (Direction));
                return True;
              end if;
            end;
          end if;
          return False;
        end Found_Item;

      begin
        case Kind_Of (Item) is
        when Sky_Object =>
          if Found_Item (Direction_Of'access) then
            return Item;
          end if;
        when Moon =>
          if Found_Item (Targets.Moon_Direction_Of'access) then
            return Item;
          end if;
        when Name.Sun | Name.Planet =>
          if Found_Item (Targets.Solar_System_Direction_Of'access) then
            return Item;
          end if;
        when Name.Small_Solar_System_Body =>
          if Found_Item (Sssb.Direction_Of'access) then
            return Item;
          end if;
        when others =>
          null;
        end case;
      end;
    end loop;
    return No_Id;
  end Item_Of;


  procedure For_All (In_List : in out Id_List;
                     Handle  : access procedure (Item : in out Id)) is
  begin
    for The_Id of In_List.Ids loop
      Handle.all (The_Id);
    end loop;
  end For_All;


  procedure Update (The_Targets : Id_List_Access;
                    Remove      : access procedure (Index : Natural);
                    Insert      : access procedure (Item  : Id_Access; Index : Natural)) is
    List_Index : Natural := 0;

  begin
    for Item of The_Targets.Ids loop
      declare
        Item_Pointer : constant Id_Access := Item'unrestricted_access;
      begin
        if Item.Was_Visible then
          if Item.Is_Visible then
            List_Index := List_Index + 1;
          else
            Remove (List_Index + 1);
            Item.Was_Visible := False;
          end if;
        elsif Item.Is_Visible then
          List_Index := List_Index + 1;
          Insert (Item_Pointer, List_Index);
          Item.Was_Visible := True;
        end if;
      end;
    end loop;
  end Update;


  protected Actual_Catalog is

    procedure Read_Favorite_Catalog;

    procedure Define_Catalog (Data_Id : Sky.Catalog_Id);

    function List return Id_List;

  private
    The_Element_List : Element_Access;
    The_Id_List      : Id_List;
  end Actual_Catalog;


  function "=" (Left, Right : Id_List) return Boolean is
    use type Sky.Catalog_Id;
    use type Ada.Containers.Count_Type;
    use type Names.List;
  begin
    if Left.Ids.Length /= Right.Ids.Length then
      return False;
    elsif Left.Kind /= Right.Kind then
      return False;
    else
      return Left.Ids /= Right.Ids;
    end if;
  end "=";


  procedure Read_Favorites (Enable_Axis_Positions : Boolean;
                            Enable_Land_Marks     : Boolean;
                            Neo_Existing          : Neo_Exists_Handler := null) is
  begin
    Support_Axis_Positions := Enable_Axis_Positions;
    Support_Land_Marks := Enable_Land_Marks;
    Neo_Exists := Neo_Existing;
    Actual_Catalog.Read_Favorite_Catalog;
  end Read_Favorites;


  procedure Define (List : Sky.Catalog_Id) is
  begin
    Actual_Catalog.Define_Catalog (List);
  end Define;


  procedure Sort_Favorites (The_List : in out Id_List) is

    function Altitude_Of (Object : Id) return Angle.Signed is
      Space_Direction : constant Space.Direction := Direction_Of (Object, Time.Universal);
      Earth_Direction : constant Earth.Direction := Objects.Direction_Of (Space_Direction, Time.Lmst);
      use type Angle.Signed;
    begin
      return Angle.Signed'(+Earth.Alt_Of (Earth_Direction));
    end Altitude_Of;

    function Compare_Altitude (Left, Right : Id) return Boolean is
      use type Angle.Signed;
    begin
      if Kind_Of (Left) = Sky_Object and Kind_Of (Right) = Sky_Object then
        return Altitude_Of (Left) > Altitude_Of (Right);
      end if;
      return False; -- don't sort others than sky objects
    end Compare_Altitude;

    package Favorite_Tool is new Names.Generic_Sorting (Compare_Altitude);

  begin -- Sort
    case The_List.Kind is
    when Sky.Favorites =>
      Favorite_Tool.Sort (The_List.Ids);
    when others =>
      raise Program_Error;
    end case;
  end Sort_Favorites;


  function Actual_List return Id_List is
  begin
    return Actual_Catalog.List;
  end Actual_List;


  protected body Actual_Catalog is

    procedure Read_Favorite_Catalog is

      Filename : constant String := Application.Composure ("Favorites", "txt");

      function Stored (Line   : String;
                       Number : Positive) return Boolean is

        Parts : constant Text.Strings := Text.Strings_Of (Text.Trimmed (Line), Separator => '|');

        function Part_For (Index : Text.String_Index) return String is
        begin
          return Text.Trimmed (Parts(Index));
        end Part_For;

        The_Element : Element_Access;

      begin -- Stored
        if Parts.Count = 0 then
          return False;
        end if;
        declare -- Store
          Part_1  : constant String := Part_For (Text.First_Index);
          Parts_1 : constant Text.Strings := Text.Strings_Of (Part_1, Separator => ' ');

          function Pixels_Of (The_Value : Angle.Value) return Integer is
            use type Angle.Value;
            Value : constant Angle.Degrees := +The_Value;
            use type Angle.Degrees;
          begin
            return Integer(Value * 8192.0 / 360.0);
          end Pixels_Of;

          procedure Add_Axis_Position is
          begin
            if Parts.Count = 3 then
              declare
                Words         : constant Text.Strings := Parts_1.Part (Text.First_Index + 1, Parts_1.Count);
                Position_Name : constant String := Words.To_Data (Separator => " ");
                Ra_Image      : constant String := Part_For (Text.First_Index + 1);
                Dec_Image     : constant String := Part_For (Text.First_Index + 2);
              begin
                The_Element := new Element_Data (Axis_Position);
                The_Element.Kind := Axis_Position;
                The_Element.Number := Number;
                The_Element.Name := [Position_Name];
                declare
                  Ra  : constant Angle.Value := Angle.Value_Of (Ra_Image);
                  Dec : constant Angle.Value := Angle.Value_Of (Dec_Image);
                begin
                  The_Element.Position := Space.Direction_Of (Ra  => Ra,
                                                              Dec => Dec);
                end;
                return;
              exception
              when others =>
                null;
              end;
            end if;
            Error.Raise_With ("Incorrect Position - " & Line);
          end Add_Axis_Position;

          procedure Add_Landmark is
          begin
            if Parts.Count = 3 then
              declare
                Words     : constant Text.Strings := Parts_1.Part (Text.First_Index + 1, Parts_1.Count);
                Mark_Name : constant String := Words.To_Data (Separator => " ");
                Az_Image  : constant String := Part_For (Text.First_Index + 1);
                Alt_Image : constant String := Part_For (Text.First_Index + 2);
              begin
                The_Element := new Element_Data (Landmark);
                The_Element.Kind := Landmark;
                The_Element.Number := Number;
                The_Element.Name := [Mark_Name];
                declare
                  Az  : constant Angle.Value := Angle.Value_Of (Az_Image);
                  Alt : constant Angle.Value := Angle.Value_Of (Alt_Image);
                begin
                  The_Element.Direction := Earth.Direction_Of (Az  => Az,
                                                               Alt => Alt);
                  Log.Write ("Pixels of LM " & Mark_Name);
                  Log.Write ("  AZ :" & Integer'((6144 + Pixels_Of (Az)) mod 8192)'img);
                  Log.Write ("  ALT:" & Integer'(2048 - Pixels_Of (Alt))'img);
                end;
                return;
              exception
              when others =>
                null;
              end;
            end if;
            Error.Raise_With ("Incorrect Location - " & Line);
          end Add_Landmark;


          procedure Add_Sky_Object is
          begin
            The_Element := new Element_Data(Sky_Object);
            The_Element.Number := Number;
            if Parts.Count = 4 then
              declare
                Object      : constant String := Part_1;
                Description : constant String := Part_For (Text.First_Index + 1);
                Ra_Image    : constant String := Part_For (Text.First_Index + 2);
                Dec_Image   : constant String := Part_For (Text.First_Index + 3);
              begin
                The_Element.Kind := Sky_Object;
                The_Element.Name := [Object];
                The_Element.Object
                  := Sky.Data.New_Object_For (Item        => Object,
                                              Description => Description,
                                              Direction   => Space.Direction_Of (Ra_Image  => Ra_Image,
                                                                                 Dec_Image => Dec_Image));
              exception
              when others =>
                Error.Raise_With ("Incorrect Location - " & Line);
              end;
            else
              declare
                Object_Name : constant String := Part_For (Text.First_Index);
                use type Sky.Object;
              begin
                The_Element.Name := [Part_For (Parts.Count)];
                if Sssb.Exists (Object_Name) then
                  The_Element.Kind := Small_Solar_System_Body;
                  return;
                elsif Neo_Exists /= null and then Neo_Exists (Object_Name) then
                  The_Element.Kind := Near_Earth_Object;
                  The_Element.Object := Sky.Data.Neo_Object_Of (Object_Name);
                  return;
                end if;
                declare
                  The_Word : Lexicon.Word;
                begin
                  The_Word := Lexicon.Word_Of (Object_Name);
                  case The_Word is
                  when Lexicon.Sun =>
                    The_Element.Kind := Sun;
                    return;
                  when Lexicon.Mercury =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Venus =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Moon =>
                    The_Element.Kind := Moon;
                    return;
                  when Lexicon.Mars =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Jupiter =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Saturn =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Uranus =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Neptune =>
                    The_Element.Kind := Planet;
                    return;
                  when Lexicon.Pluto =>
                    The_Element.Kind := Planet;
                    return;
                  when others =>
                    null;
                  end case;
                exception
                when Lexicon.Not_Found =>
                  null;
                end;
                The_Element.Kind := Sky_Object;
                The_Element.Object := Sky.Catalog.Object_Of (Object_Name);
                if The_Element.Object = Sky.Undefined then
                  Error.Raise_With ("Unknown Name - " & Line);
                end if;
                The_Element.Name := [Sky.Catalog.Object_Image_Of (The_Element.Object, +@)];
              end;
            end if;
          end Add_Sky_Object;

        begin -- Store
          if Support_Axis_Positions and then Parts_1.Count >= 2 and then Parts_1(Text.First_Index) = "AP" then
            Add_Axis_Position;
          elsif Support_Land_Marks and then Parts_1.Count >= 2 and then Parts_1(Text.First_Index) = "LM" then
            Add_Landmark;
          else
            Add_Sky_Object;
          end if;
        end; -- Store
        if The_Element_List = null then
          The_Element_List := The_Element;
        elsif The_Element < The_Element_List then
          The_Element.Next := The_Element_List;
          The_Element_List := The_Element;
        else
          declare
            The_Element_Pointer : Element_Access := The_Element_List;
          begin
            loop
              if The_Element_Pointer.Next = null then
                The_Element_Pointer.Next := The_Element;
                exit;
              elsif The_Element < The_Element_Pointer.Next then
                The_Element.Next := The_Element_Pointer.Next;
                The_Element_Pointer.Next := The_Element;
                exit;
              else
                The_Element_Pointer := The_Element_Pointer.Next;
              end if;
            end loop;
          end;
        end if;
        return True;
      end Stored;

      procedure Create_Default_Favorites is

        The_File : Ada.Text_IO.File_Type;

        procedure Put (Line : String) is
        begin
          Ada.Text_IO.Put_Line (The_File, Line);
        end Put;

        function Image_Of (Word : Lexicon.Word) return String renames Lexicon.Image_Of;

      begin -- Create_Default_Favorites
        Ada.Text_IO.Create (The_File, Name => Filename);
        Ada.Text_IO.Put (The_File, Text.Bom_8);
        if Support_Axis_Positions then
          Put ("AP Home | 30° 00' 00"" | -125° 00' 00""");
          Put ("");
        end if;
        Put (Image_Of (Lexicon.Mercury));
        Put (Image_Of (Lexicon.Venus));
        Put (Image_Of (Lexicon.Moon));
        Put (Image_Of (Lexicon.Mars));
        Put (Image_Of (Lexicon.Jupiter));
        Put (Image_Of (Lexicon.Saturn));
        Put (Image_Of (Lexicon.Uranus));
        Put (Image_Of (Lexicon.Neptune));
        Put (Image_Of (Lexicon.Pluto));
        Put ("");
        if Support_Land_Marks then
          -- example for observatorium Schaffhausen
          Put ("LM " & Image_Of (Lexicon.Road_Sign) & " | 259° 49' 13"" | 2° 56' 15""");
          Put ("");
        end if;
        Put (Image_Of (Lexicon.Albereo));
        Put (Image_Of (Lexicon.Aldebaran));
        Put (Image_Of (Lexicon.Altair));
        Put (Image_Of (Lexicon.Arcturus));
        Put (Image_Of (Lexicon.Betelgeuse));
        Put (Image_Of (Lexicon.Deneb));
        Put (Image_Of (Lexicon.Mizar));
        Put (Image_Of (Lexicon.Polaris));
        Put (Image_Of (Lexicon.Pollux));
        Put (Image_Of (Lexicon.Procyon));
        Put (Image_Of (Lexicon.Regulus));
        Put (Image_Of (Lexicon.Rigel));
        Put (Image_Of (Lexicon.Sirius));
        Put (Image_Of (Lexicon.Vega));
        Put ("");
        Put ("NGC 869   | h-Persei");
        Put ("NGC 884   | χ-Persei");
        Put ("OCl 113.0 | Cr399");
        Put ("3C 273");
        Put ("");
        Put ("C14 | " & Image_Of (Lexicon.Persei_Clusters));
        Put ("C33 | " & Image_Of (Lexicon.East_Veil_Nebula));
        Put ("C34 | " & Image_Of (Lexicon.Veil_Nebula));
        Put ("C39 | " & Image_Of (Lexicon.Eskimo_Nebula));
        Put ("C46 | " & Image_Of (Lexicon.Hubbles_Nebula));
        Put ("C55 | " & Image_Of (Lexicon.Saturn_Nebula));
        Put ("");
        Put ("M1");
        Put ("M2");
        Put ("M3");
        Put ("M5");
        Put ("M8");
        Put ("M10");
        Put ("M11 | " & Image_Of (Lexicon.Wild_Duck_Cluster));
        Put ("M12");
        Put ("M13");
        Put ("M15");
        Put ("M16");
        Put ("M17");
        Put ("M20");
        Put ("M22");
        Put ("M27");
        Put ("M31 | " & Image_Of (Lexicon.Andromeda_Galaxy));
        Put ("M33");
        Put ("M34");
        Put ("M35");
        Put ("M36");
        Put ("M37");
        Put ("M38");
        Put ("M39");
        Put ("M41");
        Put ("M42 | " & Image_Of (Lexicon.Orion_Nebula));
        Put ("M44");
        Put ("M45");
        Put ("M46");
        Put ("M47");
        Put ("M51 | " & Image_Of (Lexicon.Whirlpool_Galaxy));
        Put ("M52");
        Put ("M57 | " & Image_Of (Lexicon.Ring_Nebula));
        Put ("M63");
        Put ("M64");
        Put ("M65");
        Put ("M66");
        Put ("M67");
        Put ("M76");
        Put ("M77");
        Put ("M78");
        Put ("M81");
        Put ("M82");
        Put ("M92");
        Put ("M96");
        Put ("M97");
        Put ("M103");
        Put ("M104");
        Put ("M106");
        Ada.Text_IO.Close (The_File);
      exception
      when others =>
        Error.Raise_With ("Can't create " & Filename);
      end Create_Default_Favorites;

      The_File : Ada.Text_IO.File_Type;

      The_Target_Number : Positive := Positive'first;

    begin -- Read
      if not File.Exists (Filename) then
        Create_Default_Favorites;
      end if;
      Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename);
      declare
        Has_Bom : constant Boolean := Text.Has_Skipped_Bom_8 (The_File);
      begin
        while not Ada.Text_IO.End_Of_File (The_File) loop
          declare
            Line : constant String := Ada.Text_IO.Get_Line (The_File);
          begin
            if Stored ((if Has_Bom then Line else Text.Utf8_Of (Line)), The_Target_Number) then
              The_Target_Number := Positive'succ(The_Target_Number);
            end if;
          end;
        end loop;
      end;
      Ada.Text_IO.Close (The_File);
    exception
    when Ada.IO_Exceptions.Name_Error =>
      Error.Raise_With (Filename & " not found");
    end Read_Favorite_Catalog;


    procedure Define_Catalog (Data_Id : Sky.Catalog_Id) is

      The_Item_Id : Natural := 0;
      The_Element : Element_Access;

      function Next return Boolean is
      begin
        case Data_Id is
        when Sky.Favorites =>
          if The_Element = null then
            The_Element := The_Element_List;
          else
            The_Element := The_Element.Next;
          end if;
          return The_Element /= null;
        when Sky.Extended_Catalogs =>
          The_Item_Id := Sky.Data.Next_Of (The_Item_Id, Data_Id);
          return The_Item_Id /= Sky.Data.No_More;
        end case;
      end Next;

      function Item  return Id is
      begin
        case Data_Id is
        when Sky.Favorites =>
          return (Sky.Favorites, False, False, The_Element);
        when others =>
          declare
            The_Id : Id(Data_Id);
          begin
            The_Id.Element_Number := The_Item_Id;
            return The_Id;
          end;
        end case;
      end Item;

      function Compare_Names (Left, Right : Id) return Boolean is
        use type Sky.Catalog_Id;
      begin
        if Left.Data_Id = Sky.Name then
          return Image_Of (Left) < Image_Of (Right);
        else
          raise Program_Error;
        end if;
      end Compare_Names;

      package Name_Tool is new Names.Generic_Sorting (Compare_Names);

    begin -- Define
      The_Id_List.Ids.Clear;
      The_Id_List := Id_List'(Kind => Data_Id, Ids => <>);
      while Next loop
        The_Id_List.Ids.Append (Item);
      end loop;
      case Data_Id is
      when Sky.Name =>
        Name_Tool.Sort (The_Id_List.Ids);
      when others =>
        null;
      end case;
    end Define_Catalog;


    function List return Id_List is
    begin
      return The_Id_List;
    end List;

  end Actual_Catalog;

end Name;
