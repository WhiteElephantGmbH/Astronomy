-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Ada.Strings.Unbounded;
with Angle;
with Application;
with Error;
with File;
with Lexicon;
with Neo;
with Solar_System;
with Sssb;
with Moon;
with Strings;
with Traces;

package body Name is

  package Log is new Traces ("Name");

  Support_Axis_Positions : Boolean := False;
  Support_Land_Marks     : Boolean := False;
  Support_Neos           : Boolean := False;

  type Text is new Ada.Strings.Unbounded.Unbounded_String;

  type Data_Kind is (Axis_Position, Landmark, Sky_Object);

  type Element_Data (Item : Data_Kind) is record
    Kind      : Object_Kind;
    Number    : Natural;
    Number_Id : Selector;
    Name      : Text;
    Next      : Element_Access;
    case Item is
    when Axis_Position =>
      Position : Space.Direction;
    when Landmark =>
      Direction : Earth.Direction;
    when others =>
      Object : Data.Object := Data.Undefined;
    end case;
  end record;


  function "=" (Left, Right : Id) return Boolean is
    use type Data.Kind;
  begin
    if Left.Data_Id /= Right.Data_Id then
      return False;
    elsif Left.Data_Id = Data.Favorites then
      if Left.Element /= null and  Right.Element /= null then
        return (Left.Element.Number_Id = Right.Element.Number_Id) and (Left.Element.Number = Right.Element.Number);
      else
        return Left.Element = Right.Element;
      end if;
    else
      return Left.Element_Number = Right.Element_Number;
    end if;
  end "=";


  function "<" (Left, Right : Element_Access) return Boolean is
  begin
    if Left.Number_Id < Right.Number_Id then
      return True;
    elsif Left.Number_Id = Right.Number_Id then
      return Left.Number < Right.Number;
    else
      return False;
    end if;
  end "<";


  function "<" (Left, Right : Id) return Boolean is
    use type Data.Kind;
  begin
    if Left.Data_Id = Data.Favorites then
      return Left.Element < Right.Element;
    else
      return Left.Element_Number < Right.Element_Number;
    end if;
  end "<";


  function Is_Known (Item : Id) return Boolean is
    use type Data.Kind;
  begin
    return Item.Data_Id /= Data.Favorites or else Item.Element /= null;
  end Is_Known;


  function Is_Visible (Item : Id) return Boolean is
  begin
    return Item.Is_Visible;
  end Is_Visible;


  function Image_Of (Item : Id) return String is

    function Id_And_Name return String is
      Image  : constant String := Item.Element_Number'img;
      Object : constant String := Data.Name_Of (Object_Of (Item));
    begin
      if Object = "" then
        return Image(Image'first + 1 .. Image'last);
      else
        return Image(Image'first + 1 .. Image'last) & " " & Object;
      end if;
    end Id_And_Name;

  begin
    case Item.Data_Id is
    when Data.Favorites =>
      return To_String (Item.Element.Name);
    when Data.Neo =>
      return Data.Name_Of (Object_Of (Item));
    when Data.Caldwell =>
      return "C" & Id_And_Name;
    when Data.Hip =>
      return "HIP " & Id_And_Name;
    when Data.Hr =>
      return "HR " & Id_And_Name;
    when Data.Messier =>
      return "M" & Id_And_Name;
    when Data.Ngc =>
      return "NGC " & Id_And_Name;
    when Data.Ocl =>
      return "OCl " & Id_And_Name;
    when Data.Quasars =>
      return "3C " & Id_And_Name;
    end case;
  end Image_Of;


  function Matches (Item      : Id;
                    Number_Id : Selector;
                    Number    : Natural) return Boolean is
    use type Data.Kind;
  begin
    if Item.Is_Visible then
      if Item.Data_Id = Data.Favorites then
        return (Item.Element.Number = Number) and (Number_Id = Item.Element.Number_Id);
      else
        return Item.Element_Number = Number;
      end if;
    end if;
    return False;
  end Matches;


  function Prefix_Of (Item : Id) return String is
    use type Data.Kind;
  begin
    if Item.Data_Id = Data.Favorites and then Item.Element.Number_Id = Enumerated then
      declare
        Id_Image  : constant String := Item.Element.Number'img & "  ";
      begin
        return "S" & Id_Image(Id_Image'first + 1 .. Id_Image'first + 3);
      end;
    end if;
    return "";
  end Prefix_Of;


  function Kind_Of (Item : Id) return Object_Kind is
  begin
    case Item.Data_Id is
    when Data.Favorites =>
      return Item.Element.Kind;
    when Data.Neo =>
      return Near_Earth_Object;
    when others =>
      return Sky_Object;
    end case;
  end Kind_Of;


  function Object_Of (Item : Id) return Data.Object is
  begin
    case Item.Data_Id is
    when Data.Favorites =>
      return Item.Element.Object;
    when others =>
      return Data.Object_Of (Item.Element_Number, Item.Data_Id);
    end case;
  end Object_Of;


  function Type_Of (Item : Id) return Data.Object_Type is
  begin
    case Kind_Of (Item) is
    when Sky_Object =>
      return Data.Type_Of (Object_Of (Item));
    when others =>
      return Data.Unknown;
    end case;
  end Type_Of;


  function Direction_Of (Item : Name.Id;
                         Ut   : Time.Ut) return Space.Direction is
  begin
    return Data.Direction_Of (Object_Of(Item), Ut);
  end Direction_Of;


  function Direction_Of (Item : Name.Id) return Space.Direction is
  begin
    return Item.Element.Position;
  end Direction_Of;


  function Direction_Of (Item : Name.Id) return Earth.Direction is
  begin
    return Item.Element.Direction;
  end Direction_Of;


  function Magnitude_Of (Item : Name.Id) return Float is
  begin
    if Kind_Of (Item) = Sky_Object then
      return Data.Magnitude_Of (Object_Of(Item));
    else
      return Float'first; -- Unknown Magnitude
    end if;
  end Magnitude_Of;


  function Visibility_Changed_For (Item    : in out Id;
                                   Visible :        Boolean) return Boolean is
  begin
    Item.Is_Visible := Visible;
    return Item.Was_Visible /= Visible;
  end Visibility_Changed_For;


  procedure Clear_History_For (Targets : in out Id_List) is
  begin
    for Index in Targets.Ids'first .. Targets.Last loop
      Targets.Ids(Index).Was_Visible := False;
    end loop;
  end Clear_History_For;


  function Item_Of (List  : Id_List;
                    Image : String) return Id is
  begin
    for Index in List.Ids'first .. List.Last loop
      declare
        Item : constant Id := List.Ids(Index);
      begin
        if Image = Image_Of (Item) then
          return Item;
        end if;
      end;
    end loop;
    return No_Id;
  end Item_Of;


  function Item_Of (List             : Id_List;
                    Direction        : Space.Direction;
                    Search_Tolerance : Space.Distance) return Id is
  begin
    for Index in List.Ids'first .. List.Last loop
      declare
        Item : constant Id := List.Ids(Index);

        function Found_Item (Direction_Of : Get_Space_Access) return Boolean is
          use type Space.Direction;
        begin
          if Is_Visible (Item) then
            declare
              List_Direction : constant Space.Direction := Direction_Of (Item, Time.Universal);
              use type Angle.Degrees;
            begin
              if (Direction - List_Direction) < Search_Tolerance then
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
          if Found_Item (Standard.Moon.Direction_Of'access) then
            return Item;
          end if;
        when Name.Sun | Name.Planet =>
          if Found_Item (Solar_System.Direction_Of'access) then
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


  procedure Update (Targets : Id_List_Access;
                    Remove  : access procedure (Index : Natural);
                    Insert  : access procedure (Item : Id_Access; Index : Natural)) is
    List_Index : Natural := 0;
  begin
    for Index in Targets.Ids'first .. Targets.Last loop
      declare
        Item : Id renames Targets.Ids(Index);
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
          Insert (Item'unrestricted_access, List_Index);
          Item.Was_Visible := True;
        end if;
      end;
    end loop;
  end Update;


  protected Actual_Catalog is

    procedure Read_Favorite_Catalog;

    procedure Define_Catalog (Data_Id : Data.Kind);

    function List return Id_List;

  private
    The_Element_List : Element_Access;
    The_Id_List      : Id_List;
  end Actual_Catalog;


  function "=" (Left, Right : Id_List) return Boolean is
    use type Data.Kind;
  begin
    if Left.Last /= Right.Last then
      return False;
    elsif Left.Kind /= Right.Kind then
      return False;
    else
      for Index in Left.Ids'first .. Left.Last loop
        if Left.Ids(Index) /= Right.Ids(Index) then
          return False;
        end if;
      end loop;
      return True;
    end if;
  end "=";


  procedure Read_Favorites (Enable_Axis_Positions : Boolean;
                            Enable_Land_Marks     : Boolean;
                            Enable_Neos           : Boolean) is
  begin
    Support_Axis_Positions := Enable_Axis_Positions;
    Support_Land_Marks := Enable_Land_Marks;
    Support_Neos := Enable_Neos;
    Actual_Catalog.Read_Favorite_Catalog;
  end Read_Favorites;


  procedure Define (List : Data.Kind) is
  begin
    Actual_Catalog.Define_Catalog (List);
  end Define;


  function Actual_List return Id_List is
  begin
    return Actual_Catalog.List;
  end Actual_List;


  protected body Actual_Catalog is

    procedure Read_Favorite_Catalog is

      Filename : constant String := Application.Composure ("Favorites", "txt");

      function Stored (Line   : String;
                       Number : Positive) return Boolean is

        Parts : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Strings.Trimmed (Line), '|'));

        function Part_For (Index : Strings.Element_Index) return String is
        begin
          return Strings.Trimmed (Parts(Index));
        end Part_For;

        The_Element : Element_Access;

      begin -- Store
        if Parts.Count = 0 then
          return False;
        end if;
        declare
          Part_1  : constant String := Part_For (Strings.First_Index);
          Parts_1 : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Part_1, ' '));

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
                Words : constant Strings.Item := Strings.Item_Of (Parts_1, (First => Strings.First_Index + 1,
                                                                            Last  => Parts_1.Count));
                Position_Name : constant String := Strings.Data_Of (Words, Separator => " ");
                Ra_Image      : constant String := Part_For (Strings.First_Index + 1);
                Dec_Image     : constant String := Part_For (Strings.First_Index + 2);
              begin
                The_Element := new Element_Data (Axis_Position);
                The_Element.Number := Number;
                The_Element.Number_Id := Enumerated;
                The_Element.Name := To_Unbounded_String (Position_Name);
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
                Words : constant Strings.Item := Strings.Item_Of (Parts_1, (First => Strings.First_Index + 1,
                                                                            Last  => Parts_1.Count));
                Mark_Name : constant String := Strings.Data_Of (Words, Separator => " ");
                Az_Image  : constant String := Part_For (Strings.First_Index + 1);
                Alt_Image : constant String := Part_For (Strings.First_Index + 2);
              begin
                The_Element := new Element_Data (Landmark);
                The_Element.Number := Number;
                The_Element.Number_Id := Enumerated;
                The_Element.Name := To_Unbounded_String (Mark_Name);
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
            The_Element.Number_Id := Enumerated;
            if Parts.Count = 4 then
              declare
                Object      : constant String := Part_1;
                Description : constant String := Part_For (Strings.First_Index + 1);
                Ra_Image    : constant String := Part_For (Strings.First_Index + 2);
                Dec_Image   : constant String := Part_For (Strings.First_Index + 3);
              begin
                The_Element.Kind := Sky_Object;
                The_Element.Name := To_Unbounded_String (Object);
                The_Element.Object := Data.New_Object_For (Name        => Object,
                                                           Description => Description,
                                                           Direction   => Space.Direction_Of (Ra_Image  => Ra_Image,
                                                                                              Dec_Image => Dec_Image));
              exception
              when others =>
                Error.Raise_With ("Incorrect Location - " & Line);
              end;
            else
              declare
                Object_Id    : constant String := Part_For (Strings.First_Index);
                Object_Parts : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Object_Id, ' '));
              begin
                if Object_Parts.Count = 0 then
                  Error.Raise_With ("No Object defined in " & Line);
                end if;
                declare
                  Item : constant String := Object_Parts(Strings.First_Index);
                begin
                  The_Element.Name := To_Unbounded_String (Part_For (Parts.Count));
                  if Object_Parts.Count = 1 then
                    if Lexicon.Found (Item, Lexicon.Sun) then
                      The_Element.Kind := Sun;
                    elsif Lexicon.Found (Item, Lexicon.Mercury) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Venus) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Moon) then
                      The_Element.Kind := Moon;
                    elsif Lexicon.Found (Item, Lexicon.Mars) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Jupiter) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Saturn) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Uranus) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Neptune) then
                      The_Element.Kind := Planet;
                    elsif Lexicon.Found (Item, Lexicon.Pluto) then
                      The_Element.Kind := Planet;
                    elsif Sssb.Exists (Object_Id) then
                      The_Element.Kind := Small_Solar_System_Body;
                    elsif Support_Neos and then Neo.Exists (Object_Id) then
                      The_Element.Kind := Near_Earth_Object;
                      The_Element.Object := Data.Neo_Object_Of (Object_Id);
                    elsif (Item(Item'first) = 'C') or (Item(Item'first) = 'M') then
                      The_Element.Kind := Sky_Object;
                      if Parts.Count > 1 then
                        The_Element.Name := Item & ' ' & The_Element.Name;
                      end if;
                      The_Element.Number := Natural'value(Item(Item'first + 1 .. Item'last));
                      if Item(Item'first) = 'C' then
                        The_Element.Number_Id := Caldwell;
                        The_Element.Object := Data.Object_Of (The_Element.Number, Data.Caldwell);
                      else
                        The_Element.Number_Id := Messier;
                        The_Element.Object := Data.Object_Of (The_Element.Number, Data.Messier);
                      end if;
                    else
                      Error.Raise_With ("Incorrect Object id in " & Filename & ": " & Item);
                    end if;
                  else
                    declare
                      Value : constant Natural := Data.Value_Of (Object_Parts(Strings.First_Index + 1));
                    begin
                      The_Element.Kind := Sky_Object;
                      begin
                        if Item = "HR" then
                          The_Element.Object := Data.Object_Of (Value, Data.Hr);
                        elsif Item = "HIP" then
                          The_Element.Object := Data.Object_Of (Value, Data.Hip);
                        elsif Item = "NGC" then
                          The_Element.Object := Data.Object_Of (Value, Data.Ngc);
                        elsif Item = "OCl" then
                          The_Element.Object := Data.Object_Of (Value, Data.Ocl);
                        elsif Item = "3C" then
                          The_Element.Object := Data.Object_Of (Value, Data.Quasars);
                        elsif Sssb.Exists (Object_Id) then
                          The_Element.Kind := Small_Solar_System_Body;
                        elsif Support_Neos and then Neo.Exists (Object_Id) then
                          The_Element.Kind := Near_Earth_Object;
                          The_Element.Object := Data.Neo_Object_Of (Object_Id);
                        else
                          Error.Raise_With (Object_Id & " unknown in " & Filename);
                        end if;
                      end;
                    end;
                  end if;
                exception
                when Error.Occurred =>
                  raise;
                when others =>
                  Error.Raise_With ("Unknown object in " & Filename & ": " & Line);
                end;
              end;
            end if;
          end Add_Sky_Object;

        begin
          if Support_Axis_Positions and then Parts_1.Count >= 2 and then Parts_1(Strings.First_Index) = "AP" then
            Add_Axis_Position;
          elsif Support_Land_Marks and then Parts_1.Count >= 2 and then Parts_1(Strings.First_Index) = "LM" then
            Add_Landmark;
          else
            Add_Sky_Object;
          end if;
        end;
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
        Put (Strings.Bom_8 & Image_Of (Lexicon.Mercury));
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
          if Support_Neos then -- like CDK observatorium Schaffhausen
            Put ("LM " & Image_Of (Lexicon.Road_Sign) & " | 259° 49' 13"" | 2° 56' 15""");
          else -- like M_Zero
            Put ("LM " & Image_Of (Lexicon.East) &  " |  90° | 0°");
            Put ("LM " & Image_Of (Lexicon.West) &  " | 270° | 0°");
          end if;
          Put ("");
        end if;
        Put ("HIP 95947  | " & Image_Of (Lexicon.Albireo));
        Put ("HIP 21421  | " & Image_Of (Lexicon.Aldebaran));
        Put ("HIP 97649  | " & Image_Of (Lexicon.Altair));
        Put ("HIP 69673  | " & Image_Of (Lexicon.Arkturus));
        Put ("HIP 27989  | " & Image_Of (Lexicon.Betelgeuse));
        Put ("HIP 102098 | " & Image_Of (Lexicon.Deneb));
        Put ("HIP 65378  | " & Image_Of (Lexicon.Mizar));
        Put ("HIP 11767  | " & Image_Of (Lexicon.Polaris));
        Put ("HIP 37826  | " & Image_Of (Lexicon.Pollux));
        Put ("HIP 37279  | " & Image_Of (Lexicon.Procyon));
        Put ("HIP 49669  | " & Image_Of (Lexicon.Regulus));
        Put ("HIP 24436  | " & Image_Of (Lexicon.Rigel));
        Put ("HIP 32349  | " & Image_Of (Lexicon.Sirius));
        Put ("HIP 91262  | " & Image_Of (Lexicon.Vega));
        Put ("");
        Put ("NGC 869   | h-Persei");
        Put ("NGC 884   | χ-Persei");
        Put ("OCl 113.0 | Cr399");
        Put ("3C 273");
        Put ("");
        Put ("C39 | " & Image_Of (Lexicon.Eskimo_Nebula));
        Put ("C34 | " & Image_Of (Lexicon.Veil_Nebula));
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
        Put ("M31 | " & Image_Of (Lexicon.Andromeda_Galaxie));
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
        Has_Bom : constant Boolean := Strings.Has_Skipped_Bom_8 (The_File);
      begin
        while not Ada.Text_IO.End_Of_File (The_File) loop
          declare
            Line : constant String := Ada.Text_IO.Get_Line (The_File);
          begin
            if Stored ((if Has_Bom then Line else Strings.Utf8_Of (Line)), The_Target_Number) then
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


    procedure Define_Catalog (Data_Id : Data.Kind) is

      The_Item_Id : Natural := 0;
      The_Element : Element_Access;

      function Next return Boolean is
      begin
        case Data_Id is
        when Data.Favorites =>
          if The_Element = null then
            The_Element := The_Element_List;
          else
            The_Element := The_Element.Next;
          end if;
          return The_Element /= null;
        when others =>
          The_Item_Id := Data.Next_Of (The_Item_Id, Data_Id);
          return True;
        end case;
      exception
      when Data.End_Of_List =>
        return False;
      end Next;

      function Item  return Id is
      begin
        case Data_Id is
        when Data.Favorites =>
          return (Data.Favorites, False, False, The_Element);
        when Data.Caldwell =>
          return (Data.Caldwell, False, False, The_Item_Id);
        when Data.Hip =>
          return (Data.Hip, False, False, The_Item_Id);
        when Data.Hr =>
          return (Data.Hr, False, False, The_Item_Id);
        when Data.Messier =>
          return (Data.Messier, False, False, The_Item_Id);
        when Data.Neo =>
          return (Data.Neo, False, False, The_Item_Id);
        when Data.Ngc =>
          return (Data.Ngc, False, False, The_Item_Id);
        when Data.Ocl =>
          return (Data.Ocl, False, False, The_Item_Id);
        when Data.Quasars =>
          return (Data.Quasars, False, False, The_Item_Id);
        end case;
      end Item;

    begin -- Define
      The_Id_List.Kind := Data_Id;
      The_Id_List.Last := 0;
      while Next loop
        The_Id_List.Last := The_Id_List.Last + 1;
        The_Id_List.Ids(The_Id_List.Last) := Item;
      end loop;
    end Define_Catalog;


    function List return Id_List is
    begin
      return The_Id_List;
    end List;

  end Actual_Catalog;

end Name;
