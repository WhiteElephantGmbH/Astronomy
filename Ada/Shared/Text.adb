-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Unchecked_Conversion;
with Unsigned;

package body Text is

  --*******************************************************************************************************************
  --**  Standard String                                                                                              **
  --*******************************************************************************************************************

  package Map renames Ada.Strings.Maps;

  pragma Warnings (Off); -- allow hiding predefined operator
  function "<" (Left  : Character;
                Right : Map.Character_Set) return Boolean renames Map.Is_In;
  pragma Warnings (On);

  Line_Break  : constant Map.Character_Set := Map.To_Set (Ascii.Cr & Ascii.Lf);
  White_Space : constant Map.Character_Set := Map.To_Set (Space & Ascii.Ht & Ascii.Cr & Ascii.Lf);


  function Is_Lowercase (Item : Standard.String) return Boolean is
  begin
    for The_Index in Item'range loop
      if Is_Uppercase (Item(The_Index)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Lowercase;


  function Is_Uppercase (Item : Standard.String) return Boolean is
  begin
    for The_Index in Item'range loop
      if Is_Lowercase (Item(The_Index)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Uppercase;


  function Found (Item      : Character;
                  In_String : Standard.String) return Boolean is
  begin
    for Character of In_String loop
      if Character = Item then
        return True;
      end if;
    end loop;
    return False;
  end Found;


  function Location_Of (Item          : Character;
                        In_String     : Standard.String;
                        From          : Positive := Start_Of_String;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Location_Of (Standard.String'[Item], In_String, From, The_Direction);
  end Location_Of;


  function Location_Of (Pattern       : Standard.String;
                        In_String     : Standard.String;
                        From          : Positive := Start_Of_String;
                        The_Direction : Direction := Forward) return Natural is
    The_Cursor  : Natural;
    Last_Cursor : Natural;
    The_Index   : Positive;
  begin
    if Pattern = "" then
      return Not_Found;
    end if;
    if The_Direction = Forward then
      if From = Start_Of_String then
        The_Cursor := In_String'first;
      else
        The_Cursor := From;
      end if;
      The_Index := Pattern'first;
      Last_Cursor := The_Cursor;
      while The_Cursor <= In_String'last loop
        if In_String(The_Cursor) = Pattern(The_Index) then
          if The_Index = Pattern'last then
            return Last_Cursor;
          end if;
          The_Cursor := @ + 1;
          The_Index := @ + 1;
        else
          The_Cursor := Last_Cursor + 1;
          Last_Cursor := The_Cursor;
          The_Index := Pattern'first;
        end if;
      end loop;
    else -- Backward
      if From = Start_Of_String then
        The_Cursor := In_String'last;
      else
        The_Cursor := From;
      end if;
      The_Index := Pattern'last;
      Last_Cursor := The_Cursor;
      while The_Cursor >= In_String'first loop
        if In_String(The_Cursor) = Pattern(The_Index) then
          if The_Index = Pattern'first then
            return The_Cursor;
          end if;
          The_Cursor := @ - 1;
          The_Index := @ - 1;
        else
          The_Cursor := Last_Cursor - 1;
          Last_Cursor := The_Cursor;
          The_Index := Pattern'last;
        end if;
      end loop;
    end if;
    return Not_Found;
  end Location_Of;


  procedure Get_Trim_Range (Item      :     Standard.String;
                            The_First : out Positive;
                            The_Last  : out Natural) with Inline is
  begin
    The_First := Item'first;
    The_Last  := Item'last;
    if Item = "" then
      return;
    end if;
    while The_First <= The_Last and then Item(The_First) <= ' ' loop
      The_First := @ + 1;
    end loop;
    if The_First > The_Last then
      return;
    end if;
    while Item(The_Last) <= ' ' loop
      The_Last := @ - 1;
    end loop;
  end Get_Trim_Range;


  function Trimmed (Item : Standard.String) return Standard.String is
    The_First : Natural;
    The_Last  : Natural;
  begin
    Get_Trim_Range (Item, The_First, The_Last);
    return Item(The_First .. The_Last);
  end Trimmed;


  function Trimmed_Leading (Item : Standard.String) return Standard.String is
    The_First : Natural;
  begin
    if Item = "" then
      return "";
    end if;
    The_First := Item'first;
    while The_First <= Item'last and then Item(The_First) <= ' ' loop
      The_First := @ + 1;
    end loop;
    if The_First > Item'last then
      return "";
    end if;
    return Item(The_First .. Item'last);
  end Trimmed_Leading;


  function Trimmed_Leading (Item : Standard.String;
                            What : Character) return Standard.String is
    The_First : Natural;
  begin
    if Item = "" then
      return "";
    end if;
    The_First := Item'first;
    while The_First <= Item'last and then Item(The_First) = What loop
      The_First := @ + 1;
    end loop;
    if The_First > Item'last then
      return "";
    end if;
    return Item(The_First .. Item'last);
  end Trimmed_Leading;


  function Trimmed_Trailing (Item : Standard.String) return Standard.String is
    The_Last : Natural;
  begin
    if Item = "" then
      return "";
    end if;
    The_Last := Item'last;
    while The_Last >= Item'first and then Item(The_Last) <= ' ' loop
      The_Last := @ - 1;
    end loop;
    return Item(Item'first .. The_Last);
  end Trimmed_Trailing;


  function Trimmed_Trailing (Item : Standard.String;
                             What : Character) return Standard.String is
    The_Last : Natural;
  begin
    if Item = "" then
      return "";
    end if;
    The_Last := Item'last;
    while The_Last >= Item'first and then Item(The_Last) = What loop
      The_Last := @ - 1;
    end loop;
    return Item(Item'first .. The_Last);
  end Trimmed_Trailing;


  function Trimmed (Item : Standard.String;
                    What : Character) return Standard.String is
    The_First : Natural;
    The_Last  : Natural;
  begin
    if Item = "" then
      return "";
    end if;
    The_First := Item'first;
    The_Last  := Item'last;
    if The_First <= The_Last and then Item(The_First) = What then
      The_First := @ + 1;
    end if;
    if The_First > The_Last then
      return "";
    end if;
    if Item(The_Last) = What then
      The_Last := @ - 1;
    end if;
    return Item(The_First .. The_Last);
  end Trimmed;


  function Purge_Of (Item : Standard.String) return Standard.String is
    The_Data : Standard.String(Item'first .. Item'last);
    The_Last : Natural := Item'first - 1;
  begin
    for The_Character of Item loop
      if The_Character > ' ' then
        The_Last := @ + 1;
        The_Data(The_Last) := The_Character;
      end if;
    end loop;
    return The_Data(Item'first..The_Last);
  end Purge_Of;


  function Legible_Of (Item : Standard.String) return Standard.String is
    Legible_String : Standard.String (1 .. Item'length);
    The_Size       : Natural := 0;
    Am_In_Gap      : Boolean := True;
    The_Character  : Character;
  begin
    for The_Index in Item'range loop
      The_Size := The_Size + 1;
      The_Character := Item (The_Index);
      if The_Character = '_' then
        The_Character := Space; -- Substitute underscores with spaces
      end if;
      if The_Character = Space then
        if Am_In_Gap then  -- Multiple space
          The_Size := The_Size - 1;  -- Overwrite previous
        else
          Am_In_Gap := True;
        end if;
      elsif Am_In_Gap then
        The_Character := Uppercase_Of (The_Character);
        Am_In_Gap := False;
      else
        The_Character := Lowercase_Of (The_Character);
      end if;
      if The_Size > 0 then  -- Ignore leading spaces
        Legible_String (The_Size) := The_Character;
      end if;
    end loop;
    if (The_Size > 0) and Am_In_Gap then -- Remove trailing space
      The_Size := @ - 1;
    end if;
    return Legible_String (1 .. The_Size);
  end Legible_Of;


  function Reduced (Item : Standard.String) return Standard.String is

    type Separator is (No_Separator, Blank, New_Line);

    Reduced_String : Standard.String (1 .. Item'length);
    The_Size       : Natural := 0;
    The_Separator  : Separator := No_Separator;
    The_Character  : Character;

  begin
    for The_Index in Item'range loop
      The_Character := Item (The_Index);
      if The_Character < White_Space then
        if The_Size /= 0 then -- not leading white spaces
          if The_Separator /= New_Line then
            if The_Character < Line_Break then
              The_Separator := New_Line;
            else
              The_Separator := Blank;
            end if;
          end if;
        end if;
      else
        The_Size := @ + 1;
        case The_Separator is
        when Blank =>
          Reduced_String(The_Size) := Space;
          The_Size := @ + 1;
        when New_Line =>
          Reduced_String (The_Size) := Ascii.Cr;
          The_Size := @ + 1;
        when No_Separator =>
          null;
        end case;
        The_Separator := No_Separator;
        Reduced_String (The_Size) := The_Character;
      end if;
    end loop;
    return Reduced_String (1 .. The_Size);
  end Reduced;


  function Image_Of (Item : Kind) return Standard.String is
  begin
    return Trimmed (Item'img);
  end Image_Of;


  function Legible_Image_Of (Item : Kind) return Standard.String is
  begin
    return Legible_Of (Item'img);
  end Legible_Image_Of;


  function Padded_Image_Of (Item    : Kind;
                            Padding : Character := '0') return Standard.String is

    function Image is new Image_Of (Kind);

    The_Image : constant Standard.String  := Image(Item);

  begin -- Padded_Image_Of
    if Item < Kind'val(0) then
      raise Usage_Error;
    end if;
    return Ada.Strings.Fixed."*"(Kind'width - The_Image'length - 1, Padding) & The_Image;
  end Padded_Image_Of;


  function File_Extension_Of (The_Filename : Standard.String) return Standard.String is
  begin
    for The_Position in reverse The_Filename'range loop
      exit when (The_Filename (The_Position) = '\') or (The_Filename (The_Position) = '/');
      if The_Filename (The_Position) = '.' then
        return Lowercase_Of (The_Filename (The_Position + 1 .. The_Filename'last));
      end if;
    end loop;
    return "";
  end File_Extension_Of;


  --*******************************************************************************************************************
  --**  Unbounded of String                                                                                          **
  --*******************************************************************************************************************

  package CA renames Character_Array;

  subtype Cursor is CA.Cursor;

  subtype Count_Type is Ada.Containers.Count_Type;

  type Elements_Type (Last : CA.Extended_Index) is limited record
    Data : Standard.String (Positive'first .. Last);
  end record;

  type Elements_Access is access all Elements_Type;


  function To_Elements_Access (Item : String) return Elements_Access is

    type Internal_Vector is record
      Unused_1 : Long_Long_Integer;
      Elements : Elements_Access;
      Last     : CA.Extended_Index;
      Dummy_1  : Integer;
      Unused_2 : Long_Long_Integer;
    end record;

    function Convert is new Ada.Unchecked_Conversion (String, Internal_Vector);

    The_Vector : constant Internal_Vector := Convert(Item);

  begin -- To_Elements_Access
    if The_Vector.Last /= Item.Count then
      raise Program_Error; -- internal structure of vecor changed
    end if;
    return The_Vector.Elements;
  end To_Elements_Access;


  function Is_Null (Item : String) return Boolean is

  begin
    return Is_Empty (Item);
  end Is_Null;


  function First (Item : String) return Character is
  begin
    return Item.First_Element;
  end First;


  function Last (Item : String) return Character is
  begin
    return Item.Last_Element;
  end Last;


  function String_Of (Item : Standard.String) return String is
    The_String : String;
  begin
    The_String.Append (Item);
    return The_String;
  end String_Of;


  function To_String (Item : String) return Standard.String is
    The_String : Standard.String (1 .. Item.Count);
  begin
    for The_Index in 1 .. Count(Item) loop
      The_String(Natural(The_Index)) := Item(The_Index);
    end loop;
    return The_String;
  end To_String;


  procedure Append (Item : in out String;
                    Data :        Standard.String) is
  begin
    Item.Insert (Data, Item.Count + 1);
  end Append;


  procedure Prepend (Item : in out String;
                     Data :        Standard.String) is
  begin
    Item.Insert (Data, Start_Of_String);
  end Prepend;


  procedure Insert (In_String : in out String;
                    Data      :        Standard.String;
                    At_Index  :        Positive) is
  begin
    In_String.Insert_Space (At_Index, Data'length);
    In_String.Update (At_Index, Data);
  end Insert;


  function Index_Of (In_String     : String;
                     Item          : Character;
                     From          : Positive := Start_Of_String;
                     The_Direction : Direction := Forward) return Natural is
    Elements : constant Elements_Access := In_String.To_Elements_Access;
  begin
    return Location_Of (Item, Elements.Data, From, The_Direction);
  end Index_Of;


  function Index_Of (In_String     : String;
                     Pattern       : Standard.String;
                     From          : Positive := Start_Of_String;
                     The_Direction : Direction := Forward) return Natural is
    Elements : constant Elements_Access := In_String.To_Elements_Access;
  begin
    return Location_Of (Pattern, Elements.Data, From, The_Direction);
  end Index_Of;


  procedure Replace (In_String : in out String;
                     From      :        Positive;
                     To        :        Natural;
                     By        :        Standard.String) is
    Length_Correction : constant Integer :=  By'length - To + From - 1;
  begin
    if Length_Correction > 0 then
      In_String.Insert_Space (From, Count_Type(Length_Correction));
    elsif Length_Correction < 0 then
      In_String.Delete (From, Count_Type(-Length_Correction));
    end if;
    In_String.Update (From, By);
  end Replace;


  procedure Replace (In_String : in out String;
                     Data      :        Standard.String;
                     By        :        Standard.String) is
    From : constant Natural := In_String.Index_Of (Data);
  begin
    if From /= Not_Found then
      In_String.Replace (From, From + Data'length - 1, By);
    end if;
  end Replace;


  procedure Make_Lowercase (Item : in out String) is
    Elements : constant Elements_Access := Item.To_Elements_Access;
  begin
    for The_Character of Elements.Data loop
      The_Character := Lowercase_Of (The_Character);
    end loop;
  end Make_Lowercase;


  procedure Make_Uppercase (Item : in out String) is
    Elements : constant Elements_Access := Item.To_Elements_Access;
  begin
    for The_Character of Elements.Data loop
      The_Character := Uppercase_Of (The_Character);
    end loop;
  end Make_Uppercase;


  function Slice (Item : String;
                  From : Positive;
                  To   : Natural) return Standard.String is
    Elements : constant Elements_Access := Item.To_Elements_Access;
  begin
    return Elements.Data(From .. To);
  end Slice;


  procedure Trim (Item : in out String) is
    Elements  : constant Elements_Access := Item.To_Elements_Access;
    The_First : Positive;
    The_Last  : Natural;
  begin
    Get_Trim_Range (Elements.Data, The_First, The_Last);
    Item.Delete (Index => The_Last, Count => Count_Type(Item.Count - The_Last));
    Item.Delete (Index => Start_Of_String, Count => Count_Type(The_First - Start_Of_String));
  end Trim;


  procedure Truncate (Item       : in out String;
                      New_Length : Natural) is
  begin
    if Item.Count > New_Length then
      Item.Delete (Index => New_Length + 1,
                   Count => Count_Type(Item.Count - New_Length));
    end if;
  end Truncate;


  function Truncation_Of (Item       : String;
                          New_Length : Natural) return String is
    The_Item : String := Item;
  begin
    The_Item.Truncate (New_Length);
    return The_Item;
  end Truncation_Of;


   procedure Update (Item : in out String;
                    From :        Positive;
                    By   :        Standard.String) is
    The_Cursor : Cursor := Item.To_Cursor (From);
  begin
    for The_Character of By loop
      Item.Replace_Element (The_Cursor, The_Character);
      CA.Next (The_Cursor);
    end loop;
  end Update;


  procedure Put_String_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                              V : String) is
  begin
    S.Put ('"' & V.To_String & '"');
  end Put_String_Image;


  --*******************************************************************************************************************
  --**  List of Strings                                                                                              **
  --*******************************************************************************************************************

  function "+" (Left  : List;
                Right : Standard.String) return List is
    The_List : List := Left;
  begin
    The_List.Append (Right);
    return The_List;
  end "+";


  function "+" (Left  : Standard.String;
                Right : List) return List is
    The_List : List := Right;
  begin
    The_List.Prepend (Left);
    return The_List;
  end "+";


  function "+" (Left  : List;
                Right : List) return List is
    The_List   : List := Right;
    The_Source : List := Left;
  begin
    The_List.Splice (Before => Linked_Strings.No_Element,
                     Source => The_Source);
    return The_List;
  end "+";


  function "-" (Left  : List;
                Right : Standard.String) return List is
    The_List   : List := Left;
    The_Cursor : Linked_Strings.Cursor;
  begin
    The_Cursor := Find (The_List, Right);
    Delete (The_List, The_Cursor);
    return The_List;
  end "-";


  procedure Put_List_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                            V : List) is
  begin
    S.Put ((if V.Is_Empty then "[]" else "[""" & V.To_Data (Separator => """, """) & """]"));
  end Put_List_Image;


  function Concatenation (Item      : List;
                          Separator : Standard.String := " ") return Standard.String is
    The_Length : Natural := 0;
  begin
    for The_String of Item loop
      The_Length := The_Length + The_String'length + Separator'length;
    end loop;
    declare
      The_String : Standard.String(1..The_Length);
      The_First  : Positive;
      The_Last   : Natural := The_String'first - 1;
    begin
      for The_Part of Item loop
        The_First := The_Last + 1;
        The_Last := The_First + The_Part'length + Separator'length - 1;
        The_String (The_First .. The_Last) := The_Part & Separator;
      end loop;
      return The_String;
    end;
  end Concatenation;


  function To_Data (Item      : List;
                    Separator : Standard.String := "") return Standard.String is
    The_Length : Natural := Item.Count;
  begin
    if The_Length = 0 then
      return "";
    elsif The_Length = 1 then
      return Item.First_Element;
    else
      The_Length := @ * Separator'length;
      for The_String of Item loop
        The_Length := @ + The_String'length;
      end loop;
      declare
        The_Data     : Standard.String(1 .. The_Length);
        The_Position : Natural := First_Index;
      begin
        for The_String of Item loop
          declare
            Data : constant Standard.String := The_String & Separator;
          begin
            The_Data(The_Position .. The_Position + Data'length - 1) := Data;
            The_Position := @ + Data'length;
          end;
        end loop;
        return The_Data(The_Data'first .. The_Length - Separator'length);
      end;
    end if;
  end To_Data;


  function List_Of (Item      : Standard.String;
                    Separator : Character;
                    Purge     : Boolean := True;
                    Do_Trim   : Boolean := True;
                    Symbols   : Standard.String := "") return List is

    The_List  : List;
    The_First : Natural := Item'first;

    function Conditional_Trimmed (Image : Standard.String) return Standard.String is
      (if Do_Trim then Trimmed(Image) else Image);

  begin -- List_Of
    for Index in Item'range loop
      declare
        function Part return Standard.String is (Conditional_Trimmed (Item(The_First .. Index - 1)));
      begin
        if Item(Index) = Separator then
          if not Purge or else Index /= The_First then
            The_List.Append (Part);
          end if;
          The_First := Index + 1;
        elsif Found (Item(Index), Symbols) then
          if not Purge or else Index /= The_First then
            The_List.Append (Part);
          end if;
          The_List.Append (Item(Index .. Index));
          The_First := Index + 1;
        end if;
      end;
    end loop;
    if not Purge or else The_First <= Item'last then
      The_List.Append (Conditional_Trimmed(Item(The_First .. Item'last)));
    end if;
    return The_List;
  end List_Of;


  package Tool is new Linked_Strings.Generic_Sorting;

  procedure Sort (Item : in out List) is
  begin
    Tool.Sort (Linked_Strings.List(Item));
  end Sort;


  function Sorted (Item : List) return List is
    The_Item : List := Item;
  begin
    Sort (The_Item);
    return The_Item;
  end Sorted;


  --*******************************************************************************************************************
  --**  Vector of Strings                                                                                            **
  --*******************************************************************************************************************

  procedure Put_Vector_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                              V : Vector) is
  begin
    S.Put ((if V.Is_Empty then "[]" else "[""" & V.To_List.To_Data (Separator => """, """) & """]"));
  end Put_Vector_Image;


  function Part (Item  : Vector;
                 From  : Positive;
                 To    : Natural) return Vector is
    The_Vector : Vector;
  begin
    if To < From then
      return Empty_Vector;
    end if;
    for Index in From .. To loop
      The_Vector.Append (Item(Index));
    end loop;
    return The_Vector;
  end Part;


  function To_List (Item : Vector'class) return List is
    The_List : List;
  begin
    for The_String of Item loop
      The_List.Append (The_String);
    end loop;
    return The_List;
  end To_List;


  function To_Vector (Item : List'class) return Vector is
    The_Vector : Vector;
  begin
    for The_String of Item loop
      The_Vector.Append (The_String);
    end loop;
    return The_Vector;
  end To_Vector;


  function To_Data (Item      : Vector;
                    Separator : Standard.String := "") return Standard.String is
  begin
    return Item.To_List.To_Data (Separator);
  end To_Data;


  --*******************************************************************************************************************
  --**  Strings                                                                                                      **
  --*******************************************************************************************************************

  procedure Append (Item : in out Strings;
                    Data :        Standard.String) is
    First_Position : constant Natural := Item.Length + 1;
  begin
    Item.Count := @ + 1;
    Item.Length := @ + Data'length;
    Item.Positions(Item.Count) := Position(First_Position);
    Item.Data(First_Position .. Item.Length) := Data;
  end Append;


  type Strings_Access is access all Strings;
  for Strings_Access'storage_size use 0;

  type Strings_Iterator is new Strings_Iterator_Interfaces.Reversible_Iterator with record
    Container : Strings_Access;
  end record;

  overriding
  function First (Object : Strings_Iterator) return String_Count;

  overriding
  function Last (Object : Strings_Iterator) return String_Count;

  overriding
  function Next (Object : Strings_Iterator;
                 Index  : String_Count) return String_Count;

  overriding
  function Previous (Object : Strings_Iterator;
                     Index  : String_Count) return String_Count;


  function First (Object : Strings_Iterator) return String_Count is
  begin
    if Object.Container.Count > 0 then
      return First_Index;
    else
      return No_String;
    end if;
  end First;


  function Last (Object : Strings_Iterator) return String_Count is
  begin
    return Object.Container.Count;
  end Last;


  function Next (Object : Strings_Iterator;
                 Index  : String_Count) return String_Count is
  begin
    if Index = No_String then
      return No_String;
    end if;
    if Index = Object.Container.Count then
      return No_String;
    else
      return Index + 1;
    end if;
  end Next;


  function Previous (Object : Strings_Iterator;
                     Index  : String_Count) return String_Count is
    pragma Unreferenced (Object);
  begin
    if Index = No_String then
      return No_String;
    end if;
    return Index - 1;
  end Previous;


  function Constant_Strings_Reference (Item  : aliased Strings;
                                       Index : String_Count) return Standard.String is
  begin
    declare
      The_Position : constant Position := Item.Positions(Index);
    begin
      if Index < Item.Count then
        return Item.Data(Positive(The_Position) .. Positive(Item.Positions(Index + 1)) - 1);
      end if;
      return Item.Data(Positive(The_Position) .. Item.Length);
    end;
  exception
  when others =>
    raise Constraint_Error with "index out of range";
  end Constant_Strings_Reference;


  function Has_String (Index : String_Count) return Boolean is
  begin
    return Index /= No_String;
  end Has_String;


  function Strings_Iterate (Item : Strings) return Strings_Iterator_Interfaces.Reversible_Iterator'class is
  begin
    return Strings_Iterator'(Container => Item'unrestricted_access);
  end Strings_Iterate;


  function Length_At (Item  : Strings;
                      Index : String_Index) return Natural is

    List_Position : constant Position := Item.Positions(Index);

  begin
    if Index < Item.Count then
      return Natural(Item.Positions(Index + 1) - List_Position);
    end if;
    return Item.Data'last + 1 - Natural(List_Position);
  end Length_At;


  function First (Item : Strings) return Standard.String is
  begin
    return Item(First_Index);
  end First;


  function Last (Item : Strings) return Standard.String is
  begin
    return Item(Item.Count);
  end Last;


  function To_Data (Item      : Strings;
                    Separator : Standard.String := "") return Standard.String is
  begin
    if Item.Count = 0 then
      return "";
    else
      declare
        The_Data     : Standard.String(1 .. (Item.Count - 1) * Separator'length + Item.Length);
        The_Position : Positive := First_Index;
        Is_First     : Boolean := True;
      begin
        for The_String of Item loop
          declare
            Data : constant Standard.String := (if Is_First then "" else Separator) & The_String;
          begin
            The_Data(The_Position .. The_Position + Data'length - 1) := Data;
            The_Position := The_Position + Data'length;
          end;
          Is_First := False;
        end loop;
        return The_Data;
      end;
    end if;
  end To_Data;


  function Strings_Of (Item      : Standard.String;
                       Separator : Character;
                       Purge     : Boolean := True;
                       Do_Trim   : Boolean := True;
                       Symbols   : Standard.String := "") return Strings is

    The_Strings : Strings;
    The_First   : Natural := Item'first;

    function Conditional_Trimmed (Image : Standard.String) return Standard.String is
      (if Do_Trim then Trimmed(Image) else Image);

  begin -- Strings_Of
    for Index in Item'range loop
      declare
        function Part return Standard.String is (Conditional_Trimmed (Item(The_First .. Index - 1)));
      begin
        if Item(Index) = Separator then
          if not Purge or else Index /= The_First then
            The_Strings.Append (Part);
          end if;
          The_First := Index + 1;
        elsif Found (Item(Index), Symbols) then
          if not Purge or else Index /= The_First then
            The_Strings.Append (Part);
          end if;
          The_Strings.Append (Item(Index .. Index));
          The_First := Index + 1;
        end if;
      end;
    end loop;
    if not Purge or else The_First <= Item'last then
      The_Strings.Append (Item(The_First .. Item'last));
    end if;
    return The_Strings;
  end Strings_Of;


  function Part (Item  : Strings;
                 From  : String_Index;
                 To    : String_Count) return Strings is
    The_Strings : Strings;
  begin
    if To < From then
      return None;
    end if;
    for Index in From .. To loop
      The_Strings.Append (Item(Index));
    end loop;
    return The_Strings;
  end Part;


  function "+" (Left  : Strings;
                Right : Strings) return Strings is
    The_Strings : Strings := Left;
  begin
    The_Strings.Positions(Left.Count + 1 .. Left.Count + Right.Count) := Right.Positions(First_Index .. Right.Count);
    for Index in Left.Count + 1 .. Left.Count + Right.Count loop
      The_Strings.Positions(Index) := The_Strings.Positions(Index) + Position(Left.Length);
    end loop;
    The_Strings.Data(Left.Length + 1 .. Left.Length + Right.Length) := Right.Data (First_Index .. Right.Length);
    The_Strings.Count := Left.Count + Right.Count;
    The_Strings.Length := Left.Length + Right.Length;
    return The_Strings;
  end "+";


  procedure Prepend (Item : in out Strings;
                     Data :        Standard.String) is
    The_Strings : Strings;
  begin
    Append (The_Strings, Data);
    Item := The_Strings + Item;
  end Prepend;


  function Contains (Item : Strings;
                     Name : Standard.String) return Boolean is
  begin
    for The_Name of Item loop
      if The_Name = Name then
        return True;
      end if;
    end loop;
    return False;
  end Contains;


  function To_List (Item : Strings'class) return List is
    The_List : List;
  begin
    for Name of Item loop
      The_List.Append (Name);
    end loop;
    return The_List;
  end To_List;


  function To_Trimmed_List (Item : Strings'class) return List is
    The_List : List;
  begin
    for Name of Item loop
      The_List.Append (Trimmed (Name));
    end loop;
    return The_List;
  end To_Trimmed_List;


  procedure Put_Strings_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                               V : Strings) is
  begin
    S.Put ((if V.Count = 0 then "[]" else "[""" & V.To_Data (Separator => """, """) & """]"));
  end Put_Strings_Image;


  --*******************************************************************************************************************
  --**  Conversion                                                                                                   **
  --*******************************************************************************************************************

  Last_Ascii : constant Character := Character'val (127);
  No_Map     : constant Unsigned.Byte := 16#20#;

  subtype Upper_Character is Character range Character'val (16#80#)..Character'last;
  subtype G1_Character is Character range Character'val (16#A0#).. Character'last;

  type G1_Conversion_Table is array (G1_Character) of Unsigned.Byte;
  type Upper_Conversion_Table is array (Upper_Character) of Unsigned.Byte;

  Ecp437_To_Latin1_Map : constant Upper_Conversion_Table
                       := [16#C7#, 16#FC#, 16#E9#, 16#E2#, 16#E4#, 16#E0#, 16#E5#, 16#E7#,
                           16#EA#, 16#EB#, 16#E8#, 16#EF#, 16#EE#, 16#EC#, 16#C4#, 16#C5#,
                           16#C9#, 16#E6#, 16#C6#, 16#F4#, 16#F6#, 16#F2#, 16#FB#, 16#F9#,
                           16#FF#, 16#D6#, 16#DC#, 16#A2#, 16#A3#, 16#A5#, No_Map, No_Map,
                           16#E1#, 16#ED#, 16#F3#, 16#FA#, 16#F1#, 16#D1#, 16#AA#, 16#BA#,
                           16#BF#, No_Map, 16#AC#, 16#BD#, 16#BC#, 16#A1#, 16#AB#, 16#BB#,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, 16#DF#, No_Map, No_Map, No_Map, No_Map, 16#B5#, No_Map,
                           No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, 16#B1#, No_Map, No_Map, No_Map, No_Map, 16#F7#, No_Map,
                           16#B0#, No_Map, 16#B7#, No_Map, No_Map, 16#B2#, No_Map, 16#A0#];

  Latin1_To_Ecp437_Map : constant G1_Conversion_Table
                       := [16#FF#, 16#AD#, 16#9B#, 16#9C#, No_Map, 16#9D#, No_Map, No_Map,
                           No_Map, No_Map, 16#A6#, 16#AE#, 16#AA#, No_Map, No_Map, No_Map,
                           16#F8#, 16#F1#, 16#FD#, No_Map, No_Map, 16#E6#, No_Map, 16#FA#,
                           No_Map, No_Map, 16#A7#, 16#AF#, 16#AC#, 16#AB#, No_Map, 16#A8#,
                           No_Map, No_Map, No_Map, No_Map, 16#8E#, 16#8F#, 16#92#, 16#80#,
                           No_Map, 16#90#, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map,
                           No_Map, 16#A5#, No_Map, No_Map, No_Map, No_Map, 16#99#, No_Map,
                           No_Map, No_Map, No_Map, No_Map, 16#9A#, No_Map, No_Map, 16#E1#,
                           16#85#, 16#A0#, 16#83#, No_Map, 16#84#, 16#86#, 16#91#, 16#87#,
                           16#8a#, 16#82#, 16#88#, 16#89#, 16#8D#, 16#A1#, 16#8C#, 16#8B#,
                           No_Map, 16#A4#, 16#95#, 16#A2#, 16#93#, No_Map, 16#94#, 16#F6#,
                           No_Map, 16#97#, 16#A3#, 16#96#, 16#81#, No_Map, No_Map, 16#98#];


  Cp850_To_Latin1_Map : constant Upper_Conversion_Table
                      := [16#C7#, 16#FC#, 16#E9#, 16#E2#, 16#E4#, 16#E0#, 16#E5#, 16#E7#,
                          16#EA#, 16#EB#, 16#E8#, 16#EF#, 16#EE#, 16#EC#, 16#C4#, 16#C5#,
                          16#C9#, 16#E6#, 16#C6#, 16#F4#, 16#F6#, 16#F2#, 16#FB#, 16#F9#,
                          16#FF#, 16#D6#, 16#DC#, 16#F8#, 16#A3#, 16#D8#, 16#D7#, No_Map,
                          16#E1#, 16#ED#, 16#F3#, 16#FA#, 16#F1#, 16#D1#, 16#AA#, 16#BA#,
                          16#BF#, 16#AE#, 16#AC#, 16#BD#, 16#BC#, 16#A1#, 16#AB#, 16#BB#,
                          No_Map, No_Map, No_Map, No_Map, No_Map, 16#C1#, 16#C2#, 16#C0#,
                          16#A9#, No_Map, No_Map, No_Map, No_Map, 16#A2#, 16#A5#, No_Map,
                          No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, 16#E3#, 16#C3#,
                          No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, No_Map, 16#A4#,
                          16#F0#, 16#D0#, 16#CA#, 16#CB#, 16#C8#, No_Map, 16#CD#, 16#CE#,
                          16#CF#, No_Map, No_Map, No_Map, No_Map, 16#A6#, 16#CC#, No_Map,
                          16#D3#, 16#DF#, 16#D4#, 16#D2#, 16#F5#, 16#D5#, 16#B5#, 16#FE#,
                          16#DE#, 16#DA#, 16#DB#, 16#D9#, 16#FD#, 16#DD#, 16#AF#, 16#B4#,
                          16#AD#, 16#B1#, No_Map, 16#BE#, 16#B6#, 16#A7#, 16#F7#, 16#B8#,
                          16#B0#, 16#A8#, 16#B7#, 16#B9#, 16#B3#, 16#B2#, No_Map, 16#A0#];

  function Has_Bom_8 (Item : Standard.String) return Boolean is
  begin
    return (Item'length >= Bom_8'length) and then (Item(Item'first .. Item'first + Bom_8'length - 1) = Bom_8);
  end Has_Bom_8;


  function Has_Skipped_Bom_8 (The_File : in out Ada.Text_IO.File_Type) return Boolean is
    The_Header : Standard.String(1..3);
  begin
    begin
      Ada.Text_IO.Get (The_File, The_Header);
      if The_Header = Bom_8 then
        return True;
      end if;
    exception
    when others =>
      null;
    end;
    Ada.Text_IO.Reset (The_File);
    return False;
  end Has_Skipped_Bom_8;


  function Removed_Bom_8 (Item : Standard.String) return Standard.String is
  begin
    if Has_Bom_8 (Item) then
      return Item(Item'first + Bom_8'length .. Item'last);
    end if;
    return Item;
  end Removed_Bom_8;


  function Is_Utf8_Encoded (Item : Standard.String) return Boolean is

    type Bits is mod 2**8;

    function Convert is new Ada.Unchecked_Conversion (Character, Bits);

    The_Index : Natural := Item'first;

    function Has_Utf8_Data (The_Count : Positive) return Boolean is
    begin
      for Index in The_Index + 1 .. The_Index + The_Count loop
        if Index > Item'last or else (Convert (Item(Index)) and 16#C0#) /= 16#80# then
          return False;
        end if;
      end loop;
      return True;
    end Has_Utf8_Data;

  begin -- Is_Utf8_Encoded
    if Has_Bom_8 (Item) then
      return True;
    end if;
    while The_Index < Item'last loop
      if Item(The_Index) >= Character'val(16#C0#) then
        declare
          The_Bits : constant Bits := Convert (Item(The_Index));
        begin
          if (The_Bits and 16#E0#) = 16#C0# then
            return Has_Utf8_Data (The_Count => 1);
          elsif (The_Bits and 16#F0#) = 16#E0# then
            return Has_Utf8_Data (The_Count => 2);
          elsif (The_Bits and 16#F8#) = 16#F0# then
            return Has_Utf8_Data (The_Count => 3);
          end if;
        end;
      end if;
      The_Index := The_Index + 1;
    end loop;
    return False;
  end Is_Utf8_Encoded;


  function Ansi_Of_Utf8 (Item : Standard.String) return Standard.String is
  begin
    if Is_Utf8_Encoded (Item) then
      return Ada.Strings.UTF_Encoding.Strings.Decode (Item);
    end if;
    return Item;
  exception
  when others =>
    return Item;
  end Ansi_Of_Utf8;


  function Utf8_Of (Item : Standard.String) return Standard.String is
  begin
    if not Is_Utf8_Encoded (Item) then
      return Ada.Strings.UTF_Encoding.Strings.Encode (Item);
    end if;
    return Item;
  exception
  when others =>
    return Item;
  end Utf8_Of;


  function Latin1_To_Ecp437 (The_Character : Character) return Character is
  begin
    if The_Character <= Last_Ascii then
      return The_Character;
    elsif The_Character in G1_Character then
      return Character'val (Latin1_To_Ecp437_Map (The_Character));
    else
      return Character'val (No_Map);
    end if;
  end Latin1_To_Ecp437;


  function Ecp437_To_Latin1 (The_Character : Character) return Character is
  begin
    if The_Character <= Last_Ascii then
      return The_Character;
    else
      return Character'val (Ecp437_To_Latin1_Map (The_Character));
    end if;
  end Ecp437_To_Latin1;


  function Cp850_To_Latin1 (The_Character : Character) return Character is
  begin
    if The_Character <= Last_Ascii then
      return The_Character;
    else
      return Character'val (Cp850_To_Latin1_Map (The_Character));
    end if;
  end Cp850_To_Latin1;


  function Ecp_Of (Ansi_String : Standard.String) return Standard.String is
  begin
    return Ada.Strings.Fixed.Translate (Ansi_String, Latin1_To_Ecp437'access);
  end Ecp_Of;


  function Ansi_Of_Ecp (Ecp_String : Standard.String) return Standard.String is
  begin
    return Ada.Strings.Fixed.Translate (Ecp_String, Ecp437_To_Latin1'access);
  end Ansi_Of_Ecp;


  function Ansi_Of_Cp850 (Cp850_String : Standard.String) return Standard.String is
  begin
    return Ada.Strings.Fixed.Translate (Cp850_String, Cp850_To_Latin1'access);
  end Ansi_Of_Cp850;

end Text;
