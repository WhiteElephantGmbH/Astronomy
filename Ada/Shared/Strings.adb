-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Strings is

  package Map renames Ada.Strings.Maps;

  pragma Warnings (Off); -- allow hiding predefined operator
  function "<" (Left  : Character;
                Right : Map.Character_Set) return Boolean renames Map.Is_In;
  pragma Warnings (On);

  Line_Break  : constant Map.Character_Set := Map.To_Set (Ascii.Cr & Ascii.Lf);
  White_Space : constant Map.Character_Set := Map.To_Set (Space & Ascii.Ht & Ascii.Cr & Ascii.Lf);


  procedure Set (The_Element : in out Element;
                 New_Item    :        String) is
  begin
    The_Element := To_Unbounded_String (New_Item);
  end Set;


  function Is_Null (The_Element : Element) return Boolean is
  begin
    return The_Element = Empty_Element;
  end Is_Null;


  procedure Clear (The_Element : in out Element) is
  begin
    The_Element := Empty_Element;
  end Clear;


  function "+" (The_Element : Element) return String is
  begin
    return To_String (The_Element);
  end "+";


  function "&" (Left  : String;
                Right : Element) return String is
  begin
    return Left & To_String (Right);
  end "&";


  function "&" (Left  : Element;
                Right : String) return String is
  begin
    return To_String (Left) & Right;
  end "&";


  function "+" (Left  : String;
                Right : List) return List is
    The_List : List := Right;
  begin
    The_List.Prepend (Left);
    return The_List;
  end "+";


  function "-" (Left  : List;
                Right : String) return List is
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


  function Is_Lowercase (The_String : String) return Boolean is
  begin
    for The_Index in The_String'range loop
      if Is_Uppercase (The_String(The_Index)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Lowercase;


  function Is_Uppercase (The_String : String) return Boolean is
  begin
    for The_Index in The_String'range loop
      if Is_Lowercase (The_String(The_Index)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Uppercase;


  function Found (The_Character : Character;
                  In_String     : String) return Boolean is
  begin
    for Character of In_String loop
      if Character = The_Character then
        return True;
      end if;
    end loop;
    return False;
  end Found;


  function Location_Of (The_Character : Character;
                        In_String     : String;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Ada.Strings.Fixed.Index (Source  => In_String,
                                    Pattern => [1 => The_Character],
                                    Going   => Ada.Strings.Direction(The_Direction));
  end Location_Of;


  function Location_Of (The_Character : Character;
                        In_String     : String;
                        From          : Positive;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Ada.Strings.Fixed.Index (Source  => In_String,
                                    Pattern => [1 => The_Character],
                                    From    => From,
                                    Going   => Ada.Strings.Direction(The_Direction));
  end Location_Of;


  function Location_Of (The_String    : String;
                        In_String     : String;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Ada.Strings.Fixed.Index (Source  => In_String,
                                    Pattern => The_String,
                                    Going   => Ada.Strings.Direction(The_Direction));
  end Location_Of;


  function Location_Of (The_String    : String;
                        In_String     : String;
                        From          : Positive;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Ada.Strings.Fixed.Index (Source  => In_String,
                                    Pattern => The_String,
                                    From    => From,
                                    Going   => Ada.Strings.Direction(The_Direction));
  end Location_Of;


  function Location_Of (The_String    : String;
                        In_String     : Element;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Strings.Index (Source  => In_String,
                          Pattern => The_String,
                          Going   => Ada.Strings.Direction(The_Direction));
  end Location_Of;


  function Location_Of (The_String    : String;
                        In_String     : Element;
                        From          : Positive;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Strings.Index (Source  => In_String,
                          Pattern => The_String,
                          From    => From,
                          Going   => Ada.Strings.Direction(The_Direction));
  end Location_Of;


  function Trimmed (Data : String) return String is
    The_First : Natural;
    The_Last  : Natural;
  begin
    if Data = "" then
      return "";
    end if;
    The_First := Data'first;
    The_Last  := Data'last;
    while The_First <= The_Last and then Data(The_First) <= ' ' loop
      The_First := The_First + 1;
    end loop;
    if The_First > The_Last then
      return "";
    end if;
    while Data(The_Last) <= ' ' loop
      The_Last := The_Last - 1;
    end loop;
    return Data(The_First .. The_Last);
  end Trimmed;


  function Trimmed_Leading (Data : String) return String is
    The_First : Natural;
  begin
    if Data = "" then
      return "";
    end if;
    The_First := Data'first;
    while The_First <= Data'last and then Data(The_First) <= ' ' loop
      The_First := The_First + 1;
    end loop;
    if The_First > Data'last then
      return "";
    end if;
    return Data(The_First .. Data'last);
  end Trimmed_Leading;


  function Trimmed_Trailing (Data : String) return String is
    The_Last : Natural;
  begin
    if Data = "" then
      return "";
    end if;
    The_Last := Data'last;
    while The_Last >= Data'first and then Data(The_Last) <= ' ' loop
      The_Last := The_Last - 1;
    end loop;
    return Data(Data'first .. The_Last);
  end Trimmed_Trailing;


  function Trimmed (Data : String;
                    What : Character) return String is
    The_First : Natural;
    The_Last  : Natural;
  begin
    if Data = "" then
      return "";
    end if;
    The_First := Data'first;
    The_Last  := Data'last;
    if The_First <= The_Last and then Data(The_First) = What then
      The_First := The_First + 1;
    end if;
    if The_First > The_Last then
      return "";
    end if;
    if Data(The_Last) = What then
      The_Last := The_Last - 1;
    end if;
    return Data(The_First .. The_Last);
  end Trimmed;


  function Purge_Of (Data : String) return String is
    The_Data : String(Data'first .. Data'last);
    The_Last : Natural := Data'first - 1;
  begin
    for The_Character of Data loop
      if The_Character > ' ' then
        The_Last := The_Last + 1;
        The_Data(The_Last) := The_Character;
      end if;
    end loop;
    return The_Data(Data'first..The_Last);
  end Purge_Of;


  function Has_Bom_8 (Data : String) return Boolean is
  begin
    return (Data'length >= Bom_8'length) and then (Data(Data'first .. Data'first + Bom_8'length - 1) = Bom_8);
  end Has_Bom_8;


  function Has_Skipped_Bom_8 (The_File : in out Ada.Text_IO.File_Type) return Boolean is
    The_Header : String(1..3);
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


  function Removed_Bom_8 (Data : String) return String is
  begin
    if Has_Bom_8 (Data) then
      return Data(Data'first + Bom_8'length .. Data'last);
    end if;
    return Data;
  end Removed_Bom_8;


  function Is_Utf8_Encoded (Data : String) return Boolean is

    type Bits is mod 2**8;

    function Convert is new Ada.Unchecked_Conversion (Character, Bits);

    The_Index : Natural := Data'first;

    function Has_Utf8_Data (The_Count : Positive) return Boolean is
    begin
      for Index in The_Index + 1 .. The_Index + The_Count loop
        if Index > Data'last or else (Convert (Data(Index)) and 16#C0#) /= 16#80# then
          return False;
        end if;
      end loop;
      return True;
    end Has_Utf8_Data;

  begin -- Is_Utf8_Encoded
    if Has_Bom_8 (Data) then
      return True;
    end if;
    while The_Index < Data'last loop
      if Data(The_Index) >= Character'val(16#C0#) then
        declare
          The_Bits : constant Bits := Convert (Data(The_Index));
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


  function Ansi_Of (Data : String) return String is
  begin
    if Is_Utf8_Encoded (Data) then
      return Ada.Strings.UTF_Encoding.Strings.Decode (Data);
    end if;
    return Data;
  exception
  when others =>
    return Data;
  end Ansi_Of;


  function Utf8_Of (Data : String) return String is
  begin
    if not Is_Utf8_Encoded (Data) then
      return Ada.Strings.UTF_Encoding.Strings.Encode (Data);
    end if;
    return Data;
  exception
  when others =>
    return Data;
  end Utf8_Of;


  function Concatenation_Of (The_List  : List;
                             Separator : String := " ") return String is
    The_Length : Natural := 0;
  begin
    for Text of The_List loop
      The_Length := The_Length + Text'length + Separator'length;
    end loop;
    declare
      The_String : String(1..The_Length);
      The_First  : Positive;
      The_Last   : Natural := The_String'first - 1;
    begin
      for Text of The_List loop
        The_First := The_Last + 1;
        The_Last := The_First + Text'length + Separator'length - 1;
        The_String (The_First .. The_Last) := Text & Separator;
      end loop;
      return The_String;
    end;
  end Concatenation_Of;


  function Legible_Of (The_String : String) return String is
    Legible_String : String (1 .. The_String'length);
    The_Size       : Natural := 0;
    Am_In_Gap      : Boolean := True;
    The_Character  : Character;
  begin
    for The_Index in The_String'range loop
      The_Size := The_Size + 1;
      The_Character := The_String (The_Index);
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
      The_Size := The_Size - 1;
    end if;
    return Legible_String (1 .. The_Size);
  end Legible_Of;


  function Reduced (The_String : String) return String is

    type Separator is (No_Separator, Blank, New_Line);

    Reduced_String : String (1 .. The_String'length);
    The_Size       : Natural := 0;
    The_Separator  : Separator := No_Separator;
    The_Character  : Character;

  begin
    for The_Index in The_String'range loop
      The_Character := The_String (The_Index);
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
        The_Size := The_Size + 1;
        case The_Separator is
        when Blank =>
          Reduced_String(The_Size) := Space;
          The_Size := The_Size + 1;
        when New_Line =>
          Reduced_String (The_Size) := Ascii.Cr;
          The_Size := The_Size + 1;
        when No_Separator =>
          null;
        end case;
        The_Separator := No_Separator;
        Reduced_String (The_Size) := The_Character;
      end if;
    end loop;
    return Reduced_String (1 .. The_Size);
  end Reduced;


  function Image_Of (The_Value : Kind) return String is
  begin
    return Trimmed (The_Value'img);
  end Image_Of;


  function Legible_Image_Of (The_Value : Kind) return String is
  begin
    return Legible_Of (The_Value'img);
  end Legible_Image_Of;


  function Padded_Image_Of (The_Value : Kind;
                            Padding   : Character := '0') return String is

    function Image is new Image_Of (Kind);

    The_Image : constant String  := Image(The_Value);

  begin -- Padded_Image_Of
    if The_Value < Kind'val(0) then
      raise Usage_Error;
    end if;
    return Ada.Strings.Fixed."*"(Kind'width - The_Image'length - 1, Padding) & The_Image;
  end Padded_Image_Of;


  function File_Extension_Of (The_Filename : String) return String is
  begin
    for The_Position in reverse The_Filename'range loop
      exit when (The_Filename (The_Position) = '\') or (The_Filename (The_Position) = '/');
      if The_Filename (The_Position) = '.' then
        return Lowercase_Of (The_Filename (The_Position + 1 .. The_Filename'last));
      end if;
    end loop;
    return "";
  end File_Extension_Of;

-------------------------------------------------------------------------------

  procedure Append (The_Item : in out Item;
                    Data     :        String) is
    First_Position : constant Natural := The_Item.Length + 1;
  begin
    The_Item.Count := @ + 1;
    The_Item.Length := @ + Data'length;
    The_Item.Positions(The_Item.Count) := Position(First_Position);
    The_Item.Data(First_Position .. The_Item.Length) := Data;
  end Append;

  type List_Access is access all Item;
  for List_Access'storage_size use 0;

  type Iterator is new List_Iterator_Interfaces.Reversible_Iterator with record
    Container : List_Access;
  end record;

  overriding
  function First (Object : Iterator) return Element_Count;

  overriding
  function Last (Object : Iterator) return Element_Count;

  overriding
  function Next (Object : Iterator;
                 Index  : Element_Count) return Element_Count;

  overriding
  function Previous (Object : Iterator;
                     Index  : Element_Count) return Element_Count;


  function First (Object : Iterator) return Element_Count is
  begin
    if Object.Container.Count > 0 then
      return First_Index;
    else
      return No_Element;
    end if;
  end First;


  function Last (Object : Iterator) return Element_Count is
  begin
    return Object.Container.Count;
  end Last;


  function Next (Object : Iterator;
                 Index  : Element_Count) return Element_Count is
  begin
    if Index = No_Element then
      return No_Element;
    end if;
    if Index = Object.Container.Count then
      return No_Element;
    else
      return Index + 1;
    end if;
  end Next;


  function Previous (Object : Iterator;
                     Index  : Element_Count) return Element_Count is
    pragma Unreferenced (Object);
  begin
    if Index = No_Element then
      return No_Element;
    end if;
    return Index - 1;
  end Previous;


  function Constant_Reference (The_Item : aliased Item;
                               Index    : Element_Count) return String is
  begin
    declare
      The_Position : constant Position := The_Item.Positions(Index);
    begin
      if Index < The_Item.Count then
        return The_Item.Data(Positive(The_Position) .. Positive(The_Item.Positions(Index + 1)) - 1);
      end if;
      return The_Item.Data(Positive(The_Position) .. The_Item.Length);
    end;
  exception
  when others =>
    raise Constraint_Error with "index out of range";
  end Constant_Reference;


  function Has_Element (Index : Element_Count) return Boolean is
  begin
    return Index /= No_Element;
  end Has_Element;


  function Iterate (The_List : Item) return List_Iterator_Interfaces.Reversible_Iterator'class is
  begin
    return Iterator'(Container => The_List'unrestricted_access);
  end Iterate;


  procedure Put_Item_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                            V : Item) is
  begin
    S.Put ((if V.Count = 0 then "[]" else "[""" & V.To_Data (Separator => """, """) & """]"));
  end Put_Item_Image;


  function Length_At (The_Item : Item;
                      Index    : Element_Index) return Natural is

    List_Position : constant Position := The_Item.Positions(Index);

  begin
    if Index < The_Item.Count then
      return Natural(The_Item.Positions(Index + 1) - List_Position);
    end if;
    return The_Item.Data'last + 1 - Natural(List_Position);
  end Length_At;


  function First (The_Item : Item) return String is
  begin
    return The_Item(First_Index);
  end First;


  function Last (The_Item : Item) return String is
  begin
    return The_Item(The_Item.Count);
  end Last;


  function To_Data (The_Item  : Item;
                    Separator : String := "") return String is
  begin
    if The_Item.Count = 0 then
      return "";
    else
      declare
        The_Data     : String(1 .. (The_Item.Count - 1) * Separator'length + The_Item.Length);
        The_Position : Positive := First_Index;
        Is_First     : Boolean := True;
      begin
        for Element of The_Item loop
          declare
            Data : constant String := (if Is_First then "" else Separator) & Element;
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


  function To_Data (The_List  : List;
                    Separator : String := "") return String is
    The_Length : Natural := The_List.Count;
  begin
    if The_Length = 0 then
      return "";
    elsif The_Length = 1 then
      return The_List.First_Element;
    else
      The_Length := The_Length * Separator'length;
      for Element of The_List loop
        The_Length := The_Length + Element'length;
      end loop;
      declare
        The_Data     : String(1 .. The_Length);
        The_Position : Natural := First_Index;
      begin
        for Element of The_List loop
          declare
            Data : constant String := Element & Separator;
          begin
            The_Data(The_Position .. The_Position + Data'length - 1) := Data;
            The_Position := The_Position + Data'length;
          end;
        end loop;
        return The_Data(The_Data'first .. The_Length - Separator'length);
      end;
    end if;
  end To_Data;


  function Item_Of (Data      : String;
                    Separator : Character;
                    Purge     : Boolean := True;
                    Symbols   : String := "") return Item is
    The_Item  : Item;
    The_First : Natural := Data'first;
  begin
    for Index in Data'range loop
      if Data(Index) = Separator then
        if not Purge or else Index /= The_First then
          The_Item.Append (Data(The_First .. Index - 1));
        end if;
        The_First := Index + 1;
      elsif Found (Data(Index), Symbols) then
        if not Purge or else Index /= The_First then
          The_Item.Append (Data(The_First .. Index - 1));
        end if;
        The_Item.Append (Data(Index .. Index));
        The_First := Index + 1;
      end if;
    end loop;
    if not Purge or else The_First <= Data'last then
      The_Item.Append (Data(The_First .. Data'last));
    end if;
    return The_Item;
  end Item_Of;


  function Part (The_Item  : Item;
                 From      : Element_Index;
                 To        : Element_Count) return Item is
    New_Item : Item;
  begin
    if To < From then
      return None;
    end if;
    for Index in From .. To loop
      New_Item.Append (The_Item(Index));
    end loop;
    return New_Item;
  end Part;


  function "+" (Left  : Item;
                Right : Item) return Item is
    New_Item : Item := Left;
  begin
    New_Item.Positions(Left.Count + 1 .. Left.Count + Right.Count) := Right.Positions(First_Index .. Right.Count);
    for Index in Left.Count + 1 .. Left.Count + Right.Count loop
      New_Item.Positions(Index) := @ + Position(Left.Length);
    end loop;
    New_Item.Data(Left.Length + 1 .. Left.Length + Right.Length) := Right.Data (First_Index .. Right.Length);
    New_Item.Count := Left.Count + Right.Count;
    New_Item.Length := Left.Length + Right.Length;
    return New_Item;
  end "+";


  procedure Prepend (The_Item : in out Item;
                     Data     :        String) is
    Data_Item : Item;
  begin
    Append (Data_Item, Data);
    The_Item := Data_Item + The_Item;
  end Prepend;


  function Contains (The_Item : Item;
                     Name     : String) return Boolean is
  begin
    for The_Name of The_Item loop
      if The_Name = Name then
        return True;
      end if;
    end loop;
    return False;
  end Contains;


  function To_List (The_Item : Item'class) return List is
    The_List : List;
  begin
    for Name of The_Item loop
      The_List.Append (Name);
    end loop;
    return The_List;
  end To_List;


  function To_Trimmed_List (The_Item : Item'class) return List is
    The_List : List;
  begin
    for Name of The_Item loop
      The_List.Append (Trimmed (Name));
    end loop;
    return The_List;
  end To_Trimmed_List;

end Strings;
