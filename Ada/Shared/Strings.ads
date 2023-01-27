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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Text_Buffers;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;

package Strings is

  type Direction is new Ada.Strings.Direction;

  type Element is new Ada.Strings.Unbounded.Unbounded_String
  with
    Aggregate => (Empty       => Empty_Element,
                  Add_Unnamed => Set);

  Empty_Element : constant Element;

  procedure Set (The_Element : in out Element;
                 New_Item    :        String) with Inline;

  function Is_Null (The_Element : Element) return Boolean with Inline;

  procedure Clear (The_Element : in out Element) with Inline;

  function "+" (The_Element : Element) return String with Inline;

  function "&" (Left  : String;
                Right : Element) return String with Inline;

  function "&" (Left  : Element;
                Right : String) return String with Inline;

  function Location_Of (The_String    : String;
                        In_String     : Element;
                        The_Direction : Direction := Forward) return Natural;
  function Location_Of (The_String    : String;
                        In_String     : Element;
                        From          : Positive;
                        The_Direction : Direction := Forward) return Natural;
  -- The procedure returns the position of the match or in case of no match Not_Found.

  package Linked_Strings is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

  type List is new Linked_Strings.List with private
  with
    Aggregate => (Empty       => Empty,
                  Add_Unnamed => Append);

  Empty : constant List;

  function "+" (Left  : String;
                Right : List) return List;

  function "-" (Left  : List;
                Right : String) return List;
  -- remove string (Constraint_Error if not in List)

  function Count (The_List : List) return Natural is (Natural(Length (The_List))) with Inline;


  function Is_Equal (Left, Right : String) return Boolean renames Ada.Strings.Equal_Case_Insensitive;

  Space : constant Character := ' ';

  function Lowercase_Of (The_Character : Character) return Character
           renames Ada.Characters.Handling.To_Lower;

  function Lowercase_Of (The_String : String) return String
           renames Ada.Characters.Handling.To_Lower;

  function Uppercase_Of (The_Character : Character) return Character
           renames Ada.Characters.Handling.To_Upper;

  function Uppercase_Of (The_String : String) return String
           renames Ada.Characters.Handling.To_Upper;

  function Is_Lowercase (The_Character : Character) return Boolean
           renames Ada.Characters.Handling.Is_Lower;

  function Is_Lowercase (The_String : String) return Boolean;
  -- Returns TRUE if all the letters in the string are lowercase.

  function Is_Uppercase (The_Character : Character) return Boolean
           renames Ada.Characters.Handling.Is_Upper;

  function Is_Uppercase (The_String : String) return Boolean;
  -- Returns TRUE if all the letters in the string are uppercase.

  function Found (The_Character : Character;
                  In_String     : String) return Boolean;

  Not_Found : constant Natural := 0;

  function Location_Of (The_Character : Character;
                        In_String     : String;
                        The_Direction : Direction := Forward) return Natural;
  function Location_Of (The_Character : Character;
                        In_String     : String;
                        From          : Positive;
                        The_Direction : Direction := Forward) return Natural;
  function Location_Of (The_String    : String;
                        In_String     : String;
                        The_Direction : Direction := Forward) return Natural;
  function Location_Of (The_String    : String;
                        In_String     : String;
                        From          : Positive;
                        The_Direction : Direction := Forward) return Natural;
  -- The procedure returns the position of the match or in case of no match Not_Found.


  function Trimmed (Data : String) return String;
  -- Trims left and right whitespace.

  function Trimmed_Leading (Data : String) return String;
  -- Trims left whitespace.

  function Trimmed_Trailing (Data : String) return String;
  -- Trims right whitespace.

  function Trimmed (Data : String;
                    What : Character) return String;
  -- Removes left and right one character.

  function Purge_Of (Data : String) return String;
  -- Removes all whitespace.

  Bom_8 : constant String := Ada.Strings.UTF_Encoding.BOM_8;

  function Has_Bom_8 (Data : String) return Boolean with Inline;
  -- Has leading BOM 8.

  function Has_Skipped_Bom_8 (The_File : in out Ada.Text_IO.File_Type) return Boolean;
  -- Precondition: The file must be open.
  -- Skips a leading BOM 8, leaving the file pointer after the BOM (if any).

  function Removed_Bom_8 (Data : String) return String;
  -- returns the string without the leading BOM 8.

  function Is_Utf8_Encoded (Data : String) return Boolean;
  -- returns true if the string begins with BOM 8 or contains any UTF8 characters.

  function Ansi_Of (Data : String) return String;
  -- if the string is UTF8 encoded then it is converted to ANSI

  function Utf8_Of (Data : String) return String;
  -- if is not UTF8 encoded then it is converted to UTF8 without BOM

  function Concatenation_Of (The_List  : List;
                             Separator : String := " ") return String;


  function File_Extension_Of (The_Filename : String) return String;
  -- Function to return, in lowercase, the file extension part of a file name.

  function Legible_Of (The_String : String) return String;
  -- Function to return a "legible" version of the supplied string.
  --    Underscores are replaced with spaces.
  --    Leading & trailing spaces are removed.
  --    Multiple spaces are condensed into a single space.
  --    The first character and characters following a space are
  --    capitalized, otherwise the string is reduced to lowercase.

  function Reduced (The_String : String) return String;
  -- Function to return a "reduced" version of the supplied string.
  --    white_spaces :== {<cr> | <lf> | <tab> | space}.
  --    Leading & trailing white_spaces are removed.
  --    {<tab> | space} replaced with a single space.
  --    [white_spaces] (<cr> | <lf>) [white_spaces] replaced with a single <cr>.


  generic
    type Kind is (<>);
  function Image_Of (The_Value : Kind) return String;

  generic
    type Kind is (<>);
  function Legible_Image_Of (The_Value : Kind) return String;

  Usage_Error : exception;
  generic
    type Kind is (<>);
  function Padded_Image_Of (The_Value : Kind;
                            Padding   : Character := '0') return String;

------------------------------------------------------------------------------------------------------------------------

  Max_Count       : constant := 2**8-1;
  Max_Data_Length : constant := 2**12-1;

  First_Index : constant := 1;

  subtype Element_Count is Natural range 0 .. Max_Count;
  subtype Element_Index is Element_Count range First_Index .. Element_Count'last;

  subtype Data_Length is Natural range 0 .. Max_Data_Length;

  type Item is tagged private --String of 'Standard.STRING'
  with
    Aggregate => (Empty       => None,
                  Add_Unnamed => Append),
    Constant_Indexing => Constant_Reference,
    Default_Iterator  => Iterate,
    Iterator_Element  => String,
    Relaxed_Initialization;

  None : constant Item;

  procedure Append (The_Item : in out Item;
                    Data     :        String)
  with
    Inline,
    Pre => Data'length <= The_Item.Free_Space and The_Item.Count < Max_Count;

  No_Element : constant Element_Count := 0;

  function Has_Element (Index : Element_Count) return Boolean;

  package List_Iterator_Interfaces is new Ada.Iterator_Interfaces (Element_Count, Has_Element);

  function Constant_Reference (The_Item : aliased Item;
                               Index    : Element_Count) return String with Inline;

  function Iterate (The_List : Item) return List_Iterator_Interfaces.Reversible_Iterator'class;


  function Count (The_Item : Item) return Natural;

  function Free_Space (The_Item : Item) return Natural;

  function Length_At (The_Item : Item;
                      Index    : Element_Index) return Natural with Inline;


  function First (The_Item : Item) return String with Inline;


  function Last (The_Item : Item) return String with Inline;


  function To_Data (The_Item  : Item;
                    Separator : String := "") return String with Inline;

  function To_Data (The_List  : List;
                    Separator : String := "") return String;

  function Item_Of (Data      : String;
                    Separator : Character;
                    Purge     : Boolean := True;
                    Symbols   : String := "") return Item;
  -- Returns the splitted strings from Data.
  --   Example: Data = "," and Separator = ',' and Purge = False results in two empty strings.
  -- Per default (Purge = True) empty strings are removed.
  -- Symbols are added to the result.


  function Part (The_Item : Item;
                 From     : Element_Index;
                 To       : Element_Count) return Item
    with Pre => To <= The_Item.Count and then To - From + 1 >= 0;


  function "+" (Left  : Item;
                Right : Item) return Item
  with
    Pre => (Left.Count + Right.Count) <= Max_Count and then (Left.Free_Space + Right.Free_Space) >= Max_Data_Length;
  -- Concatenate

  procedure Prepend (The_Item : in out Item;
                     Data     :        String)
  with
    Pre => Data'length <= The_Item.Free_Space and The_Item.Count < Max_Count;

  function Contains (The_Item : Item;
                     Name     : String) return Boolean;

  function To_List (The_Item : Item'class) return List;

  function To_Trimmed_List (The_Item : Item'class) return List;

private

  Empty_Element : constant Element := Element(Ada.Strings.Unbounded.Null_Unbounded_String);

  type List is new Linked_Strings.List with null record
  with Put_Image => Put_List_Image;

  procedure Put_List_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                            V : List);

  Empty : constant List := (Linked_Strings.Empty_List with null record);

  type Position is new Data_Length range First_Index .. Data_Length'last;

  type Element_Positions is array (Element_Index) of Position with Pack, Suppress_Initialization;

  subtype String_Data is String (First_Index .. Max_Data_Length) with Suppress_Initialization;

  type Item is tagged record
    Count     : Element_Count := 0;
    Length    : Data_Length := 0;
    Positions : Element_Positions;
    Data      : String_Data;
  end record
  with Put_Image => Put_Item_Image;

  procedure Put_Item_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                            V : Item);

  None : constant Item := (Count => 0, Length => 0, others => <>);

  function Count (The_Item : Item) return Natural is (The_Item.Count);

  function Free_Space (The_Item : Item) return Natural is (Max_Data_Length - The_Item.Length);

end Strings;
