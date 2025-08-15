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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Text_Buffers;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;

package Text is

  type Direction is new Ada.Strings.Direction;

  Start_Of_String : constant Positive := Positive'first;

  --*******************************************************************************************************************
  --**  Standard String                                                                                              **
  --*******************************************************************************************************************

  function Matches (Left, Right : Standard.String) return Boolean renames Ada.Strings.Equal_Case_Insensitive;

  Space : constant Character := ' ';

  function Lowercase_Of (Item : Character) return Character
           renames Ada.Characters.Handling.To_Lower;

  function Lowercase_Of (Item : Standard.String) return Standard.String
           renames Ada.Characters.Handling.To_Lower;

  function Uppercase_Of (Item : Character) return Character
           renames Ada.Characters.Handling.To_Upper;

  function Uppercase_Of (Item : Standard.String) return Standard.String
           renames Ada.Characters.Handling.To_Upper;

  function Is_Lowercase (Item : Character) return Boolean
           renames Ada.Characters.Handling.Is_Lower;

  function Is_Lowercase (Item : Standard.String) return Boolean;
  -- Returns TRUE if all the letters in the string are lowercase.

  function Is_Uppercase (Item : Character) return Boolean
           renames Ada.Characters.Handling.Is_Upper;

  function Is_Uppercase (Item : Standard.String) return Boolean;
  -- Returns TRUE if all the letters in the string are uppercase.

  function Found (Item      : Character;
                  In_String : Standard.String) return Boolean;

  Not_Found : constant Natural := 0;

  function Location_Of (Item          : Character;
                        In_String     : Standard.String;
                        From          : Positive := Start_Of_String; -- from end if backwards
                        The_Direction : Direction := Forward) return Natural
  -- The procedure returns ether the position of the found string or Not_Found.
  with
    Inline,
    Pre => From = Start_Of_String or else (From >= In_String'first and From <= In_String'last);

  function Location_Of (Pattern       : Standard.String;
                        In_String     : Standard.String;
                        From          : Positive := Start_Of_String; -- from end if backwards
                        The_Direction : Direction := Forward) return Natural
  -- The procedure returns ether the position of the found string or Not_Found.
  with
    Inline,
    Pre => From = Start_Of_String or else
           (if The_Direction = Forward then
              From >= In_String'first and From <= In_String'last - Pattern'length + 1
            else -- Backward
              From >= In_String'first + Pattern'length - 1 and From <= In_String'last);


  function Trimmed (Item : Standard.String) return Standard.String;
  -- Trims left and right whitespace.

  function Trimmed_Leading (Item : Standard.String) return Standard.String;
  -- Trims left whitespace.

  function Trimmed_Leading (Item : Standard.String;
                            What : Character) return Standard.String;
  -- Trims left characters.

  function Trimmed_Trailing (Item : Standard.String) return Standard.String;
  -- Trims right whitespace.

  function Trimmed_Trailing (Item : Standard.String;
                             What : Character) return Standard.String;
  -- Trims right characters.

  function Trimmed (Item : Standard.String;
                    What : Character) return Standard.String;
  -- Removes left and right one character.


  function Purge_Of (Item : Standard.String) return Standard.String;
  -- Removes all whitespace.

  function Capitalized_Of (Item : Standard.String) return Standard.String;

  function Legible_Of (Item : Standard.String) return Standard.String;
  -- Function to return a "legible" version of the supplied string.
  --    Underscores are replaced with spaces.
  --    Leading & trailing spaces are removed.
  --    Multiple spaces are condensed into a single space.
  --    The first character and characters following a space or a '.'
  --    are capitalized, otherwise the string is reduced to lowercase.

  function Reduced (Item : Standard.String) return Standard.String;
  -- Function to return a "reduced" version of the supplied string.
  --    white_spaces :== {<cr> | <lf> | <tab> | space}.
  --    Leading & trailing white_spaces are removed.
  --    {<tab> | space} replaced with a single space.
  --    [white_spaces] (<cr> | <lf>) [white_spaces] replaced with a single <cr>.


  generic
    type Kind is (<>);
  function Image_Of (Item : Kind) return Standard.String;

  generic
    type Kind is (<>);
  function Legible_Image_Of (Item : Kind) return Standard.String;

  Usage_Error : exception;
  generic
    type Kind is (<>);
  function Padded_Image_Of (Item    : Kind;
                            Padding : Character := '0') return Standard.String;

  function File_Extension_Of (The_Filename : Standard.String) return Standard.String;
  -- Function to return, in lowercase, the file extension part of a file name.


  --*******************************************************************************************************************
  --**  Unbounded of String                                                                                          **
  --*******************************************************************************************************************

  package Character_Array is new Ada.Containers.Vectors (Positive, Character);

  type String is new Character_Array.Vector with private
  with
    Aggregate => (Empty       => Empty_String,
                  Add_Unnamed => Append);

  function Empty_String return String with Inline;

  function Count (Item : String) return Natural is (Natural(Character_Array.Length(Character_Array.Vector(Item))));

  function Is_Null (Item : String) return Boolean with Inline; -- same as Is_Empty or Count = 0

  function String_Of (Item : Standard.String) return String with Inline;

  function To_String (Item : String) return Standard.String with Inline;

  function "+" (Item : String) return Standard.String renames To_String;

  function First (Item : String) return Character with Inline;

  function Last (Item : String) return Character with Inline;

  function "=" (Left : String; Right : Standard.String) return Boolean is (Left.To_String = Right);
  function "=" (Left : Standard.String; Right : String) return Boolean is (Left = Right.To_String);

  function "<" (Left : String; Right : String)          return Boolean is (Left.To_String < Right.To_String);
  function "<" (Left : Standard.String; Right : String) return Boolean is (Left < Right.To_String);
  function "<" (Left : String; Right : Standard.String) return Boolean is (Left.To_String < Right);

  function "<=" (Left : String; Right : String)          return Boolean is (Left.To_String <= Right.To_String);
  function "<=" (Left : Standard.String; Right : String) return Boolean is (Left <= Right.To_String);
  function "<=" (Left : String; Right : Standard.String) return Boolean is (Left.To_String <= Right);

  function Matches (Left, Right : String) return Boolean is (Matches (Left.To_String, Right.To_String));

  function Matches (Left : Standard.String; Right : String) return Boolean is (Matches (Left, Right.To_String));

  function Matches (Left : String; Right : Standard.String) return Boolean is (Matches (Left.To_String, Right));

--procedure Clear (Item : in out String); -- inherited

--function Empty (Capacity : Ada.Containers.Count_Type := 10) return String; -- inherited

--procedure Append (Item : in out String;
--                  Data :        Character); -- inherited

  procedure Append (Item : in out String;
                    Data :        Standard.String) with Inline;

--procedure Prepend (Item : in out String;
--                   Data :        Character); -- inherited

  procedure Prepend (Item : in out String;
                     Data :        Standard.String) with Inline;

  procedure Insert (In_String : in out String;
                    Data      :        Standard.String;
                    At_Index  :        Positive) with Inline;

--function "&" (Left  : String;
--              Right : String) return String; -- inherited

  function "&" (Left  : String;
                Right : Standard.String) return String is (Left & String_Of (Right));

  function "&" (Left  : Standard.String;
                Right : String) return String is (String_Of (Left) & Right);

  function "&" (Left  : Standard.String;
                Right : String) return Standard.String is (Left & Right.To_String);

  function "&" (Left  : String;
                Right : Standard.String) return Standard.String is (Left.To_String & Right);

  function Index_Of (In_String     : String;
                     Item          : Character;
                     From          : Positive := Start_Of_String; -- from end if backwards
                     The_Direction : Direction := Forward) return Natural;
  function Index_Of (In_String     : String;
                     Pattern       : Standard.String;
                     From          : Positive := Start_Of_String; -- from end if backwards
                     The_Direction : Direction := Forward) return Natural;
  -- The procedure returns ether the position of the found string or Not_Found (calls Location_Of).

  procedure Replace (In_String : in out String;
                     From      :        Positive;
                     To        :        Natural;
                     By        :        Standard.String) with Inline;

  procedure Replace (In_String : in out String;
                     Data      :        Standard.String;
                     By        :        Standard.String) with Inline;

  procedure Update (Item : in out String;
                    From :        Positive;
                    By   :        Standard.String);

  procedure Make_Lowercase (Item : in out String);

  procedure Make_Uppercase (Item : in out String);

  function To_Lowercase (Item : String) return Standard.String is (Lowercase_Of (Item.To_String));

  function To_Uppercase (Item : String) return Standard.String is (Uppercase_Of (Item.To_String));

  function Slice (Item : String;
                  From : Positive;
                  To   : Natural) return Standard.String with Inline;

  procedure Trim (Item : in out String);
  -- Trims left and right whitespace.

  function Trimmed (Item : String) return Standard.String is (Trimmed (Item.To_String));
  -- Trims left and right whitespace.

  procedure Truncate (Item       : in out String;
                      New_Length : Natural);

  function Truncation_Of (Item       : String;
                          New_Length : Natural) return String;

  --  This procedure sets the length of the string. If the specified length is
  --  greater than the current length of the string the current length remains
  --  unchanged.

  --*******************************************************************************************************************
  --**  List of Strings                                                                                              **
  --*******************************************************************************************************************

  package Linked_Strings is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Standard.String);

  type List is new Linked_Strings.List with private
  with
    Aggregate => (Empty       => Empty_List,
                  Add_Unnamed => Append);

  function Empty_List return List with Inline;

  function "+" (Left  : List;
                Right : Standard.String) return List;

  function "+" (Left  : Standard.String;
                Right : List) return List;

  function "+" (Left  : List;
                Right : List) return List;

  function "-" (Left  : List;
                Right : Standard.String) return List;
  -- remove string (Constraint_Error if not in List)

  function Count (Item : List) return Natural is (Natural(Length (Item))) with Inline;

  function Concatenation (Item      : List;
                          Separator : Standard.String := " ") return Standard.String;

  function To_Data (Item      : List;
                    Separator : Standard.String := "") return Standard.String;

  function List_Of (Item      : Standard.String;
                    Separator : Character;
                    Purge     : Boolean := True;
                    Do_Trim   : Boolean := True;
                    Symbols   : Standard.String := "") return List;
  -- Returns the splitted strings from Data.
  --   Example: Data = "," and Separator = ',' and Purge = False results in two empty strings.
  -- Per default (Purge = True) empty strings are removed and (Do_Trim = True) strings are trimmed.
  -- Symbols are added to the result.

  procedure Sort (Item : in out List);

  function Sorted (Item : List) return List;


  --*******************************************************************************************************************
  --**  Vector of Strings                                                                                            **
  --*******************************************************************************************************************

  package String_Array is new Ada.Containers.Indefinite_Vectors (Positive, Standard.String);

  type Vector is new String_Array.Vector with private
  with
    Aggregate => (Empty       => Empty_Vector,
                  Add_Unnamed => Append);

  function Empty_Vector return Vector with Inline;

  function Count (Item : Vector) return Natural is (Natural(Length (Item))) with Inline;

  function Part (Item  : Vector;
                 From  : Positive;
                 To    : Natural) return Vector
  with
    Pre    => From - 1 <= Item.Count and then To <= Item.Count,
    Post   => Part'result.Count = Natural'max (0, To - From + 1),
    Global => null;

  function To_List (Item : Vector'class) return List;

  function To_Vector (Item : List'class) return Vector;

  function To_Data (Item      : Vector;
                    Separator : Standard.String := "") return Standard.String;


  --*******************************************************************************************************************
  --**  Strings                                                                                                      **
  --*******************************************************************************************************************

  Max_Count       : constant := 2**8-1;
  Max_Data_Length : constant := 2**12-1;

  First_Index : constant := 1;

  subtype String_Count is Natural range 0 .. Max_Count;
  subtype String_Index is String_Count range First_Index .. String_Count'last;

  subtype Data_Length is Natural range 0 .. Max_Data_Length;

  type Strings is tagged private
  with
    Aggregate => (Empty       => None,
                  Add_Unnamed => Append),
    Constant_Indexing => Constant_Strings_Reference,
    Default_Iterator  => Strings_Iterate,
    Iterator_Element  => Standard.String,
    Relaxed_Initialization;

  function None return Strings with Inline;

  procedure Append (Item : in out Strings;
                    Data :        Standard.String)
  with
    Inline,
    Pre => Data'length <= Item.Free_Space and Item.Count < Max_Count;

  No_String : constant String_Count := 0;

  function Has_String (Index : String_Count) return Boolean;

  package Strings_Iterator_Interfaces is new Ada.Iterator_Interfaces (String_Count, Has_String);


  function Constant_Strings_Reference (Item  : aliased Strings;
                                       Index : String_Count) return Standard.String with Inline;

  function Strings_Iterate (Item : Strings) return Strings_Iterator_Interfaces.Reversible_Iterator'class;


  function Count (Item : Strings) return Natural;

  function Free_Space (Item : Strings) return Natural;

  function Length_At (Item  : Strings;
                      Index : String_Index) return Natural with Inline;


  function First (Item : Strings) return Standard.String with Inline;


  function Last (Item : Strings) return Standard.String with Inline;


  function To_Data (Item      : Strings;
                    Separator : Standard.String := "") return Standard.String;

  function Strings_Of (Item      : Standard.String;
                       Separator : Character;
                       Purge     : Boolean := True;
                       Do_Trim   : Boolean := True;
                       Symbols   : Standard.String := "") return Strings;
  -- Returns the splitted strings from Data.
  --   Example: Data = "," and Separator = ',' and Purge = False results in two empty strings.
  -- Per default (Purge = True) empty strings are removed and (Do_Trim = True) strings are trimmed.
  -- Symbols are added to the result.

  function "+" (Data : Standard.String) return Strings is (Strings_Of (Data, '|'));


  function Part (Item : Strings;
                 From : String_Index;
                 To   : String_Count) return Strings
    with Pre => To <= Item.Count and then To - From + 1 >= 0;


  function "+" (Left  : Strings;
                Right : Strings) return Strings
  with
    Pre => (Left.Count + Right.Count) <= Max_Count and then (Left.Free_Space + Right.Free_Space) >= Max_Data_Length;
  -- Concatenate

  procedure Prepend (Item : in out Strings;
                     Data :        Standard.String)
  with
    Pre => Data'length <= Item.Free_Space and Item.Count < Max_Count;

  function Contains (Item : Strings;
                     Name : Standard.String) return Boolean;

  function To_List (Item : Strings'class) return List;

  function To_Trimmed_List (Item : Strings'class) return List;


  --*******************************************************************************************************************
  --**  Conversion                                                                                                   **
  --*******************************************************************************************************************

  Bom_8 : constant Standard.String := Ada.Strings.UTF_Encoding.BOM_8;

  function Has_Bom_8 (Item : Standard.String) return Boolean with Inline;
  -- Has leading BOM 8.

  function Has_Skipped_Bom_8 (The_File : in out Ada.Text_IO.File_Type) return Boolean;
  -- Precondition: The file must be open.
  -- Skips a leading BOM 8, leaving the file pointer after the BOM (if any).

  function Removed_Bom_8 (Item : Standard.String) return Standard.String;
  -- returns the string without the leading BOM 8.

  function Is_Utf8_Encoded (Item : Standard.String) return Boolean;
  -- returns true if the string begins with BOM 8 or contains any UTF8 characters.

  function Ansi_Of_Utf8 (Item : Standard.String) return Standard.String;
  -- if the string is UTF8 encoded then it is converted to ANSI

  function Utf8_Of (Item : Standard.String) return Standard.String;
  -- if is not UTF8 encoded then it is converted to UTF8 without BOM

  function Latin1_To_Ecp437 (The_Character : Character) return Character;
  --
  -- Converts a ISO Latin_1 (8859-1) character (as used by Windows) into the equivalent ECP character variant
  -- (Code page) 437 (DOS Latin US) as used by DOS and Windows Console applications.
  --

  function Ecp437_To_Latin1 (The_Character : Character) return Character;
  --
  -- Converts a ECP character (as used by DOS and Windows Console applications) to the equivalent ISO Latin_1 (8859-1)
  -- character (as used by Windows) assuming ECP variant (Code page) 437 (DOS Latin US).
  --

  function Ecp_Of (Ansi_String : Standard.String) return Standard.String;
  --
  -- Converts an Ansi encoded string into the equivalent Ecp (Cp437)
  --

  function Ansi_Of_Ecp (Ecp_String : Standard.String) return Standard.String;
  --
  -- Converts an Ecp encoded string into the equivalent ANSI
  --

  function Ansi_Of_Cp850 (Cp850_String : Standard.String) return Standard.String;
  --
  -- Converts an Ecp encoded string into the equivalent ANSI
  --

  --*******************************************************************************************************************
  --**  Private                                                                                                      **
  --*******************************************************************************************************************

private

------------------------------------------------------------------------------------------------------------------------

  type String is new Character_Array.Vector with null record
  with Put_Image => Put_String_Image;

  procedure Put_String_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                              V :        String);

  function Empty_String return String is (Empty);

  Null_String : constant String := (Character_Array.Empty_Vector with null record);

------------------------------------------------------------------------------------------------------------------------

  type List is new Linked_Strings.List with null record
  with Put_Image => Put_List_Image;

  procedure Put_List_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                            V :        List);

  function Empty_List return List is ((Linked_Strings.Empty_List with null record));

------------------------------------------------------------------------------------------------------------------------

  type Vector is new String_Array.Vector with null record
  with Put_Image => Put_Vector_Image;

  procedure Put_Vector_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                              V :        Vector);

  function Empty_Vector return Vector is ((String_Array.Empty_Vector with null record));

------------------------------------------------------------------------------------------------------------------------

  type Position is new Data_Length range First_Index .. Data_Length'last;

  type String_Positions is array (String_Index) of Position with Pack, Suppress_Initialization;

  subtype String_Data is Standard.String (First_Index .. Max_Data_Length) with Suppress_Initialization;

  type Strings is tagged record
    Count     : String_Count := 0;
    Length    : Data_Length := 0;
    Positions : String_Positions;
    Data      : String_Data;
  end record
  with Put_Image => Put_Strings_Image;

  procedure Put_Strings_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                               V :        Strings);

  function None return Strings is ((Count => 0, Length => 0, others => <>));

  function Count (Item : Strings) return Natural is (Item.Count);

  function Free_Space (Item : Strings) return Natural is (Max_Data_Length - Item.Length);

end Text;
