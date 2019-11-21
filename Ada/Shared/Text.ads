-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Ada.Strings;
with Ada.Strings.Unbounded;

package Text is

  Index_Error : exception renames Ada.Strings.Index_Error;


  type String is new Ada.Strings.Unbounded.Unbounded_String;

  type Strings is array (Positive range <>) of String;

  type Strings_Access is access Strings;

  type Direction is new Ada.Strings.Direction;

  type Image is access Standard.String;

  type Image_List is array (Integer range <>) of Image;


  Start_Of_String : constant Positive := Positive'first;

  Start_Of_Search : constant := Natural'pred(Start_Of_String);

  Null_String : constant String := String (Ada.Strings.Unbounded.Null_Unbounded_String);


  --*******************************************************************************************************************
  --**  String_Of  & Strings_Of                                                                                      **
  --*******************************************************************************************************************

  function String_Of (The_Character : Character) return String;

  function String_Of (The_String : Standard.String) return String renames To_Unbounded_String;

  function String_Of (The_String : String) return Standard.String renames To_String;

  function String_Of (The_String    : String;
                      From_Position : Positive;
                      The_Size      : Natural) return String;

  function String_Of (The_String    : String;
                      From_Position : Positive;
                      The_Size      : Natural) return Standard.String;


  --*******************************************************************************************************************
  --**  Truncate & Truncation_Of                                                                                     **
  --*******************************************************************************************************************

  procedure Truncate (The_String     : in out String;
                      The_New_Length : Natural);

  function Truncation_Of (The_String     : String;
                          The_New_Length : Natural) return String;

  --  This procedure sets the length of the string. If the specified length is
  --  greater than the current length of the string the current length remains
  --  unchanged.


  --*******************************************************************************************************************
  --**  Clear                                                                                                        **
  --*******************************************************************************************************************

  procedure Clear (The_String : in out String);


  --*******************************************************************************************************************
  --**  Prepend_To                                                                                                   **
  --*******************************************************************************************************************

  procedure Prepend_To (The_String : in out String;
                        The_Source : Character;
                        The_Count  : Natural := 1);

  procedure Prepend_To (The_String : in out String;
                        The_Source : Standard.String);

  procedure Prepend_To (The_String : in out String;
                        The_Source : String);

  --*******************************************************************************************************************
  --**  Append_To                                                                                                    **
  --*******************************************************************************************************************

  procedure Append_To (The_String : in out String;
                       The_Source : Character;
                       The_Count  : Natural := 1);

  procedure Append_To (The_String : in out String;
                       The_Source : Standard.String) renames Append;

  procedure Append_To (The_String : in out String;
                       The_Source : String)          renames Append;


  --*******************************************************************************************************************
  --**  Insert_In                                                                                                    **
  --*******************************************************************************************************************

  procedure Insert_In (The_String  : in out String;
                       At_Position : Positive;
                       The_Source  : Standard.String) renames Insert;

  procedure Insert_In (The_String  : in out String;
                       At_Position : Positive;
                       The_Source  : Character;
                       The_Count   : Natural := 1);

  procedure Insert_In (The_String  : in out String;
                       At_Position : Positive;
                       The_Source  : String);


  --*******************************************************************************************************************
  --**  Replace_In                                                                                                   **
  --*******************************************************************************************************************

  procedure Replace_In (The_String  : in out String;
                        At_Position : Positive;
                        The_Source  : Standard.String) renames Overwrite;

  procedure Replace_In (The_String  : in out String;
                        At_Position : Positive;
                        The_Source  : Character;
                        The_Count   : Natural := 1);

  procedure Replace_In (The_String  : in out String;
                        At_Position : Positive;
                        The_Source  : String);


  --*******************************************************************************************************************
  --**  Replace_Slice_In                                                                                             **
  --*******************************************************************************************************************

  procedure Replace_Slice_In (The_String  : in out String;
                              From        : Positive;
                              To          : Natural;
                              The_Source  : Standard.String) renames Replace_Slice;

  procedure Replace_Slice_In (The_String  : in out String;
                              From        : Positive;
                              To          : Natural;
                              The_Source  : Character;
                              The_Count   : Natural := 1);

  procedure Replace_Slice_In (The_String  : in out String;
                              From        : Positive;
                              To          : Natural;
                              The_Source  : String);


  --*******************************************************************************************************************
  --**  Delete_From                                                                                                  **
  --*******************************************************************************************************************

  procedure Delete_From (The_String    : in out String;
                         From_Position : Positive;
                         To_Position   : Natural) renames Delete;

  procedure Delete_From (The_String    : in out String;
                         At_Position   : Positive);


  --*******************************************************************************************************************
  --**  Comparisions =, <, >                                                                                         **
  --*******************************************************************************************************************

  function Is_Equal (Left              : String;
                     Right             : String;
                     Is_Case_Sensitive : Boolean := False) return Boolean;

  function Is_Equal (Left              : Standard.String;
                     Right             : String;
                     Is_Case_Sensitive : Boolean := False) return Boolean;

  function Is_Equal (Left              : String;
                     Right             : Standard.String;
                     Is_Case_Sensitive : Boolean := False) return Boolean;

  function Is_Equal (Left              : Standard.String;
                     Right             : Standard.String;
                     Is_Case_Sensitive : Boolean := False) return Boolean;


  --function "=" (Left  : String;
  --              Right : String)          return Boolean;
  --function "=" (Left  : String;
  --              Right : Standard.String) return Boolean;
  --function "=" (Left  : Standard.String;
  --              Right : String)          return Boolean;

  function "=" (Left  : String;
                Right : Character) return Boolean;

  --function "=" (Left  : Character;
  --              Right : String) return Boolean;

  --function "<" (Left  : String;
  --              Right : String)          return Boolean;
  --function "<" (Left  : String;
  --              Right : Standard.String) return Boolean;
  --function "<" (Left  : Standard.String;
  --              Right : String)          return Boolean;

  function "<" (Left  : String;
                Right : Character) return Boolean;

  --function "<" (Left  : Character;
  --              Right : String) return Boolean;

  --function ">" (Left  : String;
  --              Right : String)          return Boolean;
  --function ">" (Left  : String;
  --              Right : Standard.String) return Boolean;
  --function ">" (Left  : Standard.String;
  --              Right : String)          return Boolean;

  function ">" (Left  : String;
                Right : Character) return Boolean;

  --function ">" (Left  : Character;
  --              Right : String) return Boolean;
  --  Returns the result of a byte per byte comparison according to the ordinal
  --  values.


  --*******************************************************************************************************************
  --**  Is_Null, Character_Of, Length_Of, Is_Empty, First_Of                                                         **
  --*******************************************************************************************************************

  function Is_Null (The_String : String) return Boolean;

  function Character_Of (The_String  : String;
                         At_Position : Positive) return Character renames Element;

  function Length_Of  (The_String : String) return Natural renames Length;


  --*******************************************************************************************************************
  --**  Location_Of                                                                                                  **
  --*******************************************************************************************************************

  Not_Found : constant Natural := Natural'first;

  function Location_Of (The_Character : Character;
                        In_String     : Standard.String) return Natural with Inline;

  function Location_Of (The_String    : String;
                        In_String     : String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural;

  function Location_Of (The_String    : String;
                        In_String     : Standard.String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural;

  function Location_Of (The_String    : Standard.String;
                        In_String     : String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural;

  function Location_Of (The_String    : Standard.String;
                        In_String     : Standard.String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural;

  function Location_Of (The_Character : Character;
                        In_String     : String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural;

  function Location_Of (The_Character : Character;
                        In_String     : Standard.String;
                        From_Position : Natural;
                        The_Direction : Direction := Forward) return Natural;

  --  Returns the location of the item in the string. It searchs
  --  the item in the range from the specified position to the end of the container.
  --  The value "Not_Found" is returned if the item is not found.


  function Location_After_All (The_Character : Character;
                               In_String     : String;
                               From_Position : Positive := Start_Of_String) return Positive;

  --  Searches In_String for the first character that does not match with
  --  The_Character starting From_Position. The procedure returns the position of
  --  the match or in case of no match the length of In_String plus one.


  --*******************************************************************************************************************
  --**  Search operations                                                                                            **
  --*******************************************************************************************************************

  function Is_Found (The_Character : Character;
                     In_String     : Standard.String) return Boolean with Inline;

  function Is_Found (The_Character : Character;
                     In_String     : String) return Boolean;


  --*******************************************************************************************************************
  --**  Case handling routines                                                                                       **
  --*******************************************************************************************************************

  function Is_Lowercase (The_String : String) return Boolean;
  -- returns true if all the alphabetic characters in The_String are lowercase
  -- Non alphabetic characters are ignored.

  function Is_Uppercase (The_String : String) return Boolean;
  -- returns true if all the alphabetic characters in The_String are uppercase
  -- Non alphabetic characters are ignored.

  procedure Make_Lowercase (The_String  : in out String);

  procedure Make_Lowercase (The_String  : in out Standard.String;
                            At_Position : Positive);

  function Lowercase_Of (The_Character : Character) return Character
           renames Ada.Characters.Handling.To_Lower;

  function Lowercase_Of (The_String : Standard.String) return Standard.String
           renames Ada.Characters.Handling.To_Lower;

  function Lowercase_Of (The_String : Standard.String) return String;

  function Lowercase_Of (The_String : String) return Standard.String;

  function Lowercase_Of (The_String : String) return String;


  procedure Make_Uppercase (The_String  : in out String);

  procedure Make_Uppercase (The_String  : in out Standard.String;
                            At_Position : Positive);

  function Uppercase_Of (The_Character : Character) return Character
           renames Ada.Characters.Handling.To_Upper;

  function Uppercase_Of (The_String : Standard.String) return Standard.String
           renames Ada.Characters.Handling.To_Upper;

  function Uppercase_Of (The_String : Standard.String) return String;

  function Uppercase_Of (The_String : String) return Standard.String;

  function Uppercase_Of (The_String : String) return String;


  --*******************************************************************************************************************
  --**  Trim, Trimmed, Trim_Leading, Trimmed_Leading                                                                 **
  --*******************************************************************************************************************

  procedure Trim (The_String : in out String);

  procedure Trim (The_String    : in out String;
                  The_Character : Character);

  function Trimmed (The_String : String) return String;

  function Trimmed (The_String : Standard.String) return String;

  function Trimmed (The_String : Standard.String) return Standard.String;

  function Trimmed (The_String    : String;
                    The_Character : Character) return String;

  function Trimmed (The_String    : Standard.String;
                    The_Character : Character) return String;

  function Trimmed (The_String    : Standard.String;
                    The_Character : Character) return Standard.String;

  --  Removes leading and trailing characters.


  procedure Trim_Leading (The_String    : in out String;
                          The_Character : Character);

  function Trimmed_Leading (The_String    : String;
                            The_Character : Character) return String;

  function Trimmed_Leading (The_String    : Standard.String;
                            The_Character : Character) return String;

  function Trimmed_Leading (The_String    : Standard.String;
                            The_Character : Character) return Standard.String;

  --  Removes leading characters.


  --*******************************************************************************************************************
  --**  Iterators                                                                                                    **
  --*******************************************************************************************************************

  generic
    with procedure Handle (The_Character : in out Character);
  procedure Iterate_Over_String_With (The_String : in out String);


  --*******************************************************************************************************************
  --**  Conversion                                                                                                   **
  --*******************************************************************************************************************

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

  function Ansi_Of (Ecp_String : Standard.String) return Standard.String;
  --
  -- Converts an Ecp encoded string into the equivalent ANSI
  --

end Text;
