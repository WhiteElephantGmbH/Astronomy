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

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Unsigned;

package body Text is

  Last_Ascii : constant Character := Character'val (127);
  No_Map     : constant Unsigned.Byte := 16#20#;

  subtype Upper_Character is Character range Character'val (16#80#)..Character'last;
  subtype G1_Character is Character range Character'val (16#A0#).. Character'last;

  type G1_Conversion_Table is array (G1_Character) of Unsigned.Byte;
  type Upper_Conversion_Table is array (Upper_Character) of Unsigned.Byte;

  Ecp437_To_Latin1_Map : constant Upper_Conversion_Table
                       := (16#C7#, 16#FC#, 16#E9#, 16#E2#, 16#E4#, 16#E0#, 16#E5#, 16#E7#,
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
                           16#B0#, No_Map, 16#B7#, No_Map, No_Map, 16#B2#, No_Map, 16#A0#);

  Latin1_To_Ecp437_Map : constant G1_Conversion_Table
                       := (16#FF#, 16#AD#, 16#9B#, 16#9C#, No_Map, 16#9D#, No_Map, No_Map,
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
                           No_Map, 16#97#, 16#A3#, 16#96#, 16#81#, No_Map, No_Map, 16#98#);


  --*******************************************************************************************************************
  --**  String_Of  & Strings_Of                                                                                      **
  --*******************************************************************************************************************

  function String_Of (The_Character : Character) return String is
  begin
    return To_Unbounded_String ("" & The_Character);
  end String_Of;


  function String_Of (The_String    : String;
                      From_Position : Positive;
                      The_Size      : Natural) return String is
  begin
    return String_Of (Slice(The_String, From_Position, From_Position + The_Size - 1));
  end String_Of;


  function String_Of (The_String    : String;
                      From_Position : Positive;
                      The_Size      : Natural) return Standard.String is
  begin
    return Slice(The_String, From_Position, From_Position + The_Size - 1);
  end String_Of;


  --*******************************************************************************************************************
  --**  Truncate & Truncation_Of                                                                                     **
  --*******************************************************************************************************************

  procedure Truncate (The_String     : in out String;
                      The_New_Length : Natural) is
  begin
    Delete (The_String, The_New_Length + 1, Length_Of (The_String));
  end Truncate;


  function Truncation_Of (The_String     : String;
                          The_New_Length : Natural) return String is
  begin
    return Delete (The_String, The_New_Length + 1, Length_Of (The_String));
  end Truncation_Of;


  --*******************************************************************************************************************
  --**  Clear                                                                                                        **
  --*******************************************************************************************************************

  procedure Clear (The_String : in out String) is
  begin
    The_String := To_Unbounded_String ("");
  end Clear;


  --*******************************************************************************************************************
  --**  Prepend_To                                                                                                   **
  --*******************************************************************************************************************

  procedure Prepend_To (The_String : in out String;
                        The_Source : Character;
                        The_Count  : Natural := 1) is
  begin
    The_String := (The_Count * The_Source) & The_String;
  end Prepend_To;


  procedure Prepend_To (The_String : in out String;
                        The_Source : Standard.String) is
  begin
    The_String := The_Source & The_String;
  end Prepend_To;


  procedure Prepend_To (The_String : in out String;
                        The_Source : String) is
  begin
    The_String := The_Source & The_String;
  end Prepend_To;


  --*******************************************************************************************************************
  --**  Append_To                                                                                                    **
  --*******************************************************************************************************************

  procedure Append_To (The_String : in out String;
                       The_Source : Character;
                       The_Count  : Natural := 1) is
  begin
    The_String := The_String & (The_Count * The_Source);
  end Append_To;


  --*******************************************************************************************************************
  --**  Insert_In                                                                                                    **
  --*******************************************************************************************************************

  procedure Insert_In (The_String  : in out String;
                       At_Position : Positive;
                       The_Source  : Character;
                       The_Count   : Natural := 1) is
  begin
    Insert (The_String, At_Position, String_Of (String'(The_Count * The_Source)));
  end Insert_In;


  procedure Insert_In (The_String  : in out String;
                       At_Position : Positive;
                       The_Source  : String) is
  begin
    Insert (The_String, At_Position, String_Of (The_Source));
  end Insert_In;


  --*******************************************************************************************************************
  --**  Replace_In                                                                                                   **
  --*******************************************************************************************************************

  procedure Replace_In (The_String  : in out String;
                        At_Position : Positive;
                        The_Source  : Character;
                        The_Count   : Natural := 1) is
  begin
    Overwrite (The_String, At_Position, String_Of (String'(The_Count * The_Source)));
  end Replace_In;


  procedure Replace_In (The_String  : in out String;
                        At_Position : Positive;
                        The_Source  : String) is
  begin
    Overwrite (The_String, At_Position, To_String(The_Source));
  end Replace_In;


  --*******************************************************************************************************************
  --**  Replace_Slice_In                                                                                             **
  --*******************************************************************************************************************

  procedure Replace_Slice_In (The_String  : in out String;
                              From        : Positive;
                              To          : Natural;
                              The_Source  : Character;
                              The_Count   : Natural := 1) is
  begin
    Replace_Slice (The_String, From, To, String_Of (String'(The_Count * The_Source)));
  end Replace_Slice_In;


  procedure Replace_Slice_In (The_String  : in out String;
                              From        : Positive;
                              To          : Natural;
                              The_Source  : String) is
  begin
    Replace_Slice (The_String, From, To, To_String(The_Source));
  end Replace_Slice_In;


  --*******************************************************************************************************************
  --**  Delete_From                                                                                                  **
  --*******************************************************************************************************************

  procedure Delete_From (The_String  : in out String;
                         At_Position : Positive) is
  begin
    Delete (The_String, At_Position, At_Position);
  end Delete_From;


  --*******************************************************************************************************************
  --**  Comparisions =, <, >                                                                                         **
  --*******************************************************************************************************************

  function Is_Equal (Left              : String;
                     Right             : String;
                     Is_Case_Sensitive : Boolean := False) return Boolean is
  begin
    return Is_Equal (String_Of (Left), String_Of (Right), Is_Case_Sensitive);
  end Is_Equal;


  function Is_Equal (Left              : Standard.String;
                     Right             : String;
                     Is_Case_Sensitive : Boolean := False) return Boolean is
  begin
    return Is_Equal (Left, String_Of (Right), Is_Case_Sensitive);
  end Is_Equal;


  function Is_Equal (Left              : String;
                     Right             : Standard.String;
                     Is_Case_Sensitive : Boolean := False) return Boolean is
  begin
    return Is_Equal (String_Of (Left), Right, Is_Case_Sensitive);
  end Is_Equal;


  function Is_Equal (Left              : Standard.String;
                     Right             : Standard.String;
                     Is_Case_Sensitive : Boolean := False) return Boolean is
  begin
    if Is_Case_Sensitive then
      return Left = Right;
    else
      return Standard.String'(Lowercase_Of (Left)) = Standard.String'(Lowercase_Of (Right));
    end if;
  end Is_Equal;


  function "=" (Left  : String;
                Right : Character) return Boolean is
  begin
    return "=" (Left, "" & Right);
  end "=";


  function "<" (Left  : String;
                Right : Character) return Boolean is
  begin
    return "<" (Left, "" & Right);
  end "<";


  function ">" (Left  : String;
                Right : Character) return Boolean is
  begin
    return ">" (Left, "" & Right);
  end ">";


  --*******************************************************************************************************************
  --**  Is_Null, Character_Of, Length_Of, Is_Empty, First_Of                                                         **
  --*******************************************************************************************************************

  function Is_Null (The_String : String) return Boolean is
  begin
    return Length (The_String) = 0;
  end Is_Null;


  --*******************************************************************************************************************
  --**  Location_Of                                                                                                  **
  --*******************************************************************************************************************

  function Location_Of (The_Character : Character;
                        In_String     : Standard.String) return Natural is
  begin
    for Index in In_String'range loop
      if The_Character = In_String(Index) then
        return Index;
      end if;
    end loop;
    return Not_Found;
  end Location_Of;


  function Location_Of (The_String    : String;
                        In_String     : String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Location_Of (Standard.String'(String_Of(The_String)),
                        In_String,
                        From_Position,
                        The_Direction);
  end Location_Of;


  function Location_Of (The_String    : Standard.String;
                        In_String     : String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural is
  begin
    if From_Position = Start_Of_Search then
      return Index (In_String, The_String, Ada.Strings.Direction(The_Direction));
    else
      declare
        First_Position : Natural;
        Last_Position  : Natural;
        The_Index      : Natural;
      begin
        if The_Direction = Forward then
          First_Position := From_Position;
          Last_Position := Length_Of (In_String);
        else
          First_Position := Start_Of_String;
          Last_Position := From_Position;
        end if;
        The_Index := Index (String_Of (Slice(In_String, First_Position, Last_Position)),
                            The_String,
                            Ada.Strings.Direction(The_Direction));
        if The_Index = Not_Found then
          return Not_Found;
        else
          return First_Position + The_Index - Start_Of_String;
        end if;
      end;
    end if;
  end Location_Of;


  function Location_Of (The_String    : String;
                        In_String     : Standard.String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Location_Of (Standard.String'(String_Of(The_String)),
                        String'(String_Of(In_String)),
                        From_Position,
                        The_Direction);
  end Location_Of;


  function Location_Of (The_String    : Standard.String;
                        In_String     : Standard.String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Location_Of (The_String,
                        String'(String_Of(In_String)),
                        From_Position,
                        The_Direction);
  end Location_Of;


  function Location_Of (The_Character : Character;
                        In_String     : String;
                        From_Position : Natural := Start_Of_Search;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Location_Of ("" & The_Character,
                        In_String,
                        From_Position,
                        The_Direction);
  end Location_Of;


  function Location_Of (The_Character : Character;
                        In_String     : Standard.String;
                        From_Position : Natural;
                        The_Direction : Direction := Forward) return Natural is
  begin
    return Location_Of ("" & The_Character,
                        In_String,
                        From_Position,
                        The_Direction);
  end Location_Of;


  function Location_After_All (The_Character : Character;
                               In_String     : String;
                               From_Position : Positive := Start_Of_String) return Positive is

    The_Index : Natural;

  begin
    The_Index := Index (String_Of (Slice(In_String, From_Position, Length_Of (In_String))),
                        Ada.Strings.Maps.To_Set (The_Character),
                        Ada.Strings.Outside);
    if The_Index = Not_Found then
      return From_Position;
    else
      return From_Position + The_Index - Start_Of_String;
    end if;
  end Location_After_All;


  --*******************************************************************************************************************
  --**  Search operations                                                                                            **
  --*******************************************************************************************************************

  function Is_Found (The_Character : Character;
                     In_String     : Standard.String) return Boolean is
  begin
    return Location_Of (The_Character, In_String) /= Not_Found;
  end Is_Found;


  function Is_Found (The_Character : Character;
                     In_String     : String) return Boolean is
  begin
    return Location_Of (The_Character, In_String) /= Not_Found;
  end Is_Found;


  --*******************************************************************************************************************
  --**  Case handling routines                                                                                       **
  --*******************************************************************************************************************

  function Is_Lowercase (The_String : String) return Boolean is
    The_Characters : constant Standard.String := String_Of (The_String);
  begin
    for The_Character of The_Characters loop
      if Ada.Characters.Handling.Is_Upper (The_Character) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Lowercase;


  function Is_Uppercase (The_String : String) return Boolean is
    The_Characters : constant Standard.String := String_Of (The_String);
  begin
    for The_Character of The_Characters loop
      if Ada.Characters.Handling.Is_Lower (The_Character) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Uppercase;


  procedure Make_Lowercase (The_String : in out String) is
  begin
    The_String := String_Of (Ada.Characters.Handling.To_Lower (String_Of (The_String)));
  end Make_Lowercase;


  procedure Make_Lowercase (The_String  : in out Standard.String;
                            At_Position : Positive) is
  begin
    The_String(At_Position) := Ada.Characters.Handling.To_Lower(The_String(At_Position));
  end Make_Lowercase;


  function Lowercase_Of (The_String : Standard.String) return String is
  begin
    return String_Of (Ada.Characters.Handling.To_Lower (The_String));
  end Lowercase_Of;


  function Lowercase_Of (The_String : String) return Standard.String is
  begin
    return Ada.Characters.Handling.To_Lower (String_Of (The_String));
  end Lowercase_Of;


  function Lowercase_Of (The_String : String) return String is
  begin
    return String_Of (Ada.Characters.Handling.To_Lower (String_Of (The_String)));
  end Lowercase_Of;


  procedure Make_Uppercase (The_String : in out String) is
  begin
    The_String := String_Of (Ada.Characters.Handling.To_Upper (String_Of (The_String)));
  end Make_Uppercase;


  procedure Make_Uppercase (The_String  : in out Standard.String;
                            At_Position : Positive) is
  begin
    The_String(At_Position) := Ada.Characters.Handling.To_Upper(The_String(At_Position));
  end Make_Uppercase;


  function Uppercase_Of (The_String : Standard.String) return String is
  begin
    return String_Of (Ada.Characters.Handling.To_Upper (The_String));
  end Uppercase_Of;


  function Uppercase_Of (The_String : String) return Standard.String is
  begin
    return Ada.Characters.Handling.To_Upper (String_Of (The_String));
  end Uppercase_Of;


  function Uppercase_Of (The_String : String) return String is
  begin
    return String_Of (Ada.Characters.Handling.To_Upper (String_Of (The_String)));
  end Uppercase_Of;


  --*******************************************************************************************************************
  --**  Trim, Trimmed, Trim_Leading, Trimmed_Leading                                                                 **
  --*******************************************************************************************************************

  procedure Trim (The_String : in out String) is
  begin
    Trim (The_String, Ada.Strings.Both);
  end Trim;


  procedure Trim (The_String    : in out String;
                  The_Character : Character) is

    Characters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (The_Character);

  begin
    Trim (The_String, Left => Characters, Right => Characters);
  end Trim;


  function Trimmed (The_String : String) return String is
    Text_String : String := The_String;
  begin
    Trim (Text_String);
    return Text_String;
  end Trimmed;


  function Trimmed (The_String : Standard.String) return String is
  begin
    return Trimmed (String_Of (The_String));
  end Trimmed;


  function Trimmed (The_String : Standard.String) return Standard.String is
  begin
    return String_Of (Trimmed (The_String));
  end Trimmed;


  function Trimmed (The_String    : String;
                    The_Character : Character) return String is
    Text_String : String := The_String;
  begin
    Trim (Text_String, The_Character);
    return Text_String;
  end Trimmed;


  function Trimmed (The_String    : Standard.String;
                    The_Character : Character) return String is
  begin
    return Trimmed (String_Of (The_String), The_Character);
  end Trimmed;


  function Trimmed (The_String    : Standard.String;
                    The_Character : Character) return Standard.String is
  begin
    return String_Of (Trimmed (The_String, The_Character));
  end Trimmed;


  procedure Trim_Leading (The_String    : in out String;
                          The_Character : Character) is

    Characters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (The_Character);

  begin
    Trim (The_String, Left => Characters, Right => Ada.Strings.Maps.Null_Set);
  end Trim_Leading;


  function Trimmed_Leading (The_String    : String;
                            The_Character : Character) return String is
    Text_String : String := The_String;
  begin
    Trim_Leading (Text_String, The_Character);
    return Text_String;
  end Trimmed_Leading;


  function Trimmed_Leading (The_String    : Standard.String;
                            The_Character : Character) return String is
  begin
    return Trimmed_Leading (String_Of (The_String), The_Character);
  end Trimmed_Leading;


  function Trimmed_Leading (The_String    : Standard.String;
                            The_Character : Character) return Standard.String is
  begin
    return String_Of (Trimmed_Leading (The_String, The_Character));
  end Trimmed_Leading;


  --*******************************************************************************************************************
  --**  Iterators                                                                                                    **
  --*******************************************************************************************************************

  procedure Iterate_Over_String_With (The_String : in out String) is
    The_Characters : Standard.String := String_Of(The_String);
  begin
    for The_Index in The_Characters'range loop
      Handle (The_Characters(The_Index));
    end loop;
    The_String := String_Of (The_Characters);
  end Iterate_Over_String_With;


  --*******************************************************************************************************************
  --**  Conversion                                                                                                   **
  --*******************************************************************************************************************

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


  function Ecp_Of (Ansi_String : Standard.String) return Standard.String is
  begin
    return Ada.Strings.Fixed.Translate (Ansi_String, Latin1_To_Ecp437'access);
  end Ecp_Of;


  function Ansi_Of (Ecp_String : Standard.String) return Standard.String is
  begin
    return Ada.Strings.Fixed.Translate (Ecp_String, Ecp437_To_Latin1'access);
  end Ansi_Of;


end Text;
