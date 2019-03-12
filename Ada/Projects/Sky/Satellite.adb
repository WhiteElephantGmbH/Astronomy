-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Directories;
with Ada.Text_IO;
with File;
with Norad;
with Parameter;
with String_List;
with Strings;
with Text;
with Traces;

package body Satellite is

  package Log is new Traces ("Satellite");

  use type File.Folder;

  procedure Read_Stellarium_Data is

    End_Of_File         : exception;
    Incorrect_Hex_Digit : exception;
    Incorrect_String    : exception;
    Incorrect_Token     : exception;
    Syntax_Error        : exception;

    The_File   : Ada.Text_IO.File_Type;
    The_Line   : String (1..256);
    The_Length : Natural := 0;
    The_Index  : Natural := 0;

    The_Character : Character;

    function Next_Character return Character is
    begin
      if The_Index = The_Length then
        The_Length := 0;
      end if;
      while The_Length = 0 loop
        The_Index := 0;
        if Ada.Text_IO.End_Of_File (The_File) then
          raise End_Of_File;
        end if;
        Ada.Text_IO.Get_Line (The_File, The_Line, The_Length);
      end loop;
      The_Index := The_Index + 1;
      The_Character := The_Line(The_Index);
      return The_Character;
    end Next_Character;


    function Next_Is_Digit return Boolean is
    begin
      if The_Index < The_Length then
        The_Character := The_Line(The_Index + 1);
        if The_Character in '0'..'9' then
          The_Index := The_Index + 1;
          return True;
        end if;
      else
        The_Character := Ascii.Nul;
      end if;
      return False;
    end Next_Is_Digit;


    procedure Skip_Separators is
    begin
      while Next_Character <= ' ' loop
        null;
      end loop;
    end Skip_Separators;


    function Lower_Character return Character is
    begin
      return Strings.Lowercase_Of (Next_Character);
    end Lower_Character;


    procedure Check_Lower (Characters : String) is
    begin
      for Character of Characters loop
        if Lower_Character /= Character then
          raise Incorrect_Token;
        end if;
      end loop;
    end Check_Lower;


    procedure Get_Hex_Digit is
    begin
      case Lower_Character is
      when '0'..'9' | 'a'..'f' =>
        null;
      when others =>
        raise Incorrect_Hex_Digit;
      end case;
    end Get_Hex_Digit;


    type Token is (Is_String,
                   Number,
                   Start_Object,
                   Start_Array,
                   Is_True,
                   Is_False,
                   Is_Null,
                   Pair_Separator,
                   Separator,
                   End_Object,
                   End_Array);

    subtype Is_Value is Token range Is_String .. Is_Null;

    The_Token_Start : Natural;
    The_Token_End   : Natural;
    Nesting_Level   : Natural := 0;


    function Next_Token return Token is
    begin
      Skip_Separators;
      The_Token_Start := The_Index;
      The_Token_End := The_Index;
      case The_Character is
      when '{' =>
        return Start_Object;
      when '}' =>
        return End_Object;
      when '[' =>
        return Start_Array;
      when ']' =>
        return End_Array;
      when ',' =>
        return Separator;
      when ':' =>
        return Pair_Separator;
      when '"' =>
        The_Token_Start := The_Token_Start + 1;
        while Next_Character /= '"' loop
          if The_Character = '\' then
            case Next_Character is
            when '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' =>
              null;
            when 'u' =>
              Get_Hex_Digit;
              Get_Hex_Digit;
              Get_Hex_Digit;
              Get_Hex_Digit;
            when others =>
              raise Incorrect_String;
            end case;
          end if;
        end loop;
        The_Token_End := The_Index - 1;
        return Is_String;
      when '-' | '0'..'9' =>
        while Next_Is_Digit loop
          null;
        end loop;
        if The_Character = '-' then
          The_Index := The_Index + 1;
        end if;
        if The_Character = '.' then
          The_Index := The_Index + 1;
          while Next_Is_Digit loop
            null;
          end loop;
        end if;
        if The_Character in 'e'| 'E' then
          The_Index := The_Index + 1;
          if Next_Character in '+' | '-' | '0' .. '9' then
            while Next_Is_Digit loop
              null;
            end loop;
          end if;
        end if;
        The_Token_End := The_Index;
        return Number;
      when 't' | 'T' =>
        Check_Lower ("rue");
        The_Token_End := The_Index;
        return Is_True;
      when 'f' | 'F' =>
        Check_Lower ("alse");
        The_Token_End := The_Index;
        return Is_False;
      when 'n' | 'N' =>
        Check_Lower ("ull");
        The_Token_End := The_Index;
        return Is_Null;
      when others =>
        raise Incorrect_Token;
      end case;
    end Next_Token;


    function Token_Image return String is
    begin
      return The_Line(The_Token_Start..The_Token_End);
    end Token_Image;


    procedure Get_Token (The_Token : Token) is
    begin
      if Next_Token /= The_Token then
        raise Syntax_Error;
      end if;
    end Get_Token;


    Handle_String : access procedure := null;
    Handle_True   : access procedure := null;

    Is_Satellites : Boolean := False;
    The_Name      : Text.String;
    The_Data      : Norad.Two_Line;
    The_Names     : String_List.Item;
    Neo_File      : Ada.Text_IO.File_Type;


    function Neo_Filename_Of (Name : String) return String is
      The_Filename : String := Name;
    begin
      for Index in Name'range loop
        case The_Filename(Index) is
        when '\' | '/' | '|' | ':' | '*' | '?' | '<' | '>' =>
          The_Filename(Index) := '-';
        when '(' =>
          return Strings.Trimmed (The_Filename(The_Filename'first .. Index - 1));
        when others =>
          null;
        end case;
      end loop;
      return Strings.Trimmed (The_Filename);
    end Neo_Filename_Of;


    procedure Add_Name is
    begin
      The_Name := Text.String_Of (Neo_Filename_Of (Token_Image));
    end Add_Name;


    procedure Add_Tle1 is
    begin
      The_Data(1) := Token_Image;
    end Add_Tle1;


    procedure Add_Tle2 is
    begin
      The_Data(2) := Token_Image;
    end Add_Tle2;


    procedure Add_Satellite is
      Filename  : constant String := Text.String_Of (The_Name);
      Full_Name : constant String := File.Composure (Directory, Filename, Extension);
      use type String_List.Item;
    begin
      if not The_Names.Contains (Filename) and then not Norad.Is_In_Deep_Space (The_Data) then
        The_Names := The_Names + Filename;
        Ada.Text_IO.Create (Neo_File, Name => Full_Name);
        Ada.Text_IO.Put_Line (Neo_File, The_Data(1));
        Ada.Text_IO.Put_Line (Neo_File, The_Data(2));
        Ada.Text_IO.Close (Neo_File);
        Log.Write ("added: " & Filename);
      else
        File.Delete (Full_Name);
      end if;
    exception
    when others =>
      null;
    end Add_Satellite;


    procedure Get_Value;


    procedure Get_Object is
    begin
      Nesting_Level := Nesting_Level + 1;
      --Log.Write ("Get_Object - level:" & Nesting_Level'img);
      loop
        case Next_Token is
        when End_Object =>
          return;
        when Is_String =>
          loop
            declare
              Name : constant String := Token_Image;
            begin
              --Log.Write ("Name: " & Name);
              case Nesting_Level is
              when 1 =>
                Is_Satellites := Token_Image = "satellites";
              when 3 =>
                if Is_Satellites then
                  if Name = "name" then
                    Handle_String := Add_Name'access;
                  elsif Name = "tle1" then
                    Handle_String := Add_Tle1'access;
                  elsif Name = "tle2" then
                    Handle_String := Add_Tle2'access;
                  elsif Name = "visible" then
                    Handle_True := Add_Satellite'access;
                  end if;
                end if;
              when others =>
                null;
              end case;
            end;
            Get_Token (Pair_Separator);
            Get_Value;
            case Next_Token is
            when End_Object =>
              Nesting_Level := Nesting_Level - 1;
              return;
            when Separator =>
              Get_Token (Is_String);
            when others =>
              raise Syntax_Error;
            end case;
          end loop;
        when others =>
          raise Syntax_Error;
        end case;
      end loop;
    end Get_Object;


    function Get_Value return Token is
      The_Token : constant Token := Next_Token;
    begin
      case The_Token is
      when Is_String =>
        --Log.Write ("String: " & Token_Image);
        if Handle_String /= null then
          Handle_String.all;
          Handle_String := null;
        end if;
      when Number =>
        --Log.Write ("Number: " & Token_Image);
        null;
      when Start_Object =>
        Get_Object;
      when Start_Array =>
        --Log.Write ("Array");
        case Get_Value is
        when End_Array =>
          null;
        when Is_Value =>
          loop
            case Next_Token is
            when End_Array =>
              exit;
            when Separator =>
              Get_Value;
            when others =>
              raise Syntax_Error;
            end case;
          end loop;
        when others =>
          raise Syntax_Error;
        end case;
      when Is_True =>
        --Log.Write ("True");
        if Handle_True /= null then
          Handle_True.all;
          Handle_True := null;
        end if;
      when Is_False =>
        --Log.Write ("False");
        Handle_True := null;
      when Is_Null =>
        --Log.Write ("Null");
        null;
      when others =>
        null;
      end case;
      return The_Token;
    end Get_Value;


    procedure Get_Value is
    begin
      if not (Get_Value in Is_Value) then
        raise Syntax_Error;
      end if;
    end Get_Value;

    Json_Filename : constant String := Parameter.Satellites_Filename;

  begin -- Read_Stellarium_Data
    File.Delete_Directory (Directory);
    if Json_Filename = "" then
      return;
    end if;
    Ada.Directories.Create_Path (Directory);
    Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Json_Filename);
    Get_Token (Start_Object);
    Get_Object;
    Ada.Text_IO.Close (The_File);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Read_Stellarium_Data;

end Satellite;
