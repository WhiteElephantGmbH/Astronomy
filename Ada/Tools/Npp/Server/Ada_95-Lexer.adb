-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Ada_95.Characters;
with Ada_95.Comment;
with Ada_95.Error;
with Ada_95.Lexical;
with Ada_95.Name;
with Ada_95.Number;
with Ada_95.Symbol;
with Ada_95.Texts;
with Log;
with Memory;

package body Ada_95.Lexer is

  Base_Delimiter    : constant Character := '#';
  Part_Separator    : constant Character := '_';
  String_Delimiter  : constant Character := '"';
  Ampersand         : constant Character := '&';
  Apostrophe        : constant Character := ''';
  At_Sign           : constant Character := '@';
  Left_Bracket      : constant Character := '[';
  Right_Bracket     : constant Character := ']';
  Left_Parenthesis  : constant Character := '(';
  Right_Parenthesis : constant Character := ')';
  Plus              : constant Character := '+';
  Minus             : constant Character := '-';
  Asterisk          : constant Character := '*';
  Slash             : constant Character := '/';
  Less              : constant Character := '<';
  Equal             : constant Character := '=';
  Greater           : constant Character := '>';
  Comma             : constant Character := ',';
  Period            : constant Character := '.';
  Colon             : constant Character := ':';
  Semicolon         : constant Character := ';';
  Vertical_Line     : constant Character := '|';
  Special_Id        : constant Character := '~';

  package Ascii renames Ada.Characters.Latin_1;

  type Bits is mod 2**8;

  function Convert is new Ada.Unchecked_Conversion (Character, Bits);


  subtype Control_Range_1 is Character range Ascii.NUL .. Ascii.US;
  subtype Control_Range_2 is Character range Ascii.DEL .. Ascii.APC;

  subtype Lowercase_Letter_Range is Character range 'a' .. 'z';
  subtype Uppercase_Letter_Range is Character range 'A' .. 'Z';
  subtype Special_Letter_Range_1 is Character range Ascii.UC_A_Grave .. Ascii.UC_O_Diaeresis;
  subtype Special_Letter_Range_2 is Character range Ascii.UC_O_Oblique_Stroke .. Ascii.LC_O_Diaeresis;
  subtype Special_Letter_Range_3 is Character range Ascii.LC_O_Oblique_Stroke .. Ascii.LC_Y_Diaeresis;

  subtype Number_Range is Character range '0' .. '9';

  type Token_Start_Kind is (Name_Start,
                            Number_Start,
                            Separator,
                            String_Start,
                            Symbol_Ampersand,
                            Symbol_Apostrophe,
                            Symbol_Left_Parenthesis,
                            Symbol_Right_Parenthesis,
                            Symbol_Left_Bracket,
                            Symbol_Right_Bracket,
                            Symbol_Plus,
                            Symbol_Minus,
                            Symbol_Asterisk,
                            Symbol_Slash,
                            Symbol_Less,
                            Symbol_Equal,
                            Symbol_Greater,
                            Symbol_Comma,
                            Symbol_Period,
                            Symbol_Colon,
                            Symbol_Semicolon,
                            Symbol_Target_Name,
                            Symbol_Vertical_Line,
                            Symbol_Underscore,
                            Symbol_Number_Sign,
                            Symbol_Special_Id,
                            Unknown);

  type Token_Start_Map is array (Character) of Token_Start_Kind;

  Token_Start : constant Token_Start_Map := [Lowercase_Letter_Range => Name_Start,
                                             Uppercase_Letter_Range => Name_Start,
                                             Special_Letter_Range_1 => Name_Start,
                                             Special_Letter_Range_2 => Name_Start,
                                             Special_Letter_Range_3 => Name_Start,
                                             Number_Range           => Number_Start,
                                             Control_Range_1        => Separator,
                                             Control_Range_2        => Separator,
                                             Ascii.Space            => Separator,
                                             String_Delimiter       => String_Start,
                                             Base_Delimiter         => Symbol_Number_Sign,
                                             Part_Separator         => Symbol_Underscore,
                                             Ampersand              => Symbol_Ampersand,
                                             Apostrophe             => Symbol_Apostrophe,
                                             Left_Bracket           => Symbol_Left_Bracket,
                                             Right_Bracket          => Symbol_Right_Bracket,
                                             Left_Parenthesis       => Symbol_Left_Parenthesis,
                                             Right_Parenthesis      => Symbol_Right_Parenthesis,
                                             Plus                   => Symbol_Plus,
                                             Minus                  => Symbol_Minus,
                                             Asterisk               => Symbol_Asterisk,
                                             Slash                  => Symbol_Slash,
                                             Less                   => Symbol_Less,
                                             Equal                  => Symbol_Equal,
                                             Greater                => Symbol_Greater,
                                             Comma                  => Symbol_Comma,
                                             Period                 => Symbol_Period,
                                             Colon                  => Symbol_Colon,
                                             Semicolon              => Symbol_Semicolon,
                                             At_Sign                => Symbol_Target_Name,
                                             Vertical_Line          => Symbol_Vertical_Line,
                                             Special_Id             => Symbol_Special_Id,
                                             others                 => Unknown];


  type Name_Continuation_Kind is (Alpha_Numeric, Name_Separator, Separator);

  type Name_Continuation_Map is array (Character) of Name_Continuation_Kind;

  Name_Continuation : constant Name_Continuation_Map := [Lowercase_Letter_Range => Alpha_Numeric,
                                                         Uppercase_Letter_Range => Alpha_Numeric,
                                                         Special_Letter_Range_1 => Alpha_Numeric,
                                                         Special_Letter_Range_2 => Alpha_Numeric,
                                                         Special_Letter_Range_3 => Alpha_Numeric,
                                                         Number_Range           => Alpha_Numeric,
                                                         Part_Separator         => Name_Separator,
                                                         others                 => Separator];

  type Number_Continuation_Kind is
    (Base_Separator,
     Radix_Point,
     Hex_Extension_Or_Exponent,
     Hex_Extension,
     Minus_Sign,
     Digit,
     Number_Separator,
     Plus_Sign,
     Separator);

  type Number_Continuation_Map is array (Character) of Number_Continuation_Kind;

  Number_Continuation : constant Number_Continuation_Map := [Base_Delimiter => Base_Separator,
                                                             Period         => Radix_Point,
                                                             'a'| 'A'       => Hex_Extension,
                                                             'b'| 'B'       => Hex_Extension,
                                                             'c'| 'C'       => Hex_Extension,
                                                             'd'| 'D'       => Hex_Extension,
                                                             'e'| 'E'       => Hex_Extension_Or_Exponent,
                                                             'f'| 'F'       => Hex_Extension,
                                                             Minus          => Minus_Sign,
                                                             Plus           => Plus_Sign,
                                                             Number_Range   => Digit,
                                                             Part_Separator => Number_Separator,
                                                             others         => Separator];

  procedure Start is
  begin
    Lexical.Define;
  end Start;


  function Tokens_For (File : in out Source.Object'class) return Token.Sequence is

    procedure Process (Line : Token.Source_Line) is

      use type Token.Column_Position;

      The_Index :          Token.Column_Position := Line'first;
      Last      : constant Token.Column_Position := Line'last;

      The_Utf8_Offset : Token.Column_Position := 0;

      Error_Detected : exception;

      procedure Add_Error (Message        : Error.Kind;
                           Start_Position : Token.Column_Range;
                           End_Position   : Token.Column_Range := Token.Column_Range'first) is
      begin
        if Start_Position > Line'last then
          Error.Add (Message, " ", Start_Position);
        elsif End_Position < Start_Position then
          Error.Add (Message, String(Line(Start_Position .. Start_Position)), Start_Position);
        else
          Error.Add (Message, String(Line(Start_Position .. End_Position)), Start_Position);
        end if;
        raise Error_Detected;
      end Add_Error;

      procedure Handle_Name is
        The_Position : Token.Column_Position := The_Index;
      begin
        while The_Index < Last loop
          case Name_Continuation (Line(The_Index + 1)) is
          when Alpha_Numeric =>
            The_Index := The_Index + 1;
          when Name_Separator =>
            Name.Add (String(Line(The_Position .. The_Index)));
            if The_Position > The_Index then
              Add_Error (Error.Identifier_With_Double_Underscore, The_Index + 1);
            end if;
            The_Index := The_Index + 1;
            The_Position := The_Index + 1;
          when Separator =>
            exit;
          end case;
        end loop;
        Name.Add (String(Line(The_Position .. The_Index)));
        if The_Position > The_Index then
          Add_Error (Error.Identifier_Ends_With_Underscore, The_Position - 1);
        end if;
        Name.Complete;
        The_Index := The_Index + 1;
      exception
      when Error_Detected =>
        while The_Index < Last loop
          The_Index := The_Index + 1;
          exit when Name_Continuation (Line(The_Index)) = Separator;
        end loop;
        Name.Clear;
      end Handle_Name;

      procedure Handle_Number is
        type Number_State is
          (At_Number_Begin,
           In_Number,
           At_Number_Separator,
           At_Based_Number_Begin,
           In_Based_Number,
           At_Based_Number_Separator,
           At_Based_Number_End,
           At_Exponent_Begin,
           In_Exponent_Number);
        The_State             : Number_State := At_Number_Begin;
        The_Continuation_Kind : Number_Continuation_Kind;
        Is_Real_Number        : Boolean := False;
      begin
        loop
          if The_Index <= Last then
            The_Continuation_Kind := Number_Continuation (Line(The_Index));
          else
            The_Continuation_Kind := Separator;
          end if;
          --Log.Write ("### " & Number_State'image (The_State) &
          --           " with " & Number_Continuation_Kind'image (The_Continuation_Kind));
          case The_Continuation_Kind is
          when Base_Separator =>
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Base_Delimiter_Not_Expected, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Number_Ends_With_Underscore, The_Index - 1);
              if Is_Real_Number then
                Add_Error (Error.Real_Base_Not_Allowed, The_Index);
              end if;
              The_State := At_Based_Number_Begin;
            when In_Number =>
              if Is_Real_Number then
                Add_Error (Error.Real_Base_Not_Allowed, The_Index);
              end if;
              The_State := At_Based_Number_Begin;
            when At_Based_Number_Begin =>
              Add_Error (Error.Extended_Digit_Expected, The_Index);
              The_State := At_Based_Number_End;
            when At_Based_Number_Separator =>
              Add_Error (Error.Based_Number_Ends_With_Underscore, The_Index - 1);
              The_State := At_Based_Number_End;
            when In_Based_Number =>
              The_State := At_Based_Number_End;
            when At_Based_Number_End =>
              exit;
            when At_Exponent_Begin =>
              Add_Error (Error.Exponent_Value_Expected, The_Index);
            when In_Exponent_Number =>
              Add_Error (Error.Exponent_Digit_Expected, The_Index);
            end case;
          when Radix_Point =>
            if The_Index < Last and then Line(The_Index + 1) = Period then
              exit; -- range symbol
            end if;
            if Is_Real_Number then
              Add_Error (Error.Duplicate_Radix_Point, The_Index);
            end if;
            Is_Real_Number := True;
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Number_Starts_With_Radix_Point, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Number_Ends_With_Underscore, The_Index - 1);
            when In_Number =>
              The_State := At_Number_Begin;
            when At_Based_Number_Begin =>
              Add_Error (Error.Based_Number_Starts_With_Radix_Point, The_Index);
            when At_Based_Number_Separator =>
              Add_Error (Error.Based_Number_Ends_With_Underscore, The_Index - 1);
            when In_Based_Number =>
              The_State := At_Based_Number_Begin;
            when At_Based_Number_End =>
              exit;
            when At_Exponent_Begin =>
              Add_Error (Error.Exponent_Value_Expected, The_Index);
            when In_Exponent_Number =>
              Add_Error (Error.Exponent_Digit_Expected, The_Index);
            end case;
          when Hex_Extension_Or_Exponent =>
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Exponent_Digit_Not_Expected, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Number_Ends_With_Underscore, The_Index - 1);
            when In_Number =>
              The_State := At_Exponent_Begin;
            when At_Based_Number_Begin =>
              The_State := In_Based_Number;
            when At_Based_Number_Separator =>
              The_State := In_Based_Number;
            when In_Based_Number =>
              null;
            when At_Based_Number_End =>
              The_State := At_Exponent_Begin;
            when At_Exponent_Begin =>
              Add_Error (Error.Exponent_Value_Expected, The_Index);
            when In_Exponent_Number =>
              Add_Error (Error.Exponent_Digit_Expected, The_Index);
            end case;
          when Hex_Extension =>
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Extended_Digit_Not_Expected, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Digit_Expected, The_Index);
            when In_Number =>
              Add_Error (Error.Digit_Expected, The_Index);
            when At_Based_Number_Begin =>
              The_State := In_Based_Number;
            when At_Based_Number_Separator =>
              The_State := In_Based_Number;
            when In_Based_Number =>
              null;
            when At_Based_Number_End =>
              Add_Error (Error.Extended_Digit_Not_Expected, The_Index);
            when At_Exponent_Begin =>
              Add_Error (Error.Exponent_Value_Expected, The_Index);
            when In_Exponent_Number =>
              Add_Error (Error.Exponent_Digit_Expected, The_Index);
            end case;
          when Digit =>
            case The_State is
            when At_Number_Begin =>
              The_State := In_Number;
            when At_Number_Separator =>
              The_State := In_Number;
            when In_Number =>
              null;
            when At_Based_Number_Begin =>
              The_State := In_Based_Number;
            when At_Based_Number_Separator =>
              The_State := In_Based_Number;
            when In_Based_Number =>
              null;
            when At_Based_Number_End =>
              Add_Error (Error.Digit_Not_Expected, The_Index);
            when At_Exponent_Begin =>
              The_State := In_Exponent_Number;
            when In_Exponent_Number =>
              null;
            end case;
          when Number_Separator =>
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Digit_Expected, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Number_With_Double_Underscore, The_Index);
            when In_Number =>
              The_State := At_Number_Separator;
            when At_Based_Number_Begin =>
              Add_Error (Error.Based_Number_Starts_With_Underscore, The_Index);
            when At_Based_Number_Separator =>
              Add_Error (Error.Based_Number_With_Double_Underscore, The_Index);
            when In_Based_Number =>
              The_State := At_Based_Number_Separator;
            when At_Based_Number_End =>
              Add_Error (Error.Underscore_Not_Expected, The_Index);
            when At_Exponent_Begin =>
              Add_Error (Error.Exponent_Value_Expected, The_Index);
            when In_Exponent_Number =>
              Add_Error (Error.Exponent_Digit_Expected, The_Index);
            end case;
          when Plus_Sign | Minus_Sign =>
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Digit_Expected, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Number_Ends_With_Underscore, The_Index - 1);
            when In_Number =>
              exit;
            when At_Based_Number_Begin =>
              Add_Error (Error.Extended_Digit_Expected, The_Index);
            when At_Based_Number_Separator =>
              Add_Error (Error.Based_Number_Not_Terminated, The_Index);
            when In_Based_Number =>
              Add_Error (Error.Based_Number_Not_Terminated, The_Index);
            when At_Based_Number_End =>
              exit;
            when At_Exponent_Begin =>
              if not Is_Real_Number and then
                Number_Continuation (Line(The_Index)) = Minus_Sign
              then
                Add_Error (Error.Negative_Exponent_Not_Allowed, The_Index);
              end if;
              The_State := In_Exponent_Number;
            when In_Exponent_Number =>
              exit;
            end case;
          when Separator =>
            case The_State is
            when At_Number_Begin =>
              Add_Error (Error.Digit_Expected, The_Index);
            when At_Number_Separator =>
              Add_Error (Error.Number_Ends_With_Underscore, The_Index);
            when In_Number =>
              exit;
            when At_Based_Number_Begin =>
              Add_Error (Error.Extended_Digit_Expected, The_Index);
            when At_Based_Number_Separator =>
              Add_Error (Error.Based_Number_Not_Terminated, The_Index);
            when In_Based_Number =>
              Add_Error (Error.Based_Number_Not_Terminated, The_Index);
            when At_Based_Number_End =>
              exit;
            when At_Exponent_Begin =>
              Add_Error (Error.Exponent_Value_Expected, The_Index);
            when In_Exponent_Number =>
              exit;
            end case;
          end case;
          The_Index := The_Index + 1;
        end loop;
        Number.Add (String(Line(Token.Position .. The_Index - 1)), Is_Real_Number);
      exception
      when Error_Detected =>
        while The_Index < Last loop
          The_Index := The_Index + 1;
          exit when Number_Continuation (Line(The_Index)) = Separator;
        end loop;
      end Handle_Number;

      procedure Handle_String is

        Start_Position : constant Token.Column_Position := The_Index;
        Start_Column   : constant Token.Column_Position := Start_Position - The_Utf8_Offset;

        procedure Append_Literal is
        begin
          Token.Append_Literal (Item         => Texts.New_String (String(Line(Token.Position + 1 .. The_Index - 1))),
                                First_Column => Start_Column,
                                Last_Column  => The_Index - The_Utf8_Offset);
        end Append_Literal;

      begin -- Handle_String
        loop
          if The_Index = Last then
            Add_Error (Error.String_Not_Terminated, Start_Position);
          else
            The_Index := The_Index + 1;
            case Line(The_Index) is
            when String_Delimiter =>
              if The_Index < Last then
                if Line(The_Index + 1) = String_Delimiter then
                  The_Index := The_Index + 1;
                else
                  Append_Literal;
                  exit;
                end if;
              else
                Append_Literal;
                exit;
              end if;
            when Character'val(16#C0#) .. Character'val(16#FF#) => -- UTF8 ?
              declare
                The_Bits : constant Bits := Convert (Line(The_Index));
              begin
                if (The_Bits and 16#E0#) = 16#C0# then
                  The_Utf8_Offset := The_Utf8_Offset + 1;
                elsif (The_Bits and 16#F0#) = 16#E0# then
                  The_Utf8_Offset := The_Utf8_Offset + 2;
                elsif (The_Bits and 16#F8#) = 16#F0# then
                  The_Utf8_Offset := The_Utf8_Offset + 3;
                end if;
              end;
            when others =>
              null;
            end case;
          end if;
        end loop;
        The_Index := The_Index + 1;
      end Handle_String;

      procedure Handle_Apostrophe with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index < Last and then Line(The_Index + 1) = Apostrophe then
          if Line(The_Index) = Left_Parenthesis and then
            The_Index + 2 < Last and then
            Line(The_Index + 3) = Apostrophe
          then
            Symbol.Add (Lexical.Apostrophe); -- start of qualified expression like Character'('x')
          else
            Characters.Add (Line(The_Index));
            The_Index := The_Index + 2;
          end if;
        else
          Symbol.Add (Lexical.Apostrophe);
        end if;
      end Handle_Apostrophe;

      procedure Handle_Symbol_Asterisk with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index <= Last and then Line(The_Index) = Asterisk then
          The_Index := The_Index + 1;
          Symbol.Add (Lexical.Exponentiation);
        else
          Symbol.Add (Lexical.Asterisk);
        end if;
      end Handle_Symbol_Asterisk;

      procedure Handle_Symbol_Slash with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index <= Last and then Line(The_Index) = Equal then
          The_Index := The_Index + 1;
          Symbol.Add (Lexical.Not_Equal);
        else
          Symbol.Add (Lexical.Slash);
        end if;
      end Handle_Symbol_Slash;

      procedure Handle_Symbol_Less with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index > Last then
          Symbol.Add (Lexical.Less);
        else
          case Line(The_Index) is
          when Equal =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Less_Or_Equal);
          when Greater =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Unconstrained);
          when Less =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Start_Label);
          when others =>
            Symbol.Add (Lexical.Less);
          end case;
        end if;
      end Handle_Symbol_Less;

      procedure Handle_Symbol_Equal with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index <= Last and then Line(The_Index) = Greater then
          The_Index := The_Index + 1;
          Symbol.Add (Lexical.Association);
        else
          Symbol.Add (Lexical.Equal);
        end if;
      end Handle_Symbol_Equal;

      procedure Handle_Symbol_Greater with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index > Last then
          Symbol.Add (Lexical.Greater);
        else
          case Line(The_Index) is
          when Equal =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Greater_Or_Equal);
          when Greater =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.End_Label);
          when others =>
            Symbol.Add (Lexical.Greater);
          end case;
        end if;
      end Handle_Symbol_Greater;

      procedure Handle_Symbol_Period with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index <= Last and then Line(The_Index) = Period then
          The_Index := The_Index + 1;
          Symbol.Add (Lexical.Range_Delimiter);
        else
          Symbol.Add (Lexical.Period);
        end if;
      end Handle_Symbol_Period;

      procedure Handle_Symbol_Colon with Inline is
      begin
        The_Index := The_Index + 1;
        if The_Index <= Last and then Line(The_Index) = Equal then
          The_Index := The_Index + 1;
          Symbol.Add (Lexical.Assignment);
        else
          Symbol.Add (Lexical.Colon);
        end if;
      end Handle_Symbol_Colon;

      procedure Handle_Symbol_Underscore with Inline is
      begin
        if The_Index < Last then
          case Token_Start (Line(The_Index + 1)) is
          when Name_Start =>
            Name.Add ("");
            Add_Error (Error.Identifier_Starts_With_Underscore, The_Index);
          when Number_Start =>
            Handle_Number;
          when others =>
            Add_Error (Error.Underscore_Not_Expected, The_Index);
          end case;
        else
          Add_Error (Error.Underscore_Not_Expected, The_Index);
        end if;
      end Handle_Symbol_Underscore;

    begin -- Process
      loop
        begin
          Token.Set_Position (The_Index);
          if The_Index > Last then
            Token.New_Line;
            exit;
          end if;
          case Token_Start (Line(The_Index)) is
          when Symbol_Minus =>
            if The_Index < Last and then Line(The_Index + 1) = Minus then
              Comment.Add (String(Line(The_Index + 2 .. Last)));
              exit;
            else
              The_Index := The_Index + 1;
              Symbol.Add (Lexical.Minus);
            end if;
          when Name_Start =>
            Handle_Name;
          when Number_Start =>
            Handle_Number;
          when String_Start =>
            Handle_String;
          when Symbol_Apostrophe =>
            Handle_Apostrophe;
          when Symbol_Ampersand =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Ampersand);
          when Symbol_Left_Bracket =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Left_Bracket);
          when Symbol_Right_Bracket =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Right_Bracket);
          when Symbol_Left_Parenthesis =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Left_Parenthesis);
          when Symbol_Right_Parenthesis =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Right_Parenthesis);
          when Symbol_Plus =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Plus);
          when Symbol_Asterisk =>
            Handle_Symbol_Asterisk;
          when Symbol_Slash =>
            Handle_Symbol_Slash;
          when Symbol_Less =>
            Handle_Symbol_Less;
          when Symbol_Equal =>
            Handle_Symbol_Equal;
          when Symbol_Greater =>
            Handle_Symbol_Greater;
          when Symbol_Comma =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Comma);
          when Symbol_Period =>
            Handle_Symbol_Period;
          when Symbol_Colon =>
            Handle_Symbol_Colon;
          when Symbol_Semicolon =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Semicolon);
          when Symbol_Target_Name =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Target_Name);
          when Symbol_Vertical_Line =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Vertical_Line);
          when Symbol_Underscore =>
            Handle_Symbol_Underscore;
          when Symbol_Number_Sign =>
            Add_Error (Error.Base_Missing, The_Index);
          when Symbol_Special_Id =>
            The_Index := The_Index + 1;
            Symbol.Add (Lexical.Special_Id);
          when Unknown =>
            Add_Error (Error.Unknown_Character, The_Index);
          when Separator =>
            The_Index := The_Index + 1;
          end case;
        exception
        when Error_Detected =>
          The_Index := The_Index + 1;
        end;
      end loop;
    exception
    when Error_Detected =>
      Token.New_Line;
    end Process;


    procedure Complete_Process is
    begin
      Token.End_List;
      Source.Close (File);
    exception
    when others =>
      null;
    end Complete_Process;

  begin -- Tokens_For
    Token.Create_List (File.Id);
    Source.Open (File);
    while not Source.End_Of (File) loop
      Process (Source.Next_Line (From => File));
    end loop;
    Complete_Process;
    return Token.Actual_List;
  exception
  when Occurrence: others =>
    Log.Write ("!!! Lexer.Tokens_For", Occurrence);
    Complete_Process;
    return Token.Actual_List;
  end Tokens_For;


  procedure Finalize is
  begin
    Name.Finalize;
    Memory.Clear;
  end Finalize;


end Ada_95.Lexer;
