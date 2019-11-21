-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Error;
with Strings;
with Text;

package body Angle is

  function To_Value    is new Ada.Unchecked_Conversion (Signed, Value);
  function To_Signed   is new Ada.Unchecked_Conversion (Value, Signed);

  Per_Degree : constant := 360.0 / (2.0 ** 32);


  function "+" (Item : Degrees) return Value is

    One_Degree : constant := (2.0 ** 32) / 360.0;

    use type Degrees;

    The_Value : Long_Long_Integer := Long_Long_Integer(Item * One_Degree);

    Value_Minimum : constant := 0;
    Value_Maximum : constant := 2**32;

  begin
    while The_Value >= Value_Maximum loop
      The_Value := The_Value - Value_Maximum;
    end loop;
    while The_Value < Value_Minimum loop
      The_Value := The_Value + Value_Maximum;
    end loop;
    return Value(The_Value);
  end "+";


  function "+" (Item : Degrees) return Signed is
  begin
    return To_Signed (Value'(+Item));
  end "+";


  function "+" (Item : Degrees) return Unsigned is
  begin
    return Unsigned(Value'(+Item));
  end "+";


  function "+" (Item : Value) return Degrees is
    use type Degrees;
  begin
    return Degrees(Item) * Per_Degree;
  end "+";


  function "+" (Item : Hours) return Value is
    use type Degrees;
  begin
    return +Degrees'(Degrees(Item) * 15.0);
  end "+";


  function "+" (Item : Value) return Hours is
    Per_Hour : constant := 24.0 / (2.0 ** 32);
    use type Hours;
  begin
    return Hours(Item) * Per_Hour;
  end "+";


  function "+" (Item : Signed) return Value is
  begin
    return To_Value (Item);
  end "+";


  function "+" (Item : Signed) return Degrees is
    use type Degrees;
  begin
    return Degrees(Item) * Per_Degree;
  end "+";


  function "+" (Item : Value) return Signed is
  begin
    return To_Signed (Item);
  end "+";


  function "-" (Item : Value) return Signed is
  begin
    return -To_Signed (Item);
  end "-";


  function "+" (Item : Unsigned) return Value is
  begin
    return Value(Item);
  end "+";


  function "+" (Item : Unsigned) return Degrees is
    use type Degrees;
  begin
    return Degrees(Item) * Per_Degree;
  end "+";


  function "+" (Item : Value) return Unsigned is
  begin
    return Unsigned(Item);
  end "+";


  function "+" (Left, Right : Value) return Value is
  begin
    return Value(Unsigned(Left) + Unsigned(Right));
  end "+";


  function "+" (Left : Value; Right : Degrees) return Value is
  begin
    return Left + Value'(+Right);
  end "+";


  function "+" (Left : Value; Right : Signed) return Value is
  begin
    return Left + To_Value(Right);
  end "+";


  function "+" (Left : Unsigned; Right : Signed) return Unsigned is
  begin
    return Left + Unsigned(To_Value(Right));
  end "+";


  function "-" (Left, Right : Value) return Value is
  begin
    return Value(Unsigned'(Unsigned(Left) - Unsigned(Right)));
  end "-";


  function "-" (Left, Right : Value) return Signed is
    use type Degrees;
  begin
    return To_Signed(Left - Right);
  end "-";


  function "-" (Left, Right : Value) return Degrees is
    use type Degrees;
  begin
    return Degrees(To_Signed(Left - Right)) * Per_Degree;
  end "-";


  function "-" (Left : Value; Right : Signed) return Value is
  begin
    return Value(Unsigned'(Unsigned(Left) - Unsigned(To_Value(Right))));
  end "-";


  function "-" (Left : Value; Right : Signed) return Unsigned is
  begin
    return Unsigned(Left) - Unsigned(To_Value(Right));
  end "-";


  function "-" (Left, Right : Unsigned) return Signed is
  begin
    return To_Signed(Value(Unsigned'(Left - Right)));
  end "-";


  function "-" (Left : Value; Right : Degrees) return Value  is
  begin
    return Left - Value'(+Right);
  end "-";


  function "<" (Left, Right : Value) return Boolean  is
  begin
    return Unsigned(Left) < Unsigned(Right);
  end "<";


  type Place is (First, Minute, Second);

  type Unit_Images is array (Place) of Character;

  type Unit_List is array (In_Unit) of Unit_Images;

  Unit_Table : constant Unit_List := ("°'""", "hms");

  One_Degree  : constant := 3600.0;
  Full_Circle : constant := 360.0 * One_Degree;

  type Seconds is delta 10.0**(-4) range -Full_Circle .. Full_Circle;


  function Image_Of (The_Value   : Value;
                     Unit        : In_Unit := In_Degrees;
                     Decimals    : Decimal_Places := 1;
                     Show_Signed : Boolean := False) return String is

    use type Degrees;

    Factor : constant Seconds := Seconds(Degrees'(10.0 ** Natural(Decimals)));

    subtype Decimals_3 is Seconds delta 10.0**(-3);
    subtype Decimals_2 is Seconds delta 10.0**(-2);
    subtype Decimals_1 is Seconds delta 10.0**(-1);

    Actual_Units : constant Unit_Images := Unit_Table(Unit);

    Half_Circle : constant := Full_Circle / 2;

    The_Seconds  : Seconds := Seconds(Degrees'(+The_Value) * One_Degree);
    The_Text     : Text.String;
    The_Number   : Natural;
    The_Minutes  : Natural;
    The_Fraction : Seconds;

  begin
    if Show_Signed then
      if The_Seconds > Half_Circle then
        The_Seconds := Full_Circle - The_Seconds;
        Text.Append_To (The_Text, '-');
      else
        Text.Append_To (The_Text, '+');
      end if;
    end if;
    if Unit = In_Hours then
      The_Seconds := The_Seconds / 15.0;
    end if;
    The_Number := Natural(The_Seconds * Factor);
    The_Fraction := Seconds(The_Number mod Natural(Factor)) / Factor;
    The_Number := The_Number / Natural(Factor);
    The_Seconds := Seconds(The_Number mod 60) + The_Fraction;
    The_Number := The_Number / 60;
    The_Minutes := The_Number mod 60;
    The_Number := The_Number / 60;
    Text.Append_To (The_Text, Text.Trimmed(The_Number'img) & Actual_Units(First));
    if The_Minutes < 10 then
      Text.Append_To (The_Text, '0');
    end if;
    Text.Append_To (The_Text, Text.Trimmed(The_Minutes'img) & Actual_Units(Minute));
    if The_Seconds < 10.0 then
      Text.Append_To (The_Text, '0');
    end if;
    case Decimals is
    when 0 =>
      Text.Append_To (The_Text, String'(Text.Trimmed(Natural(The_Seconds)'img)));
    when 1 =>
      Text.Append_To (The_Text, String'(Text.Trimmed(Decimals_1'image(The_Seconds))));
    when 2 =>
      Text.Append_To (The_Text, String'(Text.Trimmed(Decimals_2'image(The_Seconds))));
    when 3 =>
      Text.Append_To (The_Text, String'(Text.Trimmed(Decimals_3'image(The_Seconds))));
    end case;
    Text.Append_To (The_Text, Actual_Units(Second));
    return Strings.Utf8_Of (Text.String_Of (The_Text));
  end Image_Of;


  function Value_Of (The_Image  : String;
                     With_Units : Units := Default_Degrees) return Value is

    Image : constant String := Strings.Ansi_Of (The_Image);

    type State is (At_Begin, At_Minutes, At_Seconds, At_End);

    The_State   : State     := At_Begin;
    The_Seconds : Seconds   := 0.0;
    The_Unit    : Units     := With_Units;
    The_Next    : Positive  := Image'first;
    Is_Negative : Boolean   := False;
    The_First   : Positive;
    The_Number  : Natural;

    function Next_Character return Character is
    begin
      if The_Next > Image'last then
        The_Next := The_Next + 1;
        return ' ';
      else
        The_Next := The_Next + 1;
        return Image(The_Next - 1);
      end if;
    end Next_Character;

    procedure Undo_Next_Character is
    begin
      if The_Next > Image'first then
        The_Next := The_Next - 1;
      end if;
    end Undo_Next_Character;

    procedure Skip_Spaces is
    begin
      loop
        if The_Next > Image'last then
          The_State := At_End;
          exit;
        elsif Image(The_Next) /= ' ' then
          exit;
        end if;
        The_Next := The_Next + 1;
      end loop;
    end Skip_Spaces;

    procedure Get_Next_Number is
    begin
      if The_State = At_End then
        Error.Raise_With ("Number expected");
      end if;
      if not (Image(The_Next) in '0' .. '9') then
        Error.Raise_With (Image(The_Next) & " is not a number");
      end if;
      The_First := The_Next;
      The_Next := The_Next + 1;
      loop
        if The_Next > Image'last then
          The_State := At_End;
          exit;
        else
          exit when not (Image(The_Next) in '0' .. '9');
        end if;
        The_Next := The_Next + 1;
      end loop;
      The_Number := Natural'value (Image(The_First .. The_Next - 1));
    end Get_Next_Number;

    procedure Handle_Fraction is
      The_Last : Natural;
    begin
      The_Seconds := The_Seconds + Seconds(The_Number);
      Get_Next_Number;
      The_Last := The_Next - 1;
      Skip_Spaces;
      if The_Last = Image'last then
        The_Seconds := The_Seconds + Seconds'value ("0." & Image(The_First .. The_Last));
      else
        case Next_Character is
        when '"' =>
          if The_Unit = In_Hours then
            Error.Raise_With ("s expected");
          end if;
          The_Seconds := The_Seconds + Seconds'value ("0." & Image(The_First .. The_Last));
        when 's' =>
          if The_Unit = In_Degrees then
            Error.Raise_With (""" expected");
          end if;
          The_Seconds := The_Seconds + Seconds'value ("0." & Image(The_First .. The_Last));
        when others =>
          Undo_Next_Character;
          The_Seconds := The_Seconds + Seconds'value ("0." & Image(The_First .. The_Last));
        end case;
      end if;
    end Handle_Fraction;

    A_Minute : constant := 60;
    A_Degree : constant := A_Minute * 60;
    Per_Unit : constant := 1.0 / 3600.0;

  begin -- Value_Of
    Skip_Spaces;
    case Next_Character is
    when '-' =>
      Is_Negative := True;
      Skip_Spaces;
    when '+' =>
      Skip_Spaces;
    when others =>
      Undo_Next_Character;
    end case;
    Get_Next_Number;
    case Next_Character is
    when '°' | 'd' =>
      if The_Unit = In_Hours then
        Error.Raise_With ("h, m or s expected");
      end if;
      The_Seconds := Seconds(The_Number * A_Degree);
      The_Unit := In_Degrees;
      The_State := At_Minutes;
    when 'h' =>
      if The_Unit = In_Degrees then
        Error.Raise_With ("°, ' or "" expected");
      end if;
      The_Seconds :=  Seconds(The_Number * A_Degree);
      The_Unit := In_Hours;
      The_State := At_Minutes;
    when ' ' =>
      The_Seconds := Seconds(The_Number * A_Degree);
      The_State := At_Minutes;
    when ''' =>
      if The_Unit = In_Hours then
        Error.Raise_With ("h, m or s expected");
      end if;
      The_Seconds := Seconds(The_Number * A_Minute);
      The_Unit := In_Degrees;
      The_State := At_Seconds;
    when 'm' =>
      if The_Unit = In_Hours then
        Error.Raise_With ("°, ' or "" expected");
      end if;
      The_Seconds := Seconds(The_Number * A_Minute);
      The_Unit := In_Hours;
      The_State := At_Seconds;
    when '.' =>
      Handle_Fraction;
    when '"' =>
      if The_Unit = In_Hours then
        Error.Raise_With ("h, m or s expected");
      end if;
      The_Seconds := Seconds(The_Number);
      The_Unit := In_Degrees;
      The_State := At_End;
    when 's' =>
      if The_Unit = In_Hours then
        Error.Raise_With ("°, ' or "" expected");
      end if;
      The_Seconds := Seconds(The_Number);
      The_Unit := In_Hours;
      The_State := At_End;
    when others =>
      Error.Raise_With ("unexpected: " & Image(The_Next - 1));
    end case;
    Skip_Spaces;
    if The_State = At_Minutes then
      Get_Next_Number;
      case Next_Character is
      when ' ' =>
        The_Seconds := The_Seconds + Seconds(The_Number * A_Minute);
        The_State := At_Seconds;
      when ''' =>
        if The_Unit = In_Hours then
          Error.Raise_With ("m or s expected");
        end if;
        The_Seconds := The_Seconds + Seconds(The_Number * A_Minute);
        The_Unit := In_Degrees;
        The_State := At_Seconds;
      when 'm' =>
        if The_Unit = In_Degrees then
          Error.Raise_With ("' or "" expected");
        end if;
        The_Seconds := The_Seconds + Seconds(The_Number * A_Minute);
        The_Unit := In_Hours;
        The_State := At_Seconds;
      when '.' =>
        Handle_Fraction;
      when '"' =>
        if The_Unit = In_Hours then
          Error.Raise_With ("m or s expected");
        end if;
        The_Seconds := The_Seconds + Seconds(The_Number);
        The_Unit := In_Degrees;
        The_State := At_End;
      when 's' =>
        if The_Unit = In_Degrees then
          Error.Raise_With ("' or "" expected");
        end if;
        The_Seconds := The_Seconds + Seconds(The_Number);
        The_Unit := In_Hours;
        The_State := At_End;
      when others =>
        Error.Raise_With ("unexpected: " & Image(The_Next));
      end case;
    end if;
    Skip_Spaces;
    if The_State = At_Seconds then
      Get_Next_Number;
      case Next_Character is
      when ' ' =>
        The_Seconds := The_Seconds + Seconds(The_Number);
      when '.' =>
        Handle_Fraction;
      when '"' =>
        if The_Unit = In_Hours then
          Error.Raise_With ("s expected");
        end if;
        The_Seconds := The_Seconds + Seconds(The_Number);
        The_Unit := In_Degrees;
      when 's' =>
        if The_Unit = In_Degrees then
          Error.Raise_With (""" expected");
        end if;
        The_Seconds := The_Seconds + Seconds(The_Number);
        The_Unit := In_Hours;
      when others =>
        Error.Raise_With ("unexpected: " & Image(The_Next));
      end case;
    end if;
    Skip_Spaces;
    if The_State /= At_End then
      Error.Raise_With ("unexpected: " & Image(The_Next..Image'last));
    end if;
    if Is_Negative then
      The_Seconds := -The_Seconds;
    end if;
    declare
      use type Degrees;
      use type Hours;
      The_Value : Value;
    begin
      case The_Unit is
      when Default_Hours | In_Hours =>
        The_Value := +(Hours(The_Seconds) * Hours(Per_Unit));
      when Default_Degrees | In_Degrees =>
        The_Value := +(Degrees(The_Seconds) * Degrees(Per_Unit));
      end case;
      return The_Value;
    end;
  exception
  when Error.Occurred =>
    raise;
  when others =>
    Error.Raise_With ("illegal value: " & Image);
  end Value_Of;


  function Hours_Image_Of (Item : Value) return String is
    type Hours_Range is delta 0.000001 range 0.0 .. 24.0;
  begin
    return Hours_Range(Hours'(+Item))'img;
  end Hours_Image_Of;


  function Signed_Degrees_Image_Of (Item : Value) return String is
    type Signed_Degrees is delta 0.000001 range -90.0 .. 90.0;
    use type Degrees;
  begin
    return Signed_Degrees(Degrees'(+To_Signed (Item)))'img;
  end Signed_Degrees_Image_Of;


  function Unsigned_Degrees_Image_Of (Item : Value) return String is
    type Unsigned_Degrees is delta 0.000001 range 0.0 .. 360.0;
  begin
    return Unsigned_Degrees(Degrees'(+Item))'img;
  end Unsigned_Degrees_Image_Of;


  function "+" (Left  : String;
                Right : Unsigned) return String is
  begin
    return Left & Unsigned_Degrees_Image_Of (Value(Right));
  end "+";


  function "+" (Left  : String;
                Right : Signed) return String is
  begin
    return Left & Signed_Degrees_Image_Of (+Right);
  end "+";


  function Interpolation_Of (A, A1, A2 : Unsigned;
                                V1, V2 : Signed) return Signed is
    DA : constant Long_Long_Integer := Long_Long_Integer(Unsigned'(A2 - A1));
    DV : constant Long_Long_Integer := Long_Long_Integer(Signed'(V2 - V1));
  begin
    if DA = 0 then
      return V1;
    else
      return V1 + Signed(Long_Long_Integer(Unsigned'(A - A1)) * DV / DA);
    end if;
  end Interpolation_Of;

end Angle;
