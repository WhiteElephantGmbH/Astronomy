-- *********************************************************************************************************************
-- *                            (c) 2013 .. 2026 by Soudronic AG, Bergdietikon, Switzerland                            *
-- *                      Developed by White Elephant GmbH, Switzerland (www.white-elephant.ch)                        *
-- *********************************************************************************************************************
-- *                                    package used as reference (never compiled)                                     *
-- *********************************************************************************************************************
package Standard is

  type Boolean is (False, True);

  function "=" (Left, Right : ~Any_Type) return Boolean;

  function "<"  (Left, Right : Boolean) return Boolean;
  function "<=" (Left, Right : Boolean) return Boolean;
  function ">=" (Left, Right : Boolean) return Boolean;
  function ">"  (Left, Right : Boolean) return Boolean;

  type Integer is range -(2 ** 31) .. +(2 ** 31 - 1);

  function "<"  (Left, Right : Integer) return Boolean;
  function "<=" (Left, Right : Integer) return Boolean;
  function ">=" (Left, Right : Integer) return Boolean;
  function ">"  (Left, Right : Integer) return Boolean;

  subtype Natural  is Integer range 0 .. Integer'last;
  subtype Positive is Integer range 1 .. Integer'last;

  type Short_Short_Integer is range -(2 **  7) .. +(2 **  7 - 1);

  function "<"  (Left, Right : Short_Short_Integer) return Boolean;
  function "<=" (Left, Right : Short_Short_Integer) return Boolean;
  function ">=" (Left, Right : Short_Short_Integer) return Boolean;
  function ">"  (Left, Right : Short_Short_Integer) return Boolean;

  type Short_Integer is range -(2 ** 15) .. +(2 ** 15 - 1);

  function "<"  (Left, Right : Short_Integer) return Boolean;
  function "<=" (Left, Right : Short_Integer) return Boolean;
  function ">=" (Left, Right : Short_Integer) return Boolean;
  function ">"  (Left, Right : Short_Integer) return Boolean;

  type Long_Integer is range -(2 ** 31) .. +(2 ** 31 - 1);

  function "<"  (Left, Right : Long_Integer) return Boolean;
  function "<=" (Left, Right : Long_Integer) return Boolean;
  function ">=" (Left, Right : Long_Integer) return Boolean;
  function ">"  (Left, Right : Long_Integer) return Boolean;

  type Long_Long_Integer is range -(2 ** 63) .. +(2 ** 63 - 1);

  function "<"  (Left, Right : Long_Long_Integer) return Boolean;
  function "<=" (Left, Right : Long_Long_Integer) return Boolean;
  function ">=" (Left, Right : Long_Long_Integer) return Boolean;
  function ">"  (Left, Right : Long_Long_Integer) return Boolean;

  type Long_Long_Long_Integer is range -(2 ** 127) .. +(2 ** 127 - 1);

  function "<"  (Left, Right : Long_Long_Long_Integer) return Boolean;
  function "<=" (Left, Right : Long_Long_Long_Integer) return Boolean;
  function ">=" (Left, Right : Long_Long_Long_Integer) return Boolean;
  function ">"  (Left, Right : Long_Long_Long_Integer) return Boolean;

  type Short_Float is digits 6 range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
  -- range -3.40282E+38 .. 3.40282E+38, size 32 bit

  function "<"  (Left, Right : Short_Float) return Boolean;
  function "<=" (Left, Right : Short_Float) return Boolean;
  function ">=" (Left, Right : Short_Float) return Boolean;
  function ">"  (Left, Right : Short_Float) return Boolean;

  type Float is digits 6 range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
  -- range -3.40282E+38 .. 3.40282E+38, size 32 bit

  function "<"  (Left, Right : Float) return Boolean;
  function "<=" (Left, Right : Float) return Boolean;
  function ">=" (Left, Right : Float) return Boolean;
  function ">"  (Left, Right : Float) return Boolean;

  type Long_Float is digits 15 range -16#0.FFFF_FFFF_FFFF_F8#E+256 .. 16#0.FFFF_FFFF_FFFF_F8#E+256;
  -- range -1.79769313486232E+308 .. 1.79769313486232E+308, size 64 bit

  function "<"  (Left, Right : Long_Float) return Boolean;
  function "<=" (Left, Right : Long_Float) return Boolean;
  function ">=" (Left, Right : Long_Float) return Boolean;
  function ">"  (Left, Right : Long_Float) return Boolean;

  type Long_Long_Float is digits 18 range -16#0.FFFF_FFFF_FFFF_FFFF#E+4096 .. 16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
  -- range -1.18973149535723177E+4932 .. 1.18973149535723177E+4932, size 80 bit

  function "<"  (Left, Right : Long_Long_Float) return Boolean;
  function "<=" (Left, Right : Long_Long_Float) return Boolean;
  function ">=" (Left, Right : Long_Long_Float) return Boolean;
  function ">"  (Left, Right : Long_Long_Float) return Boolean;

  type Character is --  Note: this type cannot be represented accurately in Ada
    (' ', '!', '"', '#', '$', '%', '&', ''', '(', ')', '*', '+', ',', '-', '.', '/',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
     '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
     'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\', ']', '^', '_',
     '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
     'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~',

     ' ', '¡', '¢', '£', '¤', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
     '°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
     'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
     'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
     'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
     'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

  function "<"  (Left, Right : Character) return Boolean;
  function "<=" (Left, Right : Character) return Boolean;
  function ">=" (Left, Right : Character) return Boolean;
  function ">"  (Left, Right : Character) return Boolean;

  type Wide_Character is --  Note: this type cannot be represented accurately in Ada
    (' ', '!', '"', '#', '$', '%', '&', ''', '(', ')', '*', '+', ',', '-', '.', '/',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
     '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
     'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\', ']', '^', '_',
     '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
     'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~',

     ' ', '¡', '¢', '£', '¤', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
     '°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
     'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
     'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
     'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
     'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

  function "<"  (Left, Right : Wide_Character) return Boolean;
  function "<=" (Left, Right : Wide_Character) return Boolean;
  function ">=" (Left, Right : Wide_Character) return Boolean;
  function ">"  (Left, Right : Wide_Character) return Boolean;

  type Wide_Wide_Character is --  Note: this type cannot be represented accurately in Ada
    (' ', '!', '"', '#', '$', '%', '&', ''', '(', ')', '*', '+', ',', '-', '.', '/',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
     '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
     'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\', ']', '^', '_',
     '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
     'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~',

     ' ', '¡', '¢', '£', '¤', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
     '°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
     'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
     'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
     'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
     'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

  function "<"  (Left, Right : Wide_Wide_Character) return Boolean;
  function "<=" (Left, Right : Wide_Wide_Character) return Boolean;
  function ">=" (Left, Right : Wide_Wide_Character) return Boolean;
  function ">"  (Left, Right : Wide_Wide_Character) return Boolean;


  package Ascii is

     --  Control characters:

     Nul   : constant Character := Character'val (16#00#);
     Soh   : constant Character := Character'val (16#01#);
     Stx   : constant Character := Character'val (16#02#);
     Etx   : constant Character := Character'val (16#03#);
     Eot   : constant Character := Character'val (16#04#);
     Enq   : constant Character := Character'val (16#05#);
     Ack   : constant Character := Character'val (16#06#);
     Bel   : constant Character := Character'val (16#07#);
     Bs    : constant Character := Character'val (16#08#);
     Ht    : constant Character := Character'val (16#09#);
     Lf    : constant Character := Character'val (16#0A#);
     Vt    : constant Character := Character'val (16#0B#);
     Ff    : constant Character := Character'val (16#0C#);
     Cr    : constant Character := Character'val (16#0D#);
     So    : constant Character := Character'val (16#0E#);
     Si    : constant Character := Character'val (16#0F#);
     Dle   : constant Character := Character'val (16#10#);
     Dc1   : constant Character := Character'val (16#11#);
     Dc2   : constant Character := Character'val (16#12#);
     Dc3   : constant Character := Character'val (16#13#);
     Dc4   : constant Character := Character'val (16#14#);
     Nak   : constant Character := Character'val (16#15#);
     Syn   : constant Character := Character'val (16#16#);
     Etb   : constant Character := Character'val (16#17#);
     Can   : constant Character := Character'val (16#18#);
     Em    : constant Character := Character'val (16#19#);
     Sub   : constant Character := Character'val (16#1A#);
     Esc   : constant Character := Character'val (16#1B#);
     Fs    : constant Character := Character'val (16#1C#);
     Gs    : constant Character := Character'val (16#1D#);
     Rs    : constant Character := Character'val (16#1E#);
     Us    : constant Character := Character'val (16#1F#);
     Del   : constant Character := Character'val (16#7F#);

     -- Other characters:

     Exclam     : constant Character := '!';
     Quotation  : constant Character := '"';
     Sharp      : constant Character := '#';
     Dollar     : constant Character := '$';
     Percent    : constant Character := '%';
     Ampersand  : constant Character := '&';
     Colon      : constant Character := ':';
     Semicolon  : constant Character := ';';
     Query      : constant Character := '?';
     At_Sign    : constant Character := '@';
     L_Bracket  : constant Character := '[';
     Back_Slash : constant Character := '\';
     R_Bracket  : constant Character := ']';
     Circumflex : constant Character := '^';
     Underline  : constant Character := '_';
     Grave      : constant Character := '`';
     L_Brace    : constant Character := '{';
     Bar        : constant Character := '|';
     R_Brace    : constant Character := '}';
     Tilde      : constant Character := '~';

     -- Lower case letters:

     Lc_A : constant Character := 'a';
     Lc_B : constant Character := 'b';
     Lc_C : constant Character := 'c';
     Lc_D : constant Character := 'd';
     Lc_E : constant Character := 'e';
     Lc_F : constant Character := 'f';
     Lc_G : constant Character := 'g';
     Lc_H : constant Character := 'h';
     Lc_I : constant Character := 'i';
     Lc_J : constant Character := 'j';
     Lc_K : constant Character := 'k';
     Lc_L : constant Character := 'l';
     Lc_M : constant Character := 'm';
     Lc_N : constant Character := 'n';
     Lc_O : constant Character := 'o';
     Lc_P : constant Character := 'p';
     Lc_Q : constant Character := 'q';
     Lc_R : constant Character := 'r';
     Lc_S : constant Character := 's';
     Lc_T : constant Character := 't';
     Lc_U : constant Character := 'u';
     Lc_V : constant Character := 'v';
     Lc_W : constant Character := 'w';
     Lc_X : constant Character := 'x';
     Lc_Y : constant Character := 'y';
     Lc_Z : constant Character := 'z';

  end Ascii;

  type String is array (Positive range <>) of Character with Pack;

  function "<"  (Left, Right : String) return Boolean;
  function "<=" (Left, Right : String) return Boolean;
  function ">=" (Left, Right : String) return Boolean;
  function ">"  (Left, Right : String) return Boolean;

  type Wide_String is array (Positive range <>) of Wide_Character with Pack;

  function "<"  (Left, Right : Wide_String) return Boolean;
  function "<=" (Left, Right : Wide_String) return Boolean;
  function ">=" (Left, Right : Wide_String) return Boolean;
  function ">"  (Left, Right : Wide_String) return Boolean;

  type Wide_Wide_String is array (Positive range <>) of Wide_Wide_Character with Pack;

  function "<"  (Left, Right : Wide_Wide_String) return Boolean;
  function "<=" (Left, Right : Wide_Wide_String) return Boolean;
  function ">=" (Left, Right : Wide_Wide_String) return Boolean;
  function ">"  (Left, Right : Wide_Wide_String) return Boolean;

  type Duration is delta 10.0**(-9) range -((2 ** 63 - 1) * 10.0**(-9)) .. +((2 ** 63 - 1) * 10.0**(-9))
    with Small => 10.0**(-9), Size => 64;

  function "<"  (Left, Right : Duration) return Boolean;
  function "<=" (Left, Right : Duration) return Boolean;
  function ">=" (Left, Right : Duration) return Boolean;
  function ">"  (Left, Right : Duration) return Boolean;

  Constraint_Error : exception;
  Program_Error    : exception;
  Storage_Error    : exception;
  Tasking_Error    : exception;

end Standard;
