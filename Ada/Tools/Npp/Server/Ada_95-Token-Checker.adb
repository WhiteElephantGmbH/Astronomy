-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Ada_95.Token.Checker is

  use all type Lexical.Style_Pragma;

  procedure Define_Block_Label_Usage (Unit      : Data.Unit_Handle;
                                      The_Style : Lexical.Style_Pragma) is
  begin
    case The_Style is
    when Is_Style_Astronomy | Is_Style_Soudronic | Is_Style_White_Elephant =>
      null;
    when Is_Style_None | Is_Style_Unrestricted =>
      Unit.Is_Used := True;
    end case;
  end Define_Block_Label_Usage;


  function Has_Style (The_Style : Lexical.Style_Pragma) return Boolean is
  begin
    case The_Style is
    when Is_Style_Astronomy | Is_Style_Soudronic | Is_Style_White_Elephant =>
      return True;
    when Is_Style_Unrestricted | Is_Style_None =>
      return False;
    end case;
  end Has_Style;


  function Is_Ok (Item      : Identifier_Handle;
                  The_Style : Lexical.Style_Pragma) return Boolean is
  begin
    if Has_Style (The_Style) then
      if Item.all in Operator_Symbol'class then
        if not Name.Is_Lower (Item.Id) then
          return False;
        end if;
      else
        if not Name.Is_Capitalized_Or_Upper (Item.Id) then
          return False;
        end if;
      end if;
    end if;
    return True;
  end Is_Ok;


  function Is_Real_Time (The_Style : Lexical.Style_Pragma) return Boolean is
  begin
    case The_Style is
    when Is_Style_Astronomy | Is_Style_Soudronic =>
      return True;
    when Is_Style_White_Elephant | Is_Style_Unrestricted | Is_Style_None =>
      return False;
    end case;
  end Is_Real_Time;


  function Is_Restricted  (The_Style : Lexical.Style_Pragma) return Boolean is
  begin
    case The_Style is
    when Is_Style_Astronomy | Is_Style_Soudronic | Is_Style_White_Elephant | Is_Style_None=>
      return True;
    when Is_Style_Unrestricted =>
      return False;
    end case;
  end Is_Restricted;

end Ada_95.Token.Checker;
