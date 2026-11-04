-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Characters.Handling;

package body Ada_95.Texts is

  function New_String (Item : String) return Handle is
  begin
    return Handle'(new String'(Item));
  end New_String;


  function Image_Of (Item : Handle) return String is
  begin
    return """" & Item.all & """";
  end Image_Of;


  type Operator_Map is array (Character) of Boolean;

  Is_Operator : constant Operator_Map := ['=' | '<' | '>' | '+' | '-' | '*' | '/' | '&' => True,
                                          others                                        => False];

  function Operator_Of (Item : Handle) return String is
  begin
    if Item'length = 1 and then Is_Operator (Item(Item'first)) then
      return Item.all;
    elsif Item'length = 2 then
      declare
        Operator : constant String := Item.all;
      begin
        if (Operator = "<=") or
           (Operator = ">=") or
           (Operator = "**") or
           (Operator = "/=") or
           (Ada.Characters.Handling.To_Lower(Operator) = "or")
        then
          return Item.all;
        end if;
      end;
    elsif Item'length = 3 then
      declare
        Operator : constant String := Ada.Characters.Handling.To_Lower (Item.all);
      begin
        if (Operator = "and") or
           (Operator = "xor") or
           (Operator = "mod") or
           (Operator = "rem") or
           (Operator = "abs") or
           (Operator = "not")
        then
          return Item.all;
        end if;
      end;
    end if;
    return "";
  end Operator_Of;


  function Length_Of (Item : Handle) return Natural is
  begin
    return Item'length + 2;
  end Length_Of;

end Ada_95.Texts;
