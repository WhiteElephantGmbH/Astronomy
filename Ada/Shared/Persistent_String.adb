-- *********************************************************************************************************************
-- *                               (c) 2015 by Soudronic AG, Bergdietikon, Switzerland                                 *
-- *                      Developed by White Elephant GmbH, Switzerland (www.white-elephant.ch)                        *
-- *********************************************************************************************************************
-->Style: White_Elephant

package body Persistent_String is

  function Item (The_Data : Data) return String is
  begin
    if The_Data.List.Is_Empty then
      return "";
    else
      return String(The_Data.List.Elements);
    end if;
  end Item;

  procedure Store (The_Data : in out Data;
                   Value    :        String) is
    use type Character_List.Element_List;
  begin
    The_Data.List := +Character_List.Element_List(Value);
  end Store;

end Persistent_String;