-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

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
