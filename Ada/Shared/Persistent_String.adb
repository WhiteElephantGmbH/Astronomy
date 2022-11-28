-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Doubly_Linked_Lists_Extension;

package body Persistent_String is

  package Extension is new Doubly_Linked_Lists_Extension (Character, String, Character_List);

  function Item (The_Data : Data) return String is

  begin
    if The_Data.List.Is_Empty then
      return "";
    else
      return Extension.Elements_Of (The_Data.List);
    end if;
  end Item;

  procedure Store (The_Data : in out Data;
                   Value    :        String) is
  begin
    The_Data.List := Extension.List_Of (Value);
  end Store;

end Persistent_String;
