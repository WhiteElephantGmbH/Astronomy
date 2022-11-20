-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Definite_Doubly_Linked_Lists;
with Persistent_Definite_Doubly_Linked_Lists;

generic
  Name : String;
package Persistent_String is

  package Character_List is new Definite_Doubly_Linked_Lists (Character);

  package Characters is new Persistent_Definite_Doubly_Linked_Lists (Name, Character, Character_List);

  type Data is new Characters.Data with null record;

  function Item (The_Data : Data) return String;

  procedure Store (The_Data : in out Data;
                   Value    :        String);

end Persistent_String;
