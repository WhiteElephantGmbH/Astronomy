-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;

package Ada_95.Source.File is

  function Exists (Id : String) return Boolean;

  type Item is new Source.Object with private;

  function New_With (Id : String) return Item;

  overriding function Update_Time (The_Object : Item) return Time;

  overriding procedure Open (The_Object : Item);

  overriding function Next_Line (From : Item) return Line;

  overriding function End_Of (The_Object : Item) return Boolean;

  overriding procedure Close (The_Object : in out Item);

private

  type File_Access is access Ada.Text_IO.File_Type;
  for File_Access'storage_pool use Memory.Global_Pool.all;

  type Item is new Source.Object with record
    The_File : File_Access;
  end record;

end Ada_95.Source.File;
