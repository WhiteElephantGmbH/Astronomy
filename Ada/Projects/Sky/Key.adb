-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Application;
with Traces;

package body Key is

  package Log is new Traces (Package_Name & " Key");


  function New_Item return String is
    Filename : constant String := Application.Composure (Package_Name, "key");
    Key_File : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Open (Key_File, Mode => Ada.Text_IO.In_File, Name => Filename);
    declare
      Item : constant String := Ada.Text_IO.Get_Line (Key_File);
    begin
      Ada.Text_IO.Close (Key_File);
      return Item;
    end;
  exception
  when others =>
    Log.Error ("Filename " & Filename & " not found");
    return "";
  end New_Item;

end Key;
