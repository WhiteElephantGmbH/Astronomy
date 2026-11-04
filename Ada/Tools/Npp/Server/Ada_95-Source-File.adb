-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Unchecked_Deallocation;
with File;
with Text;

package body Ada_95.Source.File is

  function Exists (Id : String) return Boolean is
  begin
    return Standard.File.Exists (Id);
  end Exists;


  function New_With (Id : String) return Item is
  begin
    return Item'(Id'length, Id, new Ada.Text_IO.File_Type);
  end New_With;


  function Update_Time (The_Object : Item) return Time is
  begin
    return Time(Standard.File.Modification_Time_Of (The_Object.Id));
  end Update_Time;


  procedure Open (The_Object : Item) is
  begin
    Ada.Text_IO.Open (The_Object.The_File.all, Ada.Text_IO.In_File, The_Object.Id);
    if Text.Has_Skipped_Bom_8 (The_Object.The_File.all) then
      null;
    end if;
  end Open;


  function Next_Line (From : Item) return Line is

    function Get_Rest (Part : String) return String is
       Buffer : String (1 .. Part'length);
       Last   : Natural;
    begin
       Ada.Text_IO.Get_Line (From.The_File.all, Buffer, Last);
       declare
          Rest : constant String := Part & Buffer (1 .. Last);
       begin
          if Last < Buffer'last then
             return Rest;
          else
             return Get_Rest (Rest);
          end if;
       end;
    end Get_Rest;

    Buffer : String(1 .. 500);
    Last   : Natural;

  begin
    Ada.Text_IO.Get_Line (From.The_File.all, Buffer, Last);
    if Last < Buffer'last then
       return Line(Buffer (1 .. Last));
    else
       return Line(Get_Rest (Buffer (1 .. Last)));
    end if;
  end Next_Line;


  function End_Of (The_Object : Item) return Boolean is
  begin
    return Ada.Text_IO.End_Of_File (The_Object.The_File.all);
  end End_Of;


  procedure Close (The_Object : in out Item) is
    procedure Dispose is new Ada.Unchecked_Deallocation (Ada.Text_IO.File_Type, File_Access);
  begin
    Ada.Text_IO.Close (The_Object.The_File.all);
    Dispose (The_Object.The_File);
  end Close;

end Ada_95.Source.File;
