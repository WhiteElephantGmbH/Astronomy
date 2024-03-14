-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Ada.IO_Exceptions;
with Ada.Text_IO;
with Text;

package body Constellation is

  Max_Nr_Of_Parts : constant := 1000;

  type Element is record
    Key  : Part;
    Name : Text.String;
  end record;

  use type Text.String;

  The_Data : array (1..Max_Nr_Of_Parts) of Element;
  The_Last : Natural := 0;


  function Read (Filename : String) return Boolean is

    procedure Store (Line : String) is

      The_Index : Natural := Line'first;

      function Next return String is
        First : constant Natural := The_Index;
        Last  : Natural := First - 1;
      begin
        if First > Line'last then
          return "";
        end if;
        loop
          if Line(The_Index) = ',' then
            The_Index := The_Index + 1;
            exit;
          end if;
          Last := The_Index;
          The_Index := The_Index + 1;
          exit when The_Index > Line'last;
        end loop;
        return Text.Trimmed(Line(First .. Last));
      end Next;

      The_Element : Element;

    begin -- Store
      if Text.Trimmed (Line) /= "" then
        The_Element.Key.From := Natural'value(Next);
        The_Element.Key.To := Natural'value(Next);
        The_Element.Name := [Next];
      end if;
      The_Last := The_Last + 1;
      The_Data(The_Last) := The_Element;
    end Store;

    File       : Ada.Text_IO.File_Type;
    The_Line   : String (1..256);
    The_Length : Natural;

  begin -- Read
    The_Last := 0;
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
    while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, The_Line, The_Length);
      Store (The_Line(1..The_Length));
    end loop;
    Ada.Text_IO.Close (File);
    return True;
  exception
  when Ada.IO_Exceptions.Name_Error =>
    return False;
  end Read;


  procedure Add (Item : Part;
                 Name : String) is
  begin
    The_Last := The_Last + 1;
    The_Data(The_Last).Key := Item;
    The_Data(The_Last).Name := [Name];
  end Add;


  function Removed (Item : Part) return Boolean is
  begin
    for Index in 1 .. The_Last loop
      if ((Item.From, Item.To) = The_Data(Index).Key) or
         ((Item.To, Item.From) = The_Data(Index).Key)
      then
        The_Data (Index .. The_Last - 1) := The_Data (Index + 1 .. The_Last);
        The_Last := The_Last - 1;
        return True;
      end if;
    end loop;
    return False;
  end Removed;


  function Is_Used (Star : Natural) return Boolean is
  begin
    for Index in 1 .. The_Last loop
      if (Star = The_Data(Index).Key.From) or
         (Star = The_Data(Index).Key.To)
      then
        return True;
      end if;
    end loop;
    return False;
  end Is_Used;


  procedure Itterate is
  begin
    for Index in 1 .. The_Last loop
      Handler (The_Data (Index).Key);
    end loop;
  end Itterate;


  procedure Save (Filename : String) is

    function Image_Of (Item : Natural) return String is
      Image : constant String := "     " & Natural'image(Item);
    begin
      return Image(Image'last - 3 .. Image'last);
    end Image_Of;

    File : Ada.Text_IO.File_Type;

  begin -- Save
    Ada.Text_IO.Create (File, Name => Filename);
    for Index in 1 .. The_Last loop
      Ada.Text_IO.Put_Line (File,
                            Image_Of (The_Data(Index).Key.From) & ", " &
                            Image_Of (The_Data(Index).Key.To) & ", " &
                            The_Data(Index).Name);
    end loop;
    Ada.Text_IO.Close (File);
  end Save;

end Constellation;
