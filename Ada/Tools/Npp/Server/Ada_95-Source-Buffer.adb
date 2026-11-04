-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Text;

package body Ada_95.Source.Buffer is

  The_Data  : Server.Source_Buffer;
  The_Last  : Natural;
  The_Index : Positive;


  procedure Initialize (Data : String) is
  begin
    The_Last := The_Data'first + Data'last - Data'first;
    The_Data(The_Data'first..The_Last) := Data;
  end Initialize;

  function New_With (Id : String) return Item is
  begin
    return Item'(Id'length, Id);
  end New_With;


  function Update_Time (The_Object : Item) return Time is
    pragma Unreferenced (The_Object);
  begin
    return Time(AC.Clock);
  end Update_Time;


  procedure Open (The_Object : Item) is
    pragma Unreferenced (The_Object);
  begin
    if Text.Has_Bom_8 (The_Data) then
      The_Index := The_Data'first + Text.Bom_8'length;
    else
      The_Index := The_Data'first;
    end if;
  end Open;


  function End_Of (The_Object : Item) return Boolean is
    pragma Unreferenced (The_Object);
  begin
    return The_Index > The_Last;
  end End_Of;


  function Next_Line (From : Item) return Line is
    pragma Unreferenced (From);
    First : constant Positive := The_Index;
    Last  : Natural := The_Last;
  begin
    while The_Index <= The_Last loop
      case The_Data(The_Index) is
      when Eol | Ascii.Nul =>
        Last := The_Index - 1;
        The_Index := The_Index + 1;
        exit;
      when Ascii.Cr =>
        Last := The_Index - 1;
        if The_Index < The_Last and then The_Data(The_Index + 1) = Eol then
          The_Index := The_Index + 2;
        else
          The_Index := The_Index + 1;
        end if;
        exit;
      when others =>
        The_Index := The_Index + 1;
      end case;
    end loop;
    declare
      subtype Data_Line is Line(Column_Range'first .. Column_Position(Natural(Column_Range'first) + Last - First));
    begin
      return Data_Line(The_Data(First .. Last));
    end;
  end Next_Line;


  procedure Close (The_Object : in out Item) is
    pragma Unreferenced (The_Object);
  begin
    null;
  end Close;

end Ada_95.Source.Buffer;
