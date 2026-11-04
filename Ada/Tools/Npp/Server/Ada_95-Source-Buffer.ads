-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Ada_95.Source.Buffer is

  Eol : constant Character := Ascii.Lf;

  type Item is new Source.Object with private;

  procedure Initialize (Data : String);

  function New_With (Id : String) return Item;

  overriding function Update_Time (The_Object : Item) return Time;

  overriding procedure Open (The_Object : Item);

  overriding function Next_Line (From : Item) return Line;

  overriding function End_Of (The_Object : Item) return Boolean;

  overriding procedure Close (The_Object : in out Item);

private

  type Item is new Source.Object with null record;

end Ada_95.Source.Buffer;
