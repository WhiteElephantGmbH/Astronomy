-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Calendar;
with Memory;
with Server;

package Ada_95.Source is

  subtype Column_Position is Server.Column_Position;

  subtype Column_Range is Column_Position range 1 .. Column_Position'last;

  type Line is array (Column_Range range <>) of Character with Pack;

  package AC renames Ada.Calendar;

  type Time is new AC.Time;

  Undefined_Time : constant Time := Time(AC.Time_Of (Year  => AC.Year_Number'first,
                                                     Month => AC.Month_Number'first,
                                                     Day   => AC.Day_Number'first));

  type Object (Length : Natural) is abstract tagged record
    Id : String(1..Length);
  end record;

  type Handle is access Object'class;
  for Handle'storage_pool use Memory.Global_Pool.all;

  function Update_Time (Item : Object) return Time is abstract;

  procedure Open (Item : Object) is abstract;

  function Next_Line (From : Object) return Line is abstract;

  function End_Of (Item : Object) return Boolean is abstract;

  procedure Close (Item : in out Object) is abstract;

end Ada_95.Source;
