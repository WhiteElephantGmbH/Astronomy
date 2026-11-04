-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Memory;

generic
  type Element is private;
  type Key (<>) is private;

  with function Comparison (Left, Right : Element) return Result;

  with function Key_Comparison (Id          : Key;
                                In_Element  : Element) return Result;

  with function Element_With (Id : Key) return Element;

package Container.Tree with Preelaborate is

  type Item is private;

  type Cursor is access all Element;
  for Cursor'storage_pool use Memory.Pool.all;

  Empty : constant Item;

  procedure Clear (The_Tree : in out Item);

  function New_With (Data : Element) return Item;

  procedure Add (Data :        Element;
                 To   : in out Item);

  procedure Add (Data        :        Element;
                 To          : in out Item;
                 Data_Cursor :    out Cursor);

  procedure Add (Id          :        Key;
                 To          : in out Item;
                 Data_Cursor :    out Cursor);

  procedure Add (The_Tree :        Item;
                 To       : in out Item);

  procedure Put (Data :        Element;
                 To   : in out Item);

  procedure Get (Id          :        Key;
                 From        :        Item;
                 Data_Cursor :    out Cursor);

  generic
    with procedure Visit (The_Cursor : Cursor);
  procedure Iterator (The_Tree : Item);

  generic
    type Parameter is private;
    with procedure Visit_With (Value      : Parameter;
                               The_Cursor : Cursor);
  procedure Iterator_With (Value    : Parameter;
                           The_Tree : Item);

  type Elements is array (Positive range <>) of Element;

  function Elements_Of (The_Tree : Item) return Elements;

private

  type Node;

  type Item is access Node;
  for Item'storage_pool use Memory.Pool.all;

  Empty : constant Item := null;

end Container.Tree;

