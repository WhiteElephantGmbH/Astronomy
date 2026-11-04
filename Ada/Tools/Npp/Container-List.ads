-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Iterator_Interfaces;
with Memory;

generic
  type Element is private;
  with function "=" (Left, Right : Element) return Boolean is <>;
  with function "<" (Left, Right : Element) return Boolean is <>;
package Container.List with Preelaborate is

  type Item is tagged private
  with
    Constant_Indexing => Constant_Reference,
    Variable_Indexing => Reference,
    Default_Iterator  => Iterate,
    Iterator_Element  => Element;

  type Cursor is private;

  Empty : constant Item;

  No_Element : constant Cursor;

  type Element_Count is range 0 .. 2**31 - 1;

  function Has_Element (Position : Cursor) return Boolean;

  package List_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

  type Constant_Reference_Type (Data : not null access constant Element) is private
    with Implicit_Dereference => Data;

  type Reference_Type (Data : not null access Element) is private
    with Implicit_Dereference => Data;

  function Constant_Reference (The_List : aliased Item;
                               Position : Cursor) return Constant_Reference_Type with Inline;

  function Constant_Reference (The_List : aliased Item;
                               Position : Positive) return Constant_Reference_Type;

  function Reference (The_List : aliased in out Item;
                      Position : Cursor) return Reference_Type with Inline;

  function Reference (The_List : aliased in out Item;
                      Position : Positive) return Reference_Type;

  function Iterate (The_List : Item) return List_Iterator_Interfaces.Forward_Iterator'class;

  type Elements is array (Positive range <>) of Element;

  type Elements_Access is access Elements;

  Not_Found : constant Natural := 0;

  procedure Clear (The_List : in out Item);

  function Is_Empty (The_List : Item) return Boolean;

  function New_With (Data : Element) return Item;

  function Copy_Of (The_List : Item) return Item;

  function Front (The_List   : Item;
                  The_Length : Natural) return Item;

  procedure Append (To   : in out Item;
                    Data :        Element);

  procedure Append (To       : in out Item;
                    The_List :        Item);

  procedure Prepend (To   : in out Item;
                     Data :        Element);

  function Elements_Of (The_List : Item) return Elements;

  function First (The_List : Item) return Element with Inline;

  function First (The_List : Item) return Cursor with Inline;

  procedure Next (The_Cursor : in out Cursor) with Inline;

  function Element_At (The_Cursor : Cursor) return Element with Inline;

  function Last (The_List : Item) return Element with Inline;

  function Length (The_List : Item) return Natural with Inline;

  function Position_Of (Data     : Element;
                        The_List : Item) return Natural;

  function "=" (Left, Right : Item) return Boolean;

  function Comparison (Left, Right : Item) return Result;

  generic
    Null_Element : Element; -- returned if key not found
    type Key is private;
    with function Has (Id         : Key;
                       In_Element : Element) return Boolean;
  function Element_With (Id   : Key;
                         From : Item) return Element;

  generic
    with procedure Visit (The_Element : Element);
  procedure Iterators (The_List : Item);

private

  type Node;

  type Cursor is access Node with Preelaborable_Initialization;
  for Cursor'storage_pool use Memory.Pool.all;

  type Item is tagged record
    The_First  : Cursor;
    The_Last   : Cursor;
    The_Length : Natural := 0;
  end record;

  type Constant_Reference_Type (Data : not null access constant Element) is null record;

  type Reference_Type (Data : not null access Element) is null record;

  Empty : constant Item := (others => <>);

  No_Element : constant Cursor := null;

end Container.List;
