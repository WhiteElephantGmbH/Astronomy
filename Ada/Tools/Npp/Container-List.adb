-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Container.List is

  type Node is record
    The_Data : aliased Element;
    The_Next : Cursor;
  end record;


  type List_Access is access all Item;
  for List_Access'storage_size use 0;

  type Iterator is new List_Iterator_Interfaces.Forward_Iterator with record
    Container : List_Access;
    Node      : Cursor;
  end record;

  overriding function First (Object : Iterator) return Cursor;

  overriding function Next (Object   : Iterator;
                            Position : Cursor) return Cursor;


  function Constant_Reference (The_List : aliased Item;
                               Position : Cursor) return Constant_Reference_Type is
  begin
    pragma Unreferenced (The_List);
    return Constant_Reference_Type'(Data => Position.The_Data'access);
  end Constant_Reference;


  function Find (From        : Item;
                 At_Position : Positive) return Cursor with Inline is
    The_Node  : Cursor := From.The_First;
    The_Index : Positive := Positive'first;
  begin
    while The_Node /= null loop
      if At_Position = The_Index then
        return The_Node;
      end if;
      The_Index := The_Index + 1;
      The_Node := The_Node.The_Next;
    end loop;
    raise Constraint_Error;
  end Find;


  function Constant_Reference (The_List : aliased Item;
                               Position : Positive) return Constant_Reference_Type is
  begin
    return (Data => Find (The_List, Position).The_Data'unrestricted_access); --!!! GPL 2016
  end Constant_Reference;


  function Reference (The_List : aliased in out Item;
                      Position : Cursor) return Reference_Type is
    pragma Unreferenced (The_List);
  begin
    return (Data => Position.The_Data'unrestricted_access); --!!! GPL 2016
  end Reference;


  function Reference (The_List : aliased in out Item;
                      Position : Positive) return Reference_Type is
  begin
    return (Data => Find (The_List, Position).The_Data'unrestricted_access); --!!! GPL 2016
  end Reference;


  function Has_Element (Position : Cursor) return Boolean is
  begin
    return Position /= null;
  end Has_Element;


  function Iterate (The_List : Item) return List_Iterator_Interfaces.Forward_Iterator'class is
  begin
    return Iterator'(Container => The_List'unrestricted_access,
                     Node      => null);
  end Iterate;


  function First (Object : Iterator) return Cursor is
  begin
    return Object.Container.The_First;
  end First;


  function Next (Object   : Iterator;
                 Position : Cursor) return Cursor is
    pragma Unreferenced (Object);
  begin
    if Position = null then
      return No_Element;
    end if;
    return Position.The_Next;
  end Next;


  procedure Clear (The_List : in out Item) is
  begin
    The_List := Empty;
  end Clear;


  function Is_Empty (The_List : Item) return Boolean is
  begin
    return The_List.The_Length = 0;
  end Is_Empty;


  function New_With (Data : Element) return Item is

    New_Node : constant Cursor := new Node'(The_Data => Data,
                                            The_Next => null);
  begin
    return (The_First  => New_Node,
            The_Last   => New_Node,
            The_Length => 1);
  end New_With;


  function Copy_Of (The_List : Item) return Item is
    New_List : Item;
    The_Last : Cursor := The_List.The_First;
  begin
    for Count in 1 .. The_List.The_Length loop
      pragma Unreferenced (Count);
      New_List.Append (The_Last.The_Data);
      The_Last := The_Last.The_Next;
    end loop;
    return New_List;
  end Copy_Of;


  function Front (The_List   : Item;
                  The_Length : Natural) return Item is
  begin
    if The_List.The_Length = 0 then
      return Empty;
    elsif The_List.The_Length = The_Length then
      return The_List;
    else
      declare
        New_List : Item;
        The_Last : Cursor := The_List.The_First;
      begin
        for Count in 1 .. The_Length loop
          pragma Unreferenced (Count);
          New_List.Append (The_Last.The_Data);
          The_Last := The_Last.The_Next;
        end loop;
        return New_List;
      end;
    end if;
  end Front;


  function First (The_List : Item) return Element is
  begin
    return The_List.The_First.The_Data;
  end First;


  function First (The_List : Item) return Cursor is
  begin
    return The_List.The_First;
  end First;


  procedure Next (The_Cursor : in out Cursor) is
  begin
    if The_Cursor /= No_Element then
      The_Cursor := The_Cursor.The_Next;
    end if;
  end Next;


  function Element_At (The_Cursor : Cursor) return Element is
  begin
    return The_Cursor.The_Data;
  end Element_At;


  function Last (The_List : Item) return Element is
  begin
    return The_List.The_Last.The_Data;
  end Last;


  procedure Append (To   : in out Item;
                    Data :        Element) is
  begin
    if To.The_First = null then
      To.The_First := new Node'(The_Data => Data,
                                The_Next => null);
      To.The_Last := To.The_First;
    else
      To.The_Last.The_Next := new Node'(The_Data => Data,
                                        The_Next => null);
      To.The_Last := To.The_Last.The_Next;
    end if;
    To.The_Length := To.The_Length + 1;
  end Append;


  procedure Append (To       : in out Item;
                    The_List :        Item) is
  begin
    if To.The_First = null then
      To := The_List;
    else
      To.The_Last.The_Next := The_List.The_First;
      To.The_Last := The_List.The_Last;
    end if;
    To.The_Length := To.The_Length + The_List.The_Length;
  end Append;


  procedure Prepend (To   : in out Item;
                     Data :        Element) is
  begin
    if To.The_First = null then
      To.The_First := new Node'(The_Data => Data,
                                The_Next => null);
      To.The_Last := To.The_First;
    else
      To.The_First := new Node'(The_Data => Data,
                                The_Next => To.The_First);
    end if;
    To.The_Length := To.The_Length + 1;
  end Prepend;


  function Elements_Of (The_List : Item) return Elements is
    The_Elements : Elements (1 .. The_List.The_Length);
    The_Node     : Cursor := The_List.The_First;
    The_Index    : Positive := The_Elements'first;
  begin
    while The_Node /= null loop
      The_Elements (The_Index) := The_Node.The_Data;
      The_Index := The_Index + 1;
      The_Node := The_Node.The_Next;
    end loop;
    return The_Elements;
  end Elements_Of;


  function Length (The_List : Item) return Natural is
  begin
    return The_List.The_Length;
  end Length;


  function Position_Of (Data     : Element;
                        The_List : Item) return Natural is
    The_Node     : Cursor := The_List.The_First;
    The_Position : Positive := Positive'first;
  begin
    while The_Node /= null loop
      if Data = The_Node.The_Data then
        return The_Position;
      end if;
      The_Node := The_Node.The_Next;
      The_Position := Positive'succ(The_Position);
    end loop;
    return Not_Found;
  end Position_Of;


  function "=" (Left, Right : Item) return Boolean is
  begin
    if Left.The_Length /= Right.The_Length then
      return False;
    else
      declare
        Left_Node  : Cursor := Left.The_First;
        Right_Node : Cursor := Right.The_First;
      begin
        while Left_Node /= null loop
          if Left_Node.The_Data /= Right_Node.The_Data then
            return False;
          end if;
          Left_Node := Left_Node.The_Next;
          Right_Node := Right_Node.The_Next;
        end loop;
        return True;
      end;
    end if;
  end "=";


  function Comparison (Left, Right : Item) return Result is
    Left_Node  : Cursor := Left.The_First;
    Right_Node : Cursor := Right.The_First;
  begin
    while Left_Node /= null loop
      if Right_Node = null then
        return After;
      elsif Left_Node.The_Data < Right_Node.The_Data then
        return Before;
      elsif Left_Node.The_Data /= Right_Node.The_Data then
        return After;
      end if;
      Left_Node := Left_Node.The_Next;
      Right_Node := Right_Node.The_Next;
    end loop;
    if Right_Node /= null then
      return Before;
    else
      return Equal;
    end if;
  end Comparison;


  function Element_With (Id   : Key;
                         From : Item) return Element is
    The_Node : Cursor := From.The_First;
  begin
    while The_Node /= null loop
      if Has (Id, The_Node.The_Data) then
        return The_Node.The_Data;
      end if;
      The_Node := The_Node.The_Next;
    end loop;
    return Null_Element;
  end Element_With;


  procedure Iterators (The_List : Item) is
    The_Node : Cursor := The_List.The_First;
  begin
    while The_Node /= null loop
      Visit (The_Node.The_Data);
      The_Node := The_Node.The_Next;
    end loop;
  end Iterators;

end Container.List;
