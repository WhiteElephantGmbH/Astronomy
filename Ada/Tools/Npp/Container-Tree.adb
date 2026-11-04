-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Container.Tree is

  type Node is record
    Data  : aliased Element;
    Left  : Item;
    Right : Item;
  end record;


  procedure Clear (The_Tree : in out Item) is
  begin
    The_Tree := null;
  end Clear;


  function New_With (Data : Element) return Item is
  begin
    return new Node'(Data  => Data,
                     Left  => null,
                     Right => null);
  end New_With;


  procedure Add (Data :        Element;
                 To   : in out Item) is
  begin
    if To = null then
      To := New_With (Data);
    else
      case Comparison (Data, To.Data) is
      when Equal =>
        null;
      when Before =>
        Add (Data, To.Left);
      when After =>
        Add (Data, To.Right);
      end case;
    end if;
  end Add;


  procedure Add (Data        :        Element;
                 To          : in out Item;
                 Data_Cursor :    out Cursor) is
  begin
    if To = null then
      To := New_With (Data);
      Data_Cursor := To.Data'access;
    else
      case Comparison (Data, To.Data) is
      when Equal =>
        Data_Cursor := To.Data'access;
      when Before =>
        Add (Data, To.Left, Data_Cursor);
      when After =>
        Add (Data, To.Right, Data_Cursor);
      end case;
    end if;
  end Add;


  procedure Add (Id          :        Key;
                 To          : in out Item;
                 Data_Cursor :    out Cursor) is
  begin
    if To = null then
      To := New_With (Element_With (Id));
      Data_Cursor := To.Data'access;
    else
      case Key_Comparison (Id, To.Data) is
      when Equal =>
        Data_Cursor := To.Data'access;
      when Before =>
        Add (Id, To.Left, Data_Cursor);
      when After =>
        Add (Id, To.Right, Data_Cursor);
      end case;
    end if;
  end Add;


  procedure Add (The_Tree :        Item;
                 To       : in out Item) is
  begin
    if The_Tree /= null then
      Add (The_Tree.Left, To);
      Add (The_Tree.Data, To);
      Add (The_Tree.Right, To);
    end if;
  end Add;


  procedure Put (Data :        Element;
                 To   : in out Item) is
  begin
    if To = null then
      To := New_With (Data);
    else
      case Comparison (Data, To.Data) is
      when Equal =>
        To.Data := Data;
      when Before =>
        Put (Data, To.Left);
      when After =>
        Put (Data, To.Right);
      end case;
    end if;
  end Put;


  procedure Get (Id          :        Key;
                 From        :        Item;
                 Data_Cursor :    out Cursor) is
  begin
    if From = null then
      Data_Cursor := null;
    else
      case Key_Comparison (Id, From.Data) is
      when Equal =>
        Data_Cursor := From.Data'access;
      when Before =>
        Get (Id, From.Left, Data_Cursor);
      when After =>
        Get (Id, From.Right, Data_Cursor);
      end case;
    end if;
  end Get;


  procedure Iterator (The_Tree : Item) is
  begin
    if The_Tree /= null then
      Iterator (The_Tree.Left);
      Visit (The_Tree.Data'access);
      Iterator (The_Tree.Right);
    end if;
  end Iterator;


  procedure Iterator_With (Value    : Parameter;
                           The_Tree : Item) is
  begin
    if The_Tree /= null then
      Iterator_With (Value, The_Tree.Left);
      Visit_With (Value, The_Tree.Data'access);
      Iterator_With (Value, The_Tree.Right);
    end if;
  end Iterator_With;


  function Elements_Of (The_Tree : Item) return Elements is
    No_Elements : Elements(1..0);
  begin
    if The_Tree = null then
      return No_Elements;
    else
      return Elements_Of (The_Tree.Left) & The_Tree.Data & Elements_Of (The_Tree.Right);
    end if;
  end Elements_Of;


end Container.Tree;

