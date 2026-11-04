-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Unchecked_Deallocation;

package body Storage is

  use type System.Storage_Elements.Storage_Count;

  subtype Element_Index is System.Storage_Elements.Storage_Count;

  Initial_Storage_Length : constant Element_Index := 2**10;
  Maximum_Storage_Length : constant Element_Index := 2**22;

  First_Index : constant Element_Index := 0;

  Element_Size : constant := System.Storage_Unit; -- bits

  type Element is mod 2 ** Element_Size;
  for Element'size use Element_Size;

  type Data_Elements is array (Element_Index range <>) of aliased Element;

  type Storage_Elements;

  type Storage_Access is access Storage_Elements;

  type Storage_Elements (Last_Index : Element_Index) is record
    Elements : aliased Data_Elements (First_Index .. Last_Index);
    Next     : Storage_Access;
  end record;

  type Bucket_Data is record
    For_Pool  : Pool_Access;
    Data      : Storage_Access;
    Length    : Element_Index;
    Next_Free : Element_Index;
    Next      : Bucket;
  end record;


  procedure Initialize (Item : in out Pool) is
  begin
    Item.Actual_Bucket := New_Bucket_For (Item'unchecked_access);
    Item.Default_Bucket := Item.Actual_Bucket;
  end Initialize;


  procedure Finalize (Item : in out Pool) is
    procedure Dispose is new Ada.Unchecked_Deallocation (Bucket_Data, Bucket);
  begin
    while Item.Bucket_List /= null loop
      declare
        The_Bucket : Bucket := Item.Bucket_List;
      begin
        Clear (The_Bucket);
        Item.Bucket_List := The_Bucket.Next;
        Dispose (The_Bucket);
      end;
    end loop;
  end Finalize;


  -- Pool Handling
  ----------------

  procedure Allocate (Item                     : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_In_Storage_Elements :        System.Storage_Elements.Storage_Count;
                      Alignment                :        System.Storage_Elements.Storage_Count) is

    Element_Length : constant System.Storage_Elements.Storage_Count := Size_In_Storage_Elements;

  begin
    if Item.Actual_Bucket.Data = null then
      if Element_Length > Initial_Storage_Length then
        Item.Actual_Bucket.Next_Free := Element_Length;
      else
        Item.Actual_Bucket.Next_Free := Initial_Storage_Length;
      end if;
      Item.Actual_Bucket.Data := new Storage_Elements (Last_Index => Item.Actual_Bucket.Next_Free - 1);
      Item.Actual_Bucket.Length := Initial_Storage_Length;
      Storage_Address := Item.Actual_Bucket.Data.Elements (First_Index)'address;
    else
      if Element_Length > Item.Actual_Bucket.Length then
        Item.Actual_Bucket.Next_Free := Element_Length;
        declare
          The_Data : constant Storage_Access := Item.Actual_Bucket.Data;
        begin
          Item.Actual_Bucket.Data := new Storage_Elements(Last_Index => Element_Length - 1);
          Item.Actual_Bucket.Data.Next := The_Data;
        end;
        Storage_Address := Item.Actual_Bucket.Data.Elements(First_Index)'address;
      else
        declare
          Aligned_Index : constant Element_Index
            := ((Item.Actual_Bucket.Next_Free + Alignment - 1) / Alignment) * Alignment;
        begin
          Item.Actual_Bucket.Next_Free := Aligned_Index + Element_Length;
          if Item.Actual_Bucket.Next_Free <= Item.Actual_Bucket.Length then
            Storage_Address := Item.Actual_Bucket.Data.Elements (Aligned_Index)'address;
          else
            Item.Actual_Bucket.Next_Free := First_Index + Element_Length;
            declare
              The_Data : constant Storage_Access := Item.Actual_Bucket.Data;
            begin
              if Item.Actual_Bucket.Length < Maximum_Storage_Length then
                Item.Actual_Bucket.Length := Item.Actual_Bucket.Length * 2;
              end if;
              Item.Actual_Bucket.Data := new Storage_Elements(Last_Index => Item.Actual_Bucket.Length -1);
              Item.Actual_Bucket.Data.Next := The_Data;
            end;
            Storage_Address := Item.Actual_Bucket.Data.Elements(First_Index)'address;
          end if;
        end;
      end if;
    end if;
  end Allocate;


  procedure Deallocate (Item                     : in out Pool;
                        Storage_Address          :        System.Address;
                        Size_In_Storage_Elements :        System.Storage_Elements.Storage_Count;
                        Alignment                :        System.Storage_Elements.Storage_Count) is
  begin
    raise Storage_Error; -- not supported
  end Deallocate;


  function Storage_Size (Item : Pool) return System.Storage_Elements.Storage_Count is
  begin
    raise Storage_Error; -- not supported
    return 0;
  end Storage_Size;


  procedure Clear (Item : Pool_Access) is
  begin
    Finalize (Item.all);
    Initialize (Item.all);
  end Clear;


  -- Bucket Handling
  ------------------

  function New_Bucket_For (Item : Pool_Access) return Bucket is

    The_Bucket : constant Bucket := new Bucket_Data'(For_Pool => Item,
                                                     Data     => null,
                                                     Length   => 0,
                                                     Next_Free => 0,
                                                     Next      => Item.Bucket_List);
  begin
    Item.Bucket_List := The_Bucket;
    return The_Bucket;
  end New_Bucket_For;


  function Actual_Bucket_Of (Item : Pool_Access) return Bucket is
  begin
    return Item.Actual_Bucket;
  end Actual_Bucket_Of;


  procedure Assign_Default_For (Item : Pool_Access) is
  begin
    Item.Actual_Bucket := Item.Default_Bucket;
  end Assign_Default_For;


  procedure Assign (Item : Bucket) is
  begin
    if Item = null then
      raise No_Bucket;
    end if;
    Item.For_Pool.Actual_Bucket := Item;
  end Assign;


  procedure Clear (Item : Bucket) is
    procedure Dispose is new Ada.Unchecked_Deallocation (Storage_Elements, Storage_Access);
  begin
    if Item = null then
      raise No_Bucket;
    end if;
    while Item.Data /= null loop
      declare
        The_Data : Storage_Access := Item.Data;
      begin
        Item.Data := The_Data.Next;
        Dispose (The_Data);
      end;
    end loop;
  end Clear;

end Storage;
