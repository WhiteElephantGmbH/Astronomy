-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with System.Storage_Elements;
with System.Storage_Pools;

package Storage is

  pragma Preelaborate;

  type Pool is new System.Storage_Pools.Root_Storage_Pool with private;

  type Pool_Access is access all Pool;

  procedure Allocate (Item                     : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_In_Storage_Elements :        System.Storage_Elements.Storage_Count;
                      Alignment                :        System.Storage_Elements.Storage_Count);

  procedure Deallocate (Item                     : in out Pool;
                        Storage_Address          :        System.Address;
                        Size_In_Storage_Elements :        System.Storage_Elements.Storage_Count;
                        Alignment                :        System.Storage_Elements.Storage_Count);

  function Storage_Size (Item : Pool) return System.Storage_Elements.Storage_Count;

  procedure Clear (Item : Pool_Access);


  type Bucket is private;

  No_Bucket : exception; -- Bucket not created (use New_Bucket_For)

  function New_Bucket_For (Item : Pool_Access) return Bucket;

  function Actual_Bucket_Of (Item : Pool_Access) return Bucket;

  procedure Assign_Default_For (Item : Pool_Access);

  procedure Assign (Item : Bucket);

  procedure Clear (Item : Bucket);

private

  type Bucket_Data;

  type Bucket is access Bucket_Data;

  type Pool is new System.Storage_Pools.Root_Storage_Pool with record
    Actual_Bucket  : Bucket;
    Default_Bucket : Bucket;
    Bucket_List    : Bucket;
  end record;

  procedure Initialize (Item : in out Pool);

  procedure Finalize (Item : in out Pool);

end Storage;
