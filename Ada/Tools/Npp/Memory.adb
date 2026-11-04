-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Memory is

  function New_Bucket return Bucket is
  begin
    return Storage.New_Bucket_For (Pool);
  end New_Bucket;

  function Actual_Bucket return Bucket is
  begin
    return Storage.Actual_Bucket_Of (Pool);
  end Actual_Bucket;

  procedure Assign_Default_Bucket is
  begin
    Storage.Assign_Default_For (Pool);
  end Assign_Default_Bucket;

  procedure Assign (Item : Bucket) is
  begin
    Storage.Assign (Item);
  end Assign;

  procedure Clear (Item : Bucket) is
  begin
    Storage.Clear (Item);
  end Clear;

  procedure Clear is
  begin
    Storage.Clear (Pool);
    Storage.Clear (Global_Pool);
  end Clear;

end Memory;
