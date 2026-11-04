-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Storage;

package Memory with Preelaborate is

  Global_Pool : constant Storage.Pool_Access;

  ------------------
  -- Pool Handling
  ------------------

  Pool : constant Storage.Pool_Access;

  subtype Bucket is Storage.Bucket;

  function New_Bucket return Bucket;

  function Actual_Bucket return Bucket;

  procedure Assign_Default_Bucket;

  procedure Assign (Item : Bucket);

  procedure Clear (Item : Bucket);


  procedure Clear; -- all

private

  Pool : constant Storage.Pool_Access := new Storage.Pool;

  Global_Pool : constant Storage.Pool_Access := new Storage.Pool;

end Memory;
