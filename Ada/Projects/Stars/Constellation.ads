-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

package Constellation is

  type Part is record
    From : Natural;
    To   : Natural;
  end record;

  function Read (Filename : String) return Boolean;

  procedure Add (Item : Part;
                 Name : String);

  function Removed (Item : Part) return Boolean;

  function Is_Used (Star : Natural) return Boolean;

  generic
    with procedure Handler (Item : Part);
  procedure Itterate;

  procedure Save (Filename : String);

end Constellation;
