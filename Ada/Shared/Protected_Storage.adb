-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Protected_Storage is

  protected Storage is

    procedure Place (Item : Element);

    function Value return Element;

  private
    The_Element : Element;
  end Storage;


  procedure Set (Item : Element) is
  begin
    Storage.Place (Item);
  end Set;


  function Data return Element is
  begin
    return Storage.Value;
  end Data;


  protected body Storage is

    procedure Place (Item : Element) is
    begin
      The_Element := Item;
    end Place;

    function Value return Element is
    begin
      return The_Element;
    end Value;

  end Storage;

end Protected_Storage;
