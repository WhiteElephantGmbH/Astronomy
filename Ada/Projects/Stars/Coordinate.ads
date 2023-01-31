-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

package Coordinate is

  Value_Error : exception;

  type Value is new Long_Float;

  type Declination is new Value;

  type Cartesian is private;

  type Right_Ascension is new Value;

  type Polar is private;

  type Position is new Value;

  type Hemisphere is (Northern, Southern);

  procedure Set (Item : Hemisphere);

  function Value_Of (Item : Value)       return Declination;
  function Value_Of (Item : Declination) return Value;

  function Radian_Of (Item : Declination)     return Value;
  function Radian_Of (Item : Right_Ascension) return Value;

  function Arc_Of (Item : Value) return Right_Ascension;
  function Arc_Of (Item : Value) return Declination;

  function Dec_Of (Item : String)    return Declination;
  function Dec_Of (Item : Cartesian) return Declination;
  function Dec_Of (Item : Polar)     return Declination;
  function Dec_Of (Item : Polar)     return String;

  function Ra_Of (Item : String)    return Right_Ascension;
  function Ra_Of (Item : Cartesian) return Right_Ascension;
  function Ra_Of (Item : Polar)     return Right_Ascension;
  function Ra_Of (Item : Polar)     return String;

  function X_Of (Item : Cartesian) return Position;
  function X_Of (Item : Polar)     return Position;

  function Y_Of (Item : Cartesian) return Position;
  function Y_Of (Item : Polar)     return Position;

  function Point_Of (Item : Polar) return Cartesian;
  function Point_Of (X    : Position;
                     Y    : Position) return Cartesian;
  function Point_Of (Ra   : Right_Ascension;
                     Dec  : Declination) return Cartesian;

  function Location_Of (Item : Cartesian) return Polar;
  function Location_Of (Ra   : Right_Ascension;
                        Dec  : Declination) return Polar;
  function Location_Of (X    : Position;
                        Y    : Position) return Polar;

  function Orientation return Declination;

  function Is_Outside (Item : Declination) return Boolean;

  function Image_Of (Item : Right_Ascension) return String;
  function Image_Of (Item : Declination)     return String;

private
  type Polar is record
    Dec : Declination;
    Ra  : Right_Ascension;
  end record;

  type Cartesian is record
    X : Position;
    Y : Position;
  end record;

end Coordinate;