-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Ada.Numerics.Generic_Elementary_Functions;

package body Coordinate is

  package Math is new Ada.Numerics.Generic_Elementary_Functions (Value);

  Pi : constant := Ada.Numerics.Pi;


  function Sqr (Item : Value) return Value is
  begin
    return Item * Item;
  end Sqr;


  The_Orientation : Value := 1.0;

  procedure Set (Item : Hemisphere) is
  begin
    case Item is
    when Northern =>
      The_Orientation := 1.0;
    when Southern =>
      The_Orientation := - 1.0;
    end case;
  end Set;


  function Value_Of (Item : Value) return Declination is
  begin
    return Declination(The_Orientation * (90.0 - Item * 180.0));
  end Value_Of;


  function Value_Of (Item : Declination) return Value is
  begin
    return (90.0 - The_Orientation * (Value(Item))) / 180.0;
  end Value_Of;


  function Radian_Of (Item : Declination) return Value is
  begin
    return Value(Item) * Pi / 180.0;
  end Radian_Of;


  function Radian_Of (Item : Right_Ascension) return Value is
  begin
    return Value(Item) * Pi / 12.0;
  end Radian_Of;


  function Arc_Of (Item : Value) return Declination is
  begin
    return Declination(Item) * 180.0 / Pi;
  end Arc_Of;


  function Arc_Of (Item : Value) return Right_Ascension is
  begin
    return Right_Ascension(Item) * 12.0 / Pi;
  end Arc_Of;


  function Ra_Of (Item : String) return Right_Ascension is
    -- **h**m**.*s
    -- 0123456   last
  begin
    if (Item(Item'first + 2) /= 'h') or
       (Item(Item'first + 5) /= 'm') or
       (Item(Item'last) /= 's')
    then
      raise Value_Error;
    end if;
    return (Right_Ascension'value(Item(Item'first + 6 .. Item'last - 1)) / 60.0
            + Right_Ascension'value(Item(Item'first + 3 .. Item'first + 4))) / 60.0
           + Right_Ascension'value(Item(Item'first .. Item'first + 1));
  exception
  when others =>
    raise Value_Error;
  end Ra_Of;


  function Ra_Of (Item : Cartesian) return Right_Ascension is
    Ra : Right_Ascension;
  begin
    if Item.X = 0.0 and Item.Y = 0.0 then
      return 0.0;
    else
      Ra := Right_Ascension(Math.Arctan (X => Value(Item.Y), Y => -The_Orientation * Value(Item.X)) * 12.0 / Pi);
      if Ra < 0.0 then
        Ra := Ra + 24.0;
      end if;
    end if;
    return Ra;
  end Ra_Of;


  function Ra_Of (Item : Polar) return Right_Ascension is
  begin
    return Item.Ra;
  end Ra_Of;


  function Ra_Of (Item : Polar) return String is
  begin
    return Image_Of (Item.Ra);
  end Ra_Of;


  function Dec_Of (Item : String) return Declination is
    -- ±**°**'**"
    --  012345678
    First : Natural := Item'first;
    Sign  : Declination := 1.0;
  begin
    case Item(First) is
    when '+' =>
      First := First + 1;
    when '-' =>
      First := First + 1;
      Sign := -Sign;
    when others =>
      null;
    end case;
    if (Item(First + 2) /= '°') or
       (Item(First + 5) /= ''') or
       (Item(First + 8) /= '"')
    then
      raise Value_Error;
    end if;
    return ((Declination'value(Item(First + 6 .. First + 7)) / 60.0
             + Declination'value(Item(First + 3 .. First + 4))) / 60.0
            + Declination'value(Item(First .. First + 1))) * Sign;
  exception
  when others =>
    raise Value_Error;
  end Dec_Of;


  function Dec_Of (Item : Cartesian) return Declination is
  begin
    return Declination(The_Orientation * (90.0 - Math.Sqrt (Sqr(Value(Item.X)) + Sqr(Value(Item.Y))) * 180.0));
  end Dec_Of;


  function Dec_Of (Item : Polar) return Declination is
  begin
    return Item.Dec;
  end Dec_Of;


  function Dec_Of (Item : Polar) return String is
  begin
    return Image_Of (Item.Dec);
  end Dec_Of;


  function X_Of (Item : Cartesian) return Position is
  begin
    return Item.X;
  end X_Of;


  function X_Of (Item : Polar) return Position is
  begin
    return Position(-The_Orientation * Math.Sin (Radian_Of (Item.Ra)) * Value_Of (Item.Dec));
  end X_Of;


  function Y_Of (Item : Cartesian) return Position is
  begin
    return Item.Y;
  end Y_Of;


  function Y_Of (Item : Polar) return Position is
  begin
    return Position(Math.Cos (Radian_Of (Item.Ra)) * Value_Of (Item.Dec));
  end Y_Of;


  function Point_Of (Item : Polar) return Cartesian is
  begin
    return (X => X_Of (Item), Y => Y_Of (Item));
  end Point_Of;


  function Point_Of (X : Position;
                     Y : Position) return Cartesian is
  begin
    return (X => X, Y => Y);
  end Point_Of;


  function Point_Of (Ra   : Right_Ascension;
                     Dec  : Declination) return Cartesian is
     Location : constant Polar := (Dec => Dec, Ra => Ra);
  begin
    return (X => X_Of (Location), Y => Y_Of (Location));
  end Point_Of;


  function Location_Of (Item : Cartesian) return Polar is
  begin
    return (Ra => Ra_Of (Item), Dec => Dec_Of (Item));
  end Location_Of;


  function Location_Of (Ra   : Right_Ascension;
                        Dec  : Declination) return Polar is
  begin
    return (Ra => Ra, Dec => Dec);
  end Location_Of;


  function Location_Of (X : Position;
                        Y : Position) return Polar is
     Point : constant Cartesian := (X => X, Y => Y);
  begin
    return (Ra => Ra_Of (Point), Dec => Dec_Of (Point));
  end Location_Of;


  function Orientation return Declination is
  begin
    return Declination(The_Orientation);
  end Orientation;


  function Is_Outside (Item : Declination) return Boolean is
  begin
    return Coordinate.Orientation * Item < 0.0;
  end Is_Outside;


  function Image_Of (Item     : Value;
                     Fraction : Boolean := False) return String is
    type Decimal is delta 0.1 digits 3;
    Image : constant String := Decimal'image(Decimal(Item));
    Last  : constant Natural := Image'last - Boolean'pos(not Fraction) * 2;
  begin
    if Item < 10.0 then
      return '0' & Image(Image'first + 1 .. Last);
    else
      return Image(Image'first + 1 .. Last);
    end if;
  end Image_Of;


  function Image_Of (Item     : Value;
                     Units    : String;
                     Fraction : Boolean := False)  return String is
    Hours   : constant Value := Value'truncation(Item);
    Minutes : constant Value := Value'truncation((Item - Hours) * 60.0);
    Seconds : constant Value := (Item - Hours) * 3600.0 - Minutes * 60.0;
  begin
    return Image_Of (Hours) & Units(Units'first)
         & Image_Of (Minutes) & Units(Units'first + 1)
         & Image_Of (Seconds, Fraction) & Units(Units'first + 2);
  end Image_Of;


  function Image_Of (Item : Right_Ascension)  return String is
  begin
    return Image_Of (Value(Item), "hms", Fraction => True);
  end Image_Of;


  function Image_Of (Item : Declination) return String is
    Image : constant String := Image_Of (Value(abs(Item)), "°'""");
  begin
    if Item >= 0.0 then
      return ' ' & Image;
    else
      return '-' & Image;
    end if;
  end Image_Of;

end Coordinate;