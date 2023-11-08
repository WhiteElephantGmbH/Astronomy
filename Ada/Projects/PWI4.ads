-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4 is

  Arc_Second_Delta : constant := 0.000_1;
  Arc_Second_Limit : constant := 10_000_000.0 - Arc_Second_Delta;

  type Arc_Second is delta Arc_Second_Delta range -Arc_Second_Limit .. Arc_Second_Limit with Small => Arc_Second_Delta;

  Degrees_Delta : constant := 0.000_000_000_1;
  Degrees_Limit : constant := 1000.0 - Degrees_Delta;

  type Degrees is delta Degrees_Delta range -Degrees_Limit .. Degrees_Limit with Small => Degrees_Delta;

  function Image_Of (Item : Degrees) return String;

  Hours_Delta : constant := 0.000_000_000_01;
  Hours_Limit : constant := 24.0;

  type Hours is delta Hours_Delta range -Hours_Limit .. Hours_Limit with Small => Hours_Delta;

  Undefined_Hours : constant := Hours'first;

  function Image_Of (Item : Hours) return String;

  type Axis_Data is record
    Position : Degrees;
  end record;

  type Microns is range -999_999 .. 999_999;

  type M3_Port is (Unknown, Between, Port_1, Port_2);

  subtype Port is M3_Port range Port_1 .. Port_2;

  function Startup (Filename   : String;
                    Ip_Address : String) return Boolean;

  procedure Shutdown;

  procedure Get_System;

private
  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "");

  function Image_Of (The_Port : Port) return Character;

end PWI4;
