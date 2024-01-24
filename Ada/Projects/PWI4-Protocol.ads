-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

private package PWI4.Protocol is

  Parsing_Error : exception;

  function Boolean_Of (Image : String) return Boolean;

  function Image_Of (Item : Boolean) return String;


  Meters_Delta : constant := 0.1;

  Meters_Lower_Limit : constant := -728.0; -- Dead See when dryed up
  Meters_Upper_Limit : constant := 8848.0; -- Himalaya

  type Meters is delta Meters_Delta range Meters_Lower_Limit .. Meters_Upper_Limit with Small => Meters_Delta;

  Undefined_Meters : constant Meters := Meters_Lower_Limit;

  function Meters_Of (Image : String) return Meters;

  function Image_Of (Item : Meters) return String;

  Day_Delta : constant := 0.000_000_01;

  type Julian_Day is delta Day_Delta range 0.0 .. 100_000_000.0 - Day_Delta with Small => Day_Delta;

  function Jd_Of (Image : String) return Julian_Day;

  function Image_Of (Item : Julian_Day) return String;


  function Focuser_Position_Of (Image : String) return Microns;

  type Focuser_Info is record
    Exists       : Boolean;
    Is_Connected : Boolean;
    Is_Enabled   : Boolean;
    Is_Moving    : Boolean;
    Position     : Microns;
  end record;

  type Rotator_Info is record
    Exists        : Boolean;
    Is_Connected  : Boolean;
    Is_Enabled    : Boolean;
    Is_Moving     : Boolean;
    Is_Slewing    : Boolean;
    Field_Angle   : Degrees;
    Mech_Position : Degrees;
  end record;

  function Arc_Second_Of (Image : String) return Arc_Second;

  type Error_Code is new Natural;

  function Error_Code_Of (Image : String) return Error_Code;

  function Image_Of (Item : Error_Code) return String;

  type Mount_Flag is record
    Is_Connected     : Boolean;
    Is_Slewing       : Boolean;
    Is_Tracking      : Boolean;
    Axis0_Is_Enabled : Boolean;
    Axis1_Is_Enabled : Boolean;
  end record with Pack;

  type Spiral_Data is record
    X_Step : Arc_Second := 0.0;
    Y_Step : Arc_Second := 0.0;
  end record;

  type Mount_Info is record
    Flags                      : Mount_Flag;
    Julian_Date                : Julian_Day;
    Ra                         : Hours;
    Dec                        : Degrees;
    Ra_Target                  : Hours;
    Dec_Target                 : Degrees;
    Ra_J2000                   : Hours;
    Dec_J2000                  : Degrees;
    Azimuth                    : Degrees;
    Altitude                   : Degrees;
    Axis0                      : Axis_Data;
    Axis1                      : Axis_Data;
    Spiral_Offsets             : Spiral_Data;
    Model                      : Model_Data;
    Field_Angle_At_Target      : Degrees;
    Field_Angle_Rate_At_Target : Degrees;
  end record;

  type Port_Number is new Integer range -1 .. 2;

  function Port_Number_Of (Image : String) return Port_Number;

  function Image_Of (Item : Port_Number) return String;

  type M3_Info is record
    Exists : Boolean := False;
    Port   : Port_Number;
 end record;

  type Site_Info is record
    Latitude  : Degrees;
    Longitude : Degrees;
    Height    : Meters;
    Lmst      : Hours;
  end record;

  type Response is record
    Site    : Site_Info;
    Mount   : Mount_Info;
    Focuser : Focuser_Info;
    Rotator : Rotator_Info;
    M3      : M3_Info;
  end record;

  procedure Parse (Data : String);

  package Mount is

    function Info return Mount_Info;

  end Mount;

  package Focuser is

    function Info return Focuser_Info;

  end Focuser;

  package Rotator is

    function Info return Rotator_Info;

  end Rotator;

  package M3 is

    function Info return M3_Info;

  end M3;

end PWI4.Protocol;
