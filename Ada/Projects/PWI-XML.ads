-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Numerics;

private package PWI.XML is

  Parsing_Error : exception;

  function Boolean_Of (Image : String) return Boolean;

  function Image_Of (Item : Boolean) return String;

  type Hour   is range 0..23;
  type Minute is range 0..59;
  type Second is range 0..60;

  type Centi_Second is range 0..99;
  type Milli_Second is range 0..999;

  type Universal_Time is record
    Hours   : Hour;
    Minutes : Minute;
    Seconds : Second;
  end record;

  function Time_Of (Image : String) return Universal_Time;

  function Image_Of (Item : Universal_Time) return String;

  type Time is record
    Hours         : Hour;
    Minutes       : Minute;
    Seconds       : Second;
    Milli_Seconds : Milli_Second;
  end record;

  function Time_Of (Image : String) return Time;

  function Image_Of (Item : Time) return String;

  type Local_Sideral_Time is new Time;

  type Julian_Day is delta 0.00000001 range 0.0 .. 99999999.99999999;

  function Jd_Of (Image : String) return Julian_Day;

  function Image_Of (Item : Julian_Day) return String;

  type Status_Info is record
    Utc : Universal_Time;
    Lst : Local_Sideral_Time;
    Jd  : Julian_Day;
  end record;

  type Focuser_Position is new Natural; -- in microns

  function Focuser_Position_Of (Image : String) return Focuser_Position;

  function Image_Of (Item : Focuser_Position) return String;

  type Focuser_Info (Has_Auto_Focus : Boolean) is record
    Connected     : Boolean;
    Position      : Focuser_Position;
    Moving        : Boolean;
    Goto_Complete : Boolean;
    Finding_Home  : Boolean;
    case Has_Auto_Focus is
    when True =>
      Auto_Focus_Busy                  : Boolean;
      Auto_Focus_Last_Result_Success   : Boolean;
      Auto_Focus_Last_Result_Position  : Focuser_Position;
      Auto_Focus_Last_Result_Tolerance : Focuser_Position;
    when False =>
      null;
    end case;
  end record;

  type Rotator_Position is delta 0.001 range 0.0 .. 360.000; -- in degrees

  function Rotator_Position_Of (Image : String) return Rotator_Position;

  function Image_Of (Item : Rotator_Position) return String;

  type Rotator_Info (Has_Derotate : Boolean) is record
    Connected     : Boolean;
    Position      : Rotator_Position;
    Moving        : Boolean;
    Goto_Complete : Boolean;
    Finding_Home  : Boolean;
    case Has_Derotate is
    when True =>
      Alt_Az_Derotate : Boolean;
    when False =>
      null;
    end case;
  end record;

  type Right_Ascension is new Time;

  type Degree is range 0 .. 90;

  type Declination is record
    Is_Positive   : Boolean;
    Degrees       : Degree;
    Minutes       : Minute;
    Seconds       : Second;
    Centi_Seconds : Centi_Second;
  end record;

  function Declination_Of (Image : String) return Declination;

  function Image_Of (Item : Declination) return String;

  type Radian is delta 0.0000001 range -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi;

  function Radian_Of (Image : String) return Radian;

  function Image_Of (Item : Radian) return String;

  function Arc_Second_Of (Image : String) return Arc_Second;

  function Image_Of (Item : Arc_Second) return String;

  type Error_Code is new Natural;

  function Error_Code_Of (Image : String) return Error_Code;

  function Image_Of (Item : Error_Code) return String;

  type Mount_Flag is record
    Connected              : Boolean;
    On_Target              : Boolean;
    Tracking               : Boolean;
    Azm_Enabled            : Boolean;
    Alt_Enabled            : Boolean;
    Is_Finding_Home        : Boolean;
    Encoders_Have_Been_Set : Boolean;
    Pointing_Model_Set     : Boolean;
  end record with Pack;

  type Mount_Info is record
    Flags                : Mount_Flag;
    Ra                   : Right_Ascension;
    Dec                  : Declination;
    Ra_Target            : Right_Ascension;
    Dec_Target           : Declination;
    Ra_2000              : Right_Ascension;
    Dec_2000             : Declination;
    Ra_Radian            : Radian;
    Dec_Radian           : Radian;
    Azm_Radian           : Radian;
    Alt_Radian           : Radian;
    Azm_Rms_Error        : Arc_Second;
    Alt_Rms_Error        : Arc_Second;
    Azm_Motor_Error_Code : Error_Code;
    Alt_Motor_Error_Code : Error_Code;
  end record;

  type Port_Number is new Natural;

  function Port_Number_Of (Image : String) return Port_Number;

  function Image_Of (Item : Port_Number) return String;

  type M3_Position is range 0 .. 360;

  function M3_Position_Of (Image : String) return M3_Position;

  function Image_Of (Item : M3_Position) return String;

  type M3_Info is record
    Connected       : Boolean := False;
    Port            : Port_Number;
    Position_Rotate : M3_Position;
    Position_Tilt   : M3_Position;
    Moving_Rotate   : Boolean;
    Moving_Tilt     : Boolean;
  end record;

  type Celsius is delta 0.1 range -99.9 .. 99.9;

  Undefined : constant Celsius := Celsius'first;

  function Degrees_Of (Image : String) return Celsius;

  function Image_Of (Item : Celsius) return String;

  type Temperature_Info is record
    Primary   : Celsius := Undefined;
    Ambient   : Celsius := Undefined;
    Secondary : Celsius := Undefined;
    Backplate : Celsius := Undefined;
    M3        : Celsius := Undefined;
  end record;

  type Fans_Info is record
    On : Boolean;
  end record;

  type Response is record
    Status      : Status_Info;
    Focuser     : Focuser_Info (Has_Auto_Focus => True);
    Focuser1    : Focuser_Info (Has_Auto_Focus => False);
    Focuser2    : Focuser_Info (Has_Auto_Focus => False);
    Rotator     : Rotator_Info (Has_Derotate => True);
    Rotator1    : Rotator_Info (Has_Derotate => False);
    Rotator2    : Rotator_Info (Has_Derotate => False);
    Mount       : Mount_Info;
    M3          : M3_Info;
    Temperature : Temperature_Info;
    Fans        : Fans_Info;
  end record;

  procedure Parse (Data : String);

  package Fans is

    function Turned_On return Boolean;

  end Fans;

  package Mount is

    procedure Define_Pointing_Model (Filename : String);

    function Defined_Pointing_Model return String;

    function Flags return Mount_Flag;

    function Info return Mount_Info;

  end Mount;

  package M3 is

    function Info return M3_Info;

  end M3;

  package Rotator is

    function Info return Rotator_Info;

    function Info1 return Rotator_Info;

  end Rotator;

end PWI.XML;
