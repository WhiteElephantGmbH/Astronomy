-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;

package Lx200 is

  Terminator : Character := '#';

  type Command is (
    Get_Alignment_Status,
    Get_Product_Name,
    Get_Firmware_Date,
    Get_Firmware_Number,
    Get_Latitude,
    Get_Longitude,
    Get_Sideral_Time,
    Get_Altitude,
    Get_Azimuth,
    Get_Right_Ascension,
    Get_Declination,
    Set_Alt_Az_Alignment,
    Set_Polar_Alignment,
    Set_Latitude,
    Set_Longitude,
    Set_Local_Time,
    Set_Time_Offset,
    Set_Altitude,
    Set_Azimuth,
    Set_Right_Ascension,
    Set_Declination,
    Slew,
    Synchronize,
    Move_East,
    Move_North,
    Move_South,
    Move_West,
    Quit_Move_East,
    Quit_Move_North,
    Quit_Move_South,
    Quit_Move_West,
    Quit_Move,
    Set_Centering_Rate,
    Set_Guiding_Rate,
    Set_Finding_Rate,
    Set_Slewing_Rate);

  function Command_For (Item : String) return String;

  function String_Of (Item      : Command;
                      Parameter : String := "") return String;

  function Signed_Degrees_Of (Item         : Angle.Value;
                              Front_Digits : Natural := 2) return String;

  function Signed_Degrees_Of (Item : String) return Angle.Value;

  function Hours_Of (Item : Angle.Value) return String;

  function Hours_Of (Item : String) return Angle.Value;

end Lx200;
