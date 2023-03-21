-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerlan                           *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;
with Refraction;
with Time;

package Lx200 is

  Protocol_Error : exception;

  Terminator : Character := '#';

  Flush_Input : String := "" & Terminator;

  Add_Alignment_Point_Ok : constant String := "V";

  Slew_Ok : constant String := "0";

  Synch_Ok : constant String := "Coordinates     matched        ";

  type Extended_Command is (
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
    Set_Slewing_Rate, -- last normal command

    Get_Alignment_Information,
    Get_Axis_RA_Position,
    Get_Axis_Dec_Position,
    Get_Air_Pressure,
    Get_Temperature,
    Get_Julian_Date,
    Get_Number_Of_Alignment_Stars,
    Get_Pointing_State,
    Get_Status,
    New_Alignment_Start,
    New_Alignment_Point,
    New_Alignment_End,
    Set_Axis_RA_Position,
    Set_Axis_Dec_Position,
    Set_Air_Pressure,
    Set_Temperature,
    Set_Julian_Date,
    Set_Ultra_Precision_Mode,
    Slew_To_Axis_Position,
    Slew_To_Park_Position,
    Stop,
    Unpark);

  subtype Command is Extended_Command range Extended_Command'first .. Set_Slewing_Rate;

  function Command_For (Item : String) return String;

  function String_Of (Item      : Extended_Command;
                      Parameter : String := "") return String;

  function Signed_Degrees_Of (Item         : Angle.Value;
                              Front_Digits : Natural := 2) return String;

  function Signed_Degrees_Of (Item : String) return Angle.Value;

  function Hours_Of (Item : Angle.Value) return String;

  function Hours_Of (Item : String) return Angle.Value;

  function Position_Of (Item : Angle.Value) return String;

  function Position_Of (Item : String) return Angle.Value;

  function Air_Pressure_Of (Item : Refraction.Hectopascal) return String;

  function Temperature_Of (Item : Refraction.Celsius) return String;

  function Julian_Date_Of (Item : Time.JD) return String;

  function Time_Offset_Of (Item : Duration) return String;

end Lx200;
