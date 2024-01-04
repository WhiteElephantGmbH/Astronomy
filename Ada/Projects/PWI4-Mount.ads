-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package PWI4.Mount is

  procedure Set_Enable_Delay (Time : Duration);

  subtype Axis_Rate is Degrees; -- per second

  type Offset_Axis is (Axis0, Axis1, Dec, Ra, Path, Transverse);

  type Offset_Command is (Add_Arcsec, Add_Gradual_Offset_Arcsec, Gradual_Offset_Seconds, Reset,
                          Set_Rate_Arcsec_Per_Sec, Set_Total_Arcsec, Stop, Stop_Gradual_Offset, Stop_Rate);

  type State is (Disconnected, -- not Connected
                 Connected,    -- Connected and not (Azm_Enabled and Alt_Enabled)
                 Enabled,      -- Azm_Enabled and Alt_Enabled and not Homed
                 Stopped,      -- Homed and not Slewing and not Tracking
                 Approaching,  -- Slewing or Slewing and Tracking
                 Tracking,     -- Tracking and not Slewing
                 Error);

  type Information is record
    Status    : State;
    Ra        : Hours;
    Dec       : Degrees;
    Ra_J2000  : Hours;
    Dec_J2000 : Degrees;
    Az        : Degrees;
    Alt       : Degrees;
    Az_Axis   : Axis_Data;
    Alt_Axis  : Axis_Data;
    Model     : Model_Data;
  end record;

  function Info return Information;

  procedure Connect;

  procedure Disconnect;

  procedure Enable;

  procedure Disable;

  procedure Find_Home;

  procedure Goto_Ra_Dec (With_Ra    : Hours;
                         With_Dec   : Degrees;
                         From_J2000 : Boolean := False);

  procedure Goto_Alt_Az (Alt : Degrees;
                         Az  : Degrees);

  procedure Confirm_Goto;

  procedure Follow_Tle (Line_1 : String;
                        Line_2 : String;
                        Line_3 : String);

  procedure Set_Offset (Axis    : Offset_Axis;
                        Command : Offset_Command;
                        Item    : Arc_Second);

  procedure Stop_Rates;

  procedure Spiral_Offset_Center;

  procedure Spiral_Offset_Next;

  procedure Spiral_Offset_Previous;

  procedure Reset_Moving_Target;

  procedure Set_Gradual_Offsets (Delta_Ra  : Arc_Second;
                                 Delta_Dec : Arc_Second);

  procedure Set_Axis0_Wrap (Range_Min : Degrees);

  procedure Add_Point (Ra_J2000  : Hours;
                       Dec_J2000 : Degrees);

  procedure Stop;

end PWI4.Mount;
