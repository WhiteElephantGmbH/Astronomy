-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
-- *    Supports Planewave Interface version PWI_4.1.6
-- *********************************************************************************************************************
pragma Style_White_Elephant;

private package PWI4.Protocol is

  Parsing_Error : exception;

  function Boolean_Of (Image : String) return Boolean;

  function Image_Of (Item : Boolean) return String;


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
    Index         : Device_Index;
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

  type Axis_Enabled is array (Natural range 0 .. 1) of Boolean;

  type Mount_Flag is record
    Has_Error       : Boolean := False;
    Is_Connected    : Boolean;
    Is_Slewing      : Boolean;
    Is_Tracking     : Boolean;
    Axis_Is_Enabled : Axis_Enabled;
  end record with Pack;

  type Spiral_Data is record
    X_Step : Arc_Second := 0.0;
    Y_Step : Arc_Second := 0.0;
  end record;

  type Mount_Info is record
    Flags                      : Mount_Flag;
    Count                      : Update_Count;
    Julian_Date                : Julian_Day;
    Ra                         : Hours;
    Dec                        : Degrees;
    Ra_Target                  : Hours;
    Dec_Target                 : Degrees;
    Ra_J2000                   : Hours;
    Dec_J2000                  : Degrees;
    Azimuth                    : Degrees;
    Altitude                   : Degrees;
    Axis                       : Mount_Axis;
    Wrap_Range_Min             : Degrees;
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

  type Response is record
    Site    : Site_Info;
    Mount   : Mount_Info;
    Focuser : Focuser_Info;
    Rotator : Rotator_Info;
    M3      : M3_Info;
  end record;

  procedure Parse (Data : String);

  procedure Set_Error (Status : Status_Code);

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

  package Site is

    function Info return Site_Info;

  end Site;

end PWI4.Protocol;
