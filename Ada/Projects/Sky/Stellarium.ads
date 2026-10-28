-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Angle;
with Language;
with Name;
with Network;
with Space;
with Text;
with Traces;
with Units;

package Stellarium is

  type Goto_Handler is access procedure (Direction : Space.Direction);

  function Landscape_Filename return String;

  function Landscape_Rotation return Angle.Degrees;

  function Language return Language.Kind;

  function Search_Tolerance return Angle.Degrees;

  procedure Startup;

  procedure Shutdown;

  procedure Define_Handler (The_Handler : Goto_Handler);

  procedure Set (Direction : Space.Direction);

  type Target_Data is record
    Name            : Text.String;
    Kind            : Text.String;
    Direction       : Space.Direction;
    Direction_J2000 : Space.Direction;
  end record;

  No_Target : constant Target_Data := (others => <>);

  function Get return Target_Data;

  type Focus_Mode is (Center, Mark, Zoom);

  procedure Set_Focus (Target_Name : String;
                       Mode        : Focus_Mode := Mark);

  procedure Center_On (Target : Name.Id);

  function Camera_Name return String;

  function Camera_Model return Units.Camera_Model;

  procedure Set_Camera (Model : Units.Camera_Model);
  -- raises not found if camera not in list

  function Chip_Resolution return Units.Chip_Resolution;

  function Chip_Size return Units.Chip_Dimension;

  function Chip_Binning return Units.Chip_Binning;

  function Pixel_Size return Units.Pixel_Size;

  function Camera_Rotation return Units.Rotation;

  procedure Set_Camera_Rotation (Value : Units.Rotation);

  function Lens_Name return String;

  procedure Set_No_Lens;

  function Lens_Multiplier return Units.Lens_Multiplier;

  function Telescope_Name return String;

  function Telescope_Model return Units.Telescope_Model;

  procedure Set_Telescope (Model : Units.Telescope_Model);
  -- raises not found if telescope not in list

  function Telescope_Focal_Length return Units.Focal_Length;

  Not_Found        : exception;
  Operation_Failed : exception;

private

  Id : constant String := "Stellarium";

  package Log is new Traces (Id);

  The_Filename : Text.String;

  The_Port_Number : Network.Port_Number;

  The_Search_Tolerance : Angle.Degrees;

  Goto_Is_Enabled : Boolean := False;

  The_Client_Address : Network.Ip_Address := Network.Ip_Address_Of_Host ("localhost");
  The_Client_Port    : Network.Port_Number := 8090;

  type Property_Index is range -1 .. 100;

  Not_Available : constant Property_Index := Property_Index'first;

  subtype Property_Id is Property_Index range 0 .. Property_Index'last;

  type Camera_Data is record
    Name       : Text.String;
    Resolution : Units.Chip_Resolution;
    Binning    : Units.Chip_Binning;
    Dimension  : Units.Chip_Dimension;
  end record;

  function Camera (Index : Property_Id) return Camera_Data;

  type Lens_Data is record
    Name       : Text.String;
    Multiplier : Units.Lens_Multiplier;
  end record;

  No_Lens_Data : constant Lens_Data := ([], Units.Lens_Multiplier'last);

  function Lens (Index : Property_Id) return Lens_Data;

  type Ocular_Data is record
    Name         : Text.String;
    Apparent_FOV : Units.Field_Of_View;
  end record;

  No_Ocular_Data : constant Ocular_Data := ([], Units.Field_Of_View'last);

  function Ocular (Index : Property_Id) return Ocular_Data;

  type Telescope_Data is record
    Name         : Text.String;
    Focal_Length : Units.Focal_Length;
  end record;

  function Telescope (Index : Property_Id) return Telescope_Data;

  Not_Defined : exception;

end Stellarium;
