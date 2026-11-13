-- *********************************************************************************************************************
-- *                           (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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
with Astap;
with Earth;
with Exif;
with Name;
with Space;
with Time;
with Units;
with Exposure;
with Sensitivity;
private with Network;
private with Traces;

package Picture is

  File_Not_Found  : exception renames Exif.File_Not_Found;
  Invalid_File    : exception renames Exif.Invalid_File;
  Not_Solved      : exception renames Astap.Not_Solved;
  Undefined_Value : exception;

  Maximum_Heigth : constant Angle.Degrees := 5.0;
  Maximum_Width  : constant Angle.Degrees := 5.0;

  function Filename return String;

  function Exists return Boolean;

  procedure Delete;

  procedure Set_Site;

  function Solve (Search_From : Space.Direction) return Boolean;

  function Solved return Boolean;

  procedure Stop_Solving;

  function Time_Stamp return Time.Ut;
  -- returns the time when the picture is taken.

  function Direction return Space.Direction;
  -- returns the center of the picture in J2000 coordinates.

  function Rotation return Units.Rotation;
  -- returns the rotation of the picture.

  procedure Evaluate (Center : out Space.Direction;
                      Lmst   : out Time.Value);
  -- PRECONDITION: The site must be defined (persistent type in package Site) or the camera must provide the location.
  -- evaluate the center of the picture in actual coordinates and the sideral time when the picture was taken.

  function Direction return Earth.Direction;
  -- PRECONDITION: The site must be defined (persistent type in package Site) or the camera must provide the location.
  -- returns the center of the picture in alt/az coordinates.

  procedure Start_Taking_Darks (Target           : Name.Id;
                                Target_Direction : Space.Direction);

  function Taking_Next_Dark_Was_Last return Boolean;

  procedure Start_Taking_Flats;

  function Taking_Next_Flat_Was_Last return Boolean;

  procedure Start_Taking_Lights (Target           : Name.Id;
                                 Target_Direction : Space.Direction);

  procedure Take_Next_Light;

  procedure Stop_Taking;

  function Taking_Exposure return Exposure.Item;

  function Taking_Sensitivity return Sensitivity.Item;

  function Processing_Enabled return Boolean;

  Processor_Not_Connected : exception;

  procedure Define_Lights_Simulation (Id : Name.Id);

  procedure Define_Darks_Simulation;

  procedure Define_Flats_Simulation;

  function Is_Simulated return Boolean;

  function Simulated_Filename return String;

private

  Id : constant String := "Picture";

  package Log is new Traces (Id);

  subtype Picture_Count is Units.Picture_Number range 1 .. 99;

  Processing_Is_Enabled : Boolean := False;
  The_Processor_Address : Network.Ip_Address;
  The_Processor_Port    : Network.Port_Number;
  The_Number_Of_Darks   : Picture_Count;
  The_Number_Of_Flats   : Picture_Count;
  The_Flats_Exposure    : Exposure.Item;
  The_Flats_Sensitivity : Sensitivity.Item;
  The_Exposure          : Exposure.Item;
  The_Sensitivity       : Sensitivity.Item;

  procedure Define (Default_Name : String);

  type Simulation is (None, Darks, Flats, M13, M15, M27, M57, M103, NGC_6992, NGC_7331);

  subtype Simulation_Target is Simulation range M13 .. Simulation'last;

  The_Simulation   : Simulation := None;
  Simulation_Index : Natural := 0;

end Picture;
