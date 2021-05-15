-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_White_Elephant;

with Angle, Earth, Space;

package Alignment is

  type Offset_Range is (Ra, Dec);

  type Offsets is array (Offset_Range) of Angle.Degrees;

  Zero_Offset : constant Offsets := (0.0, 0.0);

  Failure : exception;

  procedure Set (Direction : Earth.Direction;
                 Offset    : Earth.Direction);

  function Has_One_Star_Offsets return Boolean;

  function Is_One_Star return Boolean;

  procedure Store;

  procedure Add;

  procedure Apply;

  procedure Add_First;

  procedure Add_Second;

  procedure Add_Third;

  procedure Read;

  procedure Set (Cone_Error : Angle.Value);

  procedure Set (Pole_Offsets : Earth.Direction);

  procedure Set (Ra_Offset  : Angle.Degrees;
                 Dec_Offset : Angle.Degrees);

  function Cone_Error return Angle.Value;

  function Pole_Offsets return Earth.Direction;

  function Rotations return Space.Direction;

  function System_Error return Angle.Value;

  function Corrections return Space.Direction;

  function Synchronized_Offsets return Offsets;

  function Has_Correction return Boolean;

  function Has_Correction_Data return Boolean;

  function Number_Of_Aligned_Stars return Natural;

  procedure Clear_Corrections;

end Alignment;
