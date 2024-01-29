-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;
with Database.Objects;
with Space;
with Time;

package Object is

  Unknown : constant := Database.Unknown;

  First : constant := Database.First;

  subtype Number is Database.Objects.Number;

  subtype Index is Database.Objects.Index;

  subtype Greek_Letter is Database.Greek_Letter;

  subtype Constellation is Database.Constellation;

  subtype Kind is Database.Object_Type;

  subtype Interstellar is Kind range Database.Cloud .. Database.Supernova_Remnant;
  subtype Galaxy       is Kind range Database.BL_Lac .. Database.Seyfert_2_Galaxy;
  subtype Star         is Kind range Database.Alpha2_Cvn_Variable_Star .. Database.Young_Stellar_Object_Star;
  subtype Stars        is Kind range Database.Association_Of_Stars .. Database.Stellar_Stream;

  subtype Parallax is Database.Parallax;

  subtype Light_Years is Database.Light_Years;

  subtype Spectral_Type is Database.Star_Spec_Type;

  subtype Spectral_Class is Database.Star_Class;

  type Magnitude is new Float;

  procedure Set (Ut : Time.Ut);

private
  function Direction_Of (Ra_J2000   : Angle.Degrees;
                         Dec_J2000  : Angle.Degrees;
                         Ra_Motion  : Angle.Degrees;
                         Dec_Motion : Angle.Degrees) return Space.Direction;
end Object;
