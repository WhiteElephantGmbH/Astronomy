-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Database.Stars;

package Object.Catalog is

  Unknown_Id : constant := Database.Unknown_Id;
  First_Id   : constant := Unknown_Id + 1;

  type Star_Id is range Unknown_Id .. Database.Stars.Data_Range'last;

  subtype Star is Star_Id range First_Id .. Star_Id'last;

  type HD_Id is new Database.Stars.HD_Id;

  subtype HD is HD_Id range First_Id .. HD_Id'last;

  type HIP_Id is new Database.Stars.HIP_Id;

  subtype HIP is HIP_Id range First_Id .. HIP_Id'last;

  type HR_Id is new Database.Stars.HR_Id;

  subtype HR is HR_Id range First_Id .. HR_Id'last;

  function Star_Of (Id : HD) return Star_Id with Inline;

  function Star_Of (Id : HIP) return Star_Id with Inline;

  function Star_Of (Id : HR) return Star_Id with Inline;

  function Ra_J2000_Of (Id : Star) return Angle.Degrees with Inline;

  function Dec_J2000_Of (Id : Star) return Angle.Degrees with Inline;

  function Ra_Motion_Of (Id : Star) return Angle.Degrees with Inline;

  function Dec_Motion_Of (Id : Star) return Angle.Degrees with Inline;

  function V_Mag_Of (Id : Star) return Magnitude with Inline;

end Object.Catalog;
