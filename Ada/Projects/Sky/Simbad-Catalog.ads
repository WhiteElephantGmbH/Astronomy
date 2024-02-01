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
with Lexicon;

package Simbad.Catalog is

  No_Image : exception;

  type Id is (Star_Id, Name_Id, HD, HIP, HR, M, NGC, IC, Ocl);

  function Main_Id_Of (Item : Index) return Id with Inline;

  function Image_Of (Item : Index) return String;
  -- returns objects main catalog image


  function Index_Of (Item : Lexicon.Word) return Number with Inline;

  function Has_Name (Item : Index) return Boolean with Inline;

  function Name_Of (Item : Index) return Lexicon.Word with Inline;

  function Name_Image_Of (Item : Index) return String;
  -- exception : No_Image


  subtype Star_Info is Database.Star_Info;

  function Index_Of (Item : Star_Info) return Number with Inline;

  function Has_Star_Info (Item : Index) return Boolean with Inline;

  function Star_Info_Of (Item : Index) return Star_Info with Inline;

  function Star_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type HD_Number is new Database.Objects.HD_Id;

  subtype HD_Index is HD_Number range First .. HD_Number'last;

  function Number_Of (Item : HD_Index) return Number with Inline;

  function HD_Of (Item : Index) return HD_Number with Inline;

  function HD_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type HIP_Number is new Database.Objects.HIP_Id;

  subtype HIP_Index is HIP_Number range First .. HIP_Number'last;

  function Number_Of (Item : HIP_Index) return Number with Inline;

  function HIP_Of (Item : Index) return HIP_Number with Inline;

  function HIP_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type HR_Number is new Database.Objects.HR_Id;

  subtype HR_Index is HR_Number range First .. HR_Number'last;

  function Number_Of (Item : HR_Index) return Number with Inline;

  function HR_Of (Item : Index) return HR_Number with Inline;

  function HR_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type M_Number is new Database.Objects.M_Id;

  subtype M_Index is M_Number range First .. M_Number'last;

  function Number_Of (Item : M_Index) return Number with Inline;

  function M_Of (Item : Index) return M_Number with Inline;

  function M_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type NGC_Number is new Database.Objects.NGC_Id;

  subtype NGC_Index is NGC_Number range First .. NGC_Number'last;

  function Number_Of (Item : NGC_Index) return Number with Inline;

  function NGC_Of (Item : Index) return NGC_Number with Inline;

  function NGC_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type IC_Number is new Database.Objects.IC_Id;

  subtype IC_Index is IC_Number range First .. IC_Number'last;

  function Number_Of (Item : IC_Index) return Number with Inline;

  function IC_Of (Item : Index) return IC_Number with Inline;

  function IC_Image_Of (Item : Index) return String;
  -- exception : No_Image


  type OCL_Number is new Database.Objects.OCL_Id;

  subtype OCL_Index is OCL_Number range First .. OCL_Number'last;

  function Number_Of (Item : OCL_Index) return Number with Inline;

  function OCL_Of (Item : Index) return OCL_Number with Inline;

  function OCL_Image_Of (Item : Index) return String;
  -- exception : No_Image


  function Ra_J2000_Of (Item : Index) return Angle.Degrees with Inline;

  function Dec_J2000_Of (Item : Index) return Angle.Degrees with Inline;

  function Ra_Motion_Of (Item : Index) return Angle.Degrees with Inline;

  function Dec_Motion_Of (Item : Index) return Angle.Degrees with Inline;

  function Magnitude_Of (Item : Index) return Magnitude with Inline;

  function Distance_Of (Item : Index) return Light_Years with Inline;

  function Parallax_Of (Item : Index) return Parallax with Inline;

  function Spec_Type_Of (Item : Index) return Spectral_Type with Inline;

  function Spec_Class_Of (Item : Number) return Spectral_Class is (Spec_Type_Of (Item).Class);

  function Object_Type_Of (Item : Index) return Object_Type with Inline;

end Simbad.Catalog;
