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

with Database.Objects;
with Sky;

package Simbad is

  package DB renames Database;

  Unknown : constant := DB.Unknown;

  First : constant := DB.First;

  subtype Number is DB.Objects.Number;

  subtype Index is DB.Objects.Index;

  subtype Object_Type is DB.Object_Type;

  subtype Spectral     is Object_Type range DB.Emission_Object .. DB.Transient_Event;
  subtype Galaxy       is Object_Type range DB.BL_Lac .. DB.Interacting_Galaxies;

  subtype Nebula is Object_Type with Static_Predicate
    => Nebula in DB.Cloud .. DB.Supernova_Remnant | DB.Planetary_Nebula_Star;

  subtype Star is Object_Type with Static_Predicate
    => Star in DB.Alpha2_Cvn_Variable .. DB.Delta_Sct_Variable |
               DB.Eclipsing_Binary_Star .. DB.Orion_Variable_Star |
               DB.Post_AGB_Star .. DB.Young_Stellar_Object;

  Multiple_Star : constant Object_Type := DB.Double_Or_Multiple_Star;

  subtype Stars is Object_Type with Static_Predicate
    => Stars in DB.Association_Of_Stars .. DB.Cluster_Of_Stars | DB.Open_Cluster .. DB.Stellar_Stream;

  Globular_Cluster : constant Object_Type := DB.Globular_Cluster;


  subtype Parallax is Database.Parallax;

  subtype Light_Years is Database.Light_Years;

  subtype Spectral_Type is Database.Star_Spec_Type;

  subtype Spectral_Class is Database.Star_Class;

  subtype Magnitude is Sky.Magnitude;

end Simbad;
