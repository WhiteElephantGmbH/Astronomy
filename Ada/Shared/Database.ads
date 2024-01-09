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

package Database is

  Unknown_Id : constant := 0;

  NI : constant := Unknown_Id; -- no information

  type Name is access constant String;

  function "+" (Item : String) return Name is (new String'(Item));


  type Star_Type is (Alpha2_Cvn_Variable_Star,
                     Asymptotic_Giant_Branch_Star,
                     Be_Star,
                     Beta_Cep_Variable_Star,
                     Blue_Straggler,
                     Blue_Supergiant_Star,
                     BY_Dra_Variable_Star,
                     Carbon_Star,
                     Cataclysmic_Binary_Star,
                     Cepheid_Variable_Star,
                     Chemically_Peculiar_Star,
                     Classical_Cepheid_Variable_Star,
                     Classical_Nova_Star,
                     Composite_Object_Blend,
                     Delta_Sct_Variable_Star,
                     Double_Or_Multiple_Star,
                     Eclipsing_Binary_Star,
                     Ellipsoidal_Variable_Star,
                     Emission_Line_Star,
                     Eruptive_Variable_Star,
                     Evolved_Supergiant_Star,
                     Gamma_Dor_Variable_Star,
                     Herbig_Ae_Be_Star,
                     High_Mass_X_Ray_Binary_Star,
                     High_Proper_Motion_Star,
                     High_Velocity_Star,
                     Horizontal_Branch_Star,
                     Hot_Subdwarf_Star,
                     Irregular_Variable_Star,
                     Long_Period_Variable_Star,
                     Low_Mass_Star,
                     Low_Mass_X_Ray_Binary_Star,
                     Main_Sequence_Star,
                     Mira_Variable_Star,
                     OH_IR_Star,
                     Orion_Variable_Star,
                     Planetary_Nebula_Star,
                     Post_AGB_Star,
                     Pulsating_Variable_Star,
                     R_Crb_Variable_Star,
                     Red_Giant_Branch_Star,
                     Red_Supergiant_Star,
                     Rotating_Variable_Star,
                     RR_Lyrae_Variable_Star,
                     RS_Cvn_Variable_Star,
                     RV_Tauri_Variable_Star,
                     S_Star,
                     Spectroscopic_Binary_Star,
                     Star,
                     Supernova,
                     SX_Phe_Variable,
                     Symbiotic_Star,
                     T_Tauri_Star,
                     Type_II_Cepheid_Variable_Star,
                     Variable_Star,
                     White_Dwarf_Star,
                     Wolf_Rayet_Star,
                     X_Ray_Binary_Star,
                     Yellow_Supergiant_Star,
                     Young_Stellar_Object_Star);

  OT00 : constant Star_Type := Alpha2_Cvn_Variable_Star;
  OT01 : constant Star_Type := Asymptotic_Giant_Branch_Star;
  OT02 : constant Star_Type := Be_Star;
  OT03 : constant Star_Type := Beta_Cep_Variable_Star;
  OT04 : constant Star_Type := Blue_Straggler;
  OT05 : constant Star_Type := Blue_Supergiant_Star;
  OT06 : constant Star_Type := BY_Dra_Variable_Star;
  OT07 : constant Star_Type := Carbon_Star;
  OT08 : constant Star_Type := Cataclysmic_Binary_Star;
  OT09 : constant Star_Type := Cepheid_Variable_Star;
  OT10 : constant Star_Type := Chemically_Peculiar_Star;
  OT11 : constant Star_Type := Classical_Cepheid_Variable_Star;
  OT12 : constant Star_Type := Classical_Nova_Star;
  OT13 : constant Star_Type := Composite_Object_Blend;
  OT14 : constant Star_Type := Delta_Sct_Variable_Star;
  OT15 : constant Star_Type := Double_Or_Multiple_Star;
  OT16 : constant Star_Type := Eclipsing_Binary_Star;
  OT17 : constant Star_Type := Ellipsoidal_Variable_Star;
  OT18 : constant Star_Type := Emission_Line_Star;
  OT19 : constant Star_Type := Eruptive_Variable_Star;
  OT20 : constant Star_Type := Evolved_Supergiant_Star;
  OT21 : constant Star_Type := Gamma_Dor_Variable_Star;
  OT22 : constant Star_Type := Herbig_Ae_Be_Star;
  OT23 : constant Star_Type := High_Mass_X_Ray_Binary_Star;
  OT24 : constant Star_Type := High_Proper_Motion_Star;
  OT25 : constant Star_Type := High_Velocity_Star;
  OT26 : constant Star_Type := Horizontal_Branch_Star;
  OT27 : constant Star_Type := Hot_Subdwarf_Star;
  OT28 : constant Star_Type := Irregular_Variable_Star;
  OT29 : constant Star_Type := Long_Period_Variable_Star;
  OT30 : constant Star_Type := Low_Mass_Star;
  OT31 : constant Star_Type := Low_Mass_X_Ray_Binary_Star;
  OT32 : constant Star_Type := Main_Sequence_Star;
  OT33 : constant Star_Type := Mira_Variable_Star;
  OT34 : constant Star_Type := OH_IR_Star;
  OT35 : constant Star_Type := Orion_Variable_Star;
  OT36 : constant Star_Type := Planetary_Nebula_Star;
  OT37 : constant Star_Type := Post_AGB_Star;
  OT38 : constant Star_Type := Pulsating_Variable_Star;
  OT39 : constant Star_Type := R_Crb_Variable_Star;
  OT40 : constant Star_Type := Red_Giant_Branch_Star;
  OT41 : constant Star_Type := Red_Supergiant_Star;
  OT42 : constant Star_Type := Rotating_Variable_Star;
  OT43 : constant Star_Type := RR_Lyrae_Variable_Star;
  OT44 : constant Star_Type := RS_Cvn_Variable_Star;
  OT45 : constant Star_Type := RV_Tauri_Variable_Star;
  OT46 : constant Star_Type := S_Star;
  OT47 : constant Star_Type := Spectroscopic_Binary_Star;
  OT48 : constant Star_Type := Star;
  OT49 : constant Star_Type := Supernova;
  OT50 : constant Star_Type := SX_Phe_Variable;
  OT51 : constant Star_Type := Symbiotic_Star;
  OT52 : constant Star_Type := T_Tauri_Star;
  OT53 : constant Star_Type := Type_II_Cepheid_Variable_Star;
  OT54 : constant Star_Type := Variable_Star;
  OT55 : constant Star_Type := White_Dwarf_Star;
  OT56 : constant Star_Type := Wolf_Rayet_Star;
  OT57 : constant Star_Type := X_Ray_Binary_Star;
  OT58 : constant Star_Type := Yellow_Supergiant_Star;
  OT59 : constant Star_Type := Young_Stellar_Object_Star;

  Delta_Degrees   : constant := 0.0000000000001;
  Delta_Motion    : constant := 0.001;
  Delta_Parallax  : constant := 0.0001;
  Delta_Magnitude : constant := 0.001;

  type Degrees_Ra  is delta Delta_Degrees range    0.0 .. 360.0 - Delta_Degrees with Small => Delta_Degrees;
  type Degrees_Dec is delta Delta_Degrees range -180.0 .. 180.0 - Delta_Degrees with Small => Delta_Degrees;

  type Motion    is delta Delta_Motion    range -10000.0 .. 11000.0; -- Bernhard's star: 10362.394
  type Parallax  is delta Delta_Parallax  range      0.0 .. 1000.0 - Delta_Parallax  with Small => Delta_Parallax;
  type Magnitude is delta Delta_Magnitude range    -10.0 .. 100.0  - Delta_Magnitude with Small => Delta_Magnitude;

  subtype Star_Magnitude is Magnitude range -2.0 .. 14.0;

  Default_Star_Magnitude : Star_Magnitude := 8.0;

  No_Plx : constant := Parallax'first;
  No_Mag : constant := Magnitude'last;

  type Star_Class is (O, B, A, F, G, K, L, M, R, S, N, C, DB, DA, DF, DG, WR, WN, WC);

  type Star_Subclass is (S0, S1, S2, S3, S4, S5, S6, S7, S8, S9);

  type Star_Luminosity_Class is (NO, Ia0, Ia, Ib, Iab, II, III, IV, V);

  type Star_Spec_Type is record
    Class      : Star_Class;
    Subclass   : Star_Subclass;
    Luminosity : Star_Luminosity_Class;
  end record;

  Undefined_Spec_Type : constant Star_Spec_Type := (G, S0, NO);

  type Star_Information is record
    Otype     : Database.Star_Type;
    Ra_J2000  : Database.Degrees_Ra;
    Dec_J2000 : Database.Degrees_Dec;
    Ra_PM     : Database.Motion;
    Dec_PM    : Database.Motion;
    Plx       : Database.Parallax;
    Vmag      : Database.Magnitude;
    Stype     : Database.Star_Spec_Type;
  end record with Pack;

end Database;
