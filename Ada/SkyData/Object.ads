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

with Strings;

package Object is

  type Id is new Natural;

  Unknown_Id : constant Id := 0;

  Undefined : constant Character := '~';

  type Kind is (Alpha2_Cvn_Variable_Star,
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

  Delta_Degrees   : constant := 0.0000000000001;
  Delta_Motion    : constant := 0.001;
  Delta_Parallax  : constant := 0.0001;
  Delta_Magnitude : constant := 0.001;

  type Degrees_Ra  is delta Delta_Degrees range    0.0 .. 360.0 - Delta_Degrees with Small => Delta_Degrees;
  type Degrees_Dec is delta Delta_Degrees range -180.0 .. 180.0 - Delta_Degrees with Small => Delta_Degrees;

  type Motion    is delta Delta_Motion    range -10000.0 .. 11000.0; -- Bernhard's star: 10362.394
  type Parallax  is delta Delta_Parallax  range      0.0 .. 1000.0 - Delta_Parallax  with Small => Delta_Parallax;
  type Magnitude is delta Delta_Magnitude range    -10.0 .. 100.0  - Delta_Magnitude with Small => Delta_Magnitude;

  subtype Visual_Magnitude is Magnitude range -2.0 .. 10.0;

  Undefined_Parallax  : constant := Parallax'first;
  Undefined_Magnitude : constant := Magnitude'last;

  type Spec_Kind is (O, B, A, F, G, K, M, R, S, N, C, DB, DA, DF, DG, WR, WN, WC);

  type Sub_Type is (None, S1, S2, S3, S4, S5, S6, S7, S8, S9);

  type Luminosity_Class is (None, Ia0, Ia, Ib, Iab, II, III, IV, V);

  type Spec_Type is record
    Kind     : Spec_Kind;
    Sub_Kind : Sub_Type;
    Class    : Luminosity_Class;
  end record;

  type Star_Information is record
    Name      : Strings.Element;
    Hip_Id    : Id := Unknown_Id;
    Hr_Id     : Id := Unknown_Id;
    Hd_Id     : Id := Unknown_Id;
    Otype     : Kind;
    Ra_J2000  : Degrees_Ra;
    Dec_J2000 : Degrees_Dec;
    Ra_Pm     : Motion;
    Dec_Pm    : Motion;
    Plx       : Parallax;
    Umag      : Magnitude;
    Bmag      : Magnitude;
    Vmag      : Magnitude;
    Rmag      : Magnitude;
    Imag      : Magnitude;
    Stype     : Spec_Type;
  end record;

  function "=" (Left, Right :Star_Information) return Boolean is
    ((Left.Hip_Id = Right.Hip_Id) and (Left.Hd_Id = Right.Hd_Id) and (Left.Hr_Id = Right.Hr_Id));

end Object;
