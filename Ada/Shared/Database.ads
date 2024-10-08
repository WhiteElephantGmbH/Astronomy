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

  Unknown : constant := 0;

  First : constant := Unknown + 1;

  NI : constant := Unknown; -- no information

  Id_Size : constant := 32;

  type Id is range 0 .. 2**Id_Size - 1 with Size => Id_Size;

  type Name is access constant String;

  function "+" (Item : String) return Name is (new String'(Item));

  type Greek_Letter is (
    Alf,  -- α
    Bet,  -- β
    Gam,  -- γ
    Del,  -- δ
    Eps,  -- ε
    Zet,  -- ζ
    Eta,  -- η
    Tet,  -- θ
    Iot,  -- ι
    Kap,  -- κ
    Lam,  -- λ
    Mu,   -- µ
    Nu,   -- ν
    Ksi,  -- ξ
    Omi,  -- o
    Pi,   -- π
    Rho,  -- ρ
    Sig,  -- σ
    Tau,  -- τ
    Ups,  -- υ
    Phi,  -- φ
    Chi,  -- χ
    Psi,  -- ψ
    Ome); -- ω

  Greek_Alphabet : constant array (Natural range <>) of String(1..2) :=
    ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "µ",
     "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"];

  type Constellation is (
    C_And, C_Ant, C_Aps, C_Aqr, C_Aql, C_Ara, C_Ari, C_Aur, C_Boo, C_Cae, C_Cam, C_Cnc, C_Cvn, C_Cma, C_Cmi, C_Cap,
    C_Car, C_Cas, C_Cen, C_Cep, C_Cet, C_Cha, C_Cir, C_Col, C_Com, C_Cra, C_Crb, C_Crv, C_Crt, C_Cru, C_Cyg, C_Del,
    C_Dor, C_Dra, C_Equ, C_Eri, C_For, C_Gem, C_Gru, C_Her, C_Hor, C_Hya, C_Hyi, C_Ind, C_Lac, C_Leo, C_Lep, C_Lib,
    C_Lmi, C_Lup, C_Lyn, C_Lyr, C_Men, C_Mic, C_Mon, C_Mus, C_Nor, C_Oct, C_Oph, C_Ori, C_Pav, C_Peg, C_Per, C_Phe,
    C_Pic, C_Psc, C_Psa, C_Pup, C_Pyx, C_Ret, C_Sge, C_Sgr, C_Sco, C_Scl, C_Sct, C_Ser, C_Sex, C_Tau, C_Tel, C_Tri,
    C_Tra, C_Tuc, C_Uma, C_Umi, C_Vel, C_Vir, C_Vol, C_Vul, C_LMC, C_SMC) with Size => 8;

  type Star_Count_Type is (None, Greek, Alphabetic, Numeric) with Size => 4;

  type Star_Number is range 0 .. 999 with Size => 12;

  type Star_Index is range 0 .. 99  with Size => 8;

  type Star_Info is record
    Kind  : Star_Count_Type := None;
    Count : Star_Number     := 0;
    Index : Star_Index      := 0;
    C_Id  : Constellation   := Constellation'first;
  end record
  with Pack, Size => Id_Size;

  Star_Image : constant String := "*";

  Unknown_Star_Info : constant Star_Info := (others => <>);

  function Star_Id_Of (Image : String) return Id;


  type Object_Type is (

    -- 1. STARS
    Alpha2_Cvn_Variable,
    Asymptotic_Giant_Branch,
    Brown_Dwarf,
    Be_Star,
    Beta_Cep_Variable,
    Blue_Straggler,
    Blue_Supergiant,
    BY_Dra_Variable,
    Carbon_Star,
    Cataclysmic_Binary,
    Cepheid_Variable,
    Chemically_Peculiar,
    Classical_Cepheid,
    Classical_Nova,
    Composite_Object_Blend,
    Delta_Sct_Variable,
    Double_Or_Multiple_Star,
    Eclipsing_Binary_Star,
    Ellipsoidal_Variable,
    Emission_Line_Star,
    Eruptive_Variable_Star,
    Evolved_Supergiant_Star,
    Extra_Solar_Planet,
    Gamma_Dor_Variable_Star,
    Herbig_Ae_Be_Star,
    High_Mass_X_Ray_Binary,
    High_Proper_Motion_Star,
    High_Velocity_Star,
    Horizontal_Branch_Star,
    Hot_Subdwarf_Star,
    Herbig_Haro_Object,
    Irregular_Variable,
    Long_Period_Variable,
    Low_Mass_Star,
    Low_Mass_X_Ray_Binary,
    Main_Sequence_Star,
    Mira_Variable_Star,
    OH_IR_Star,
    Orion_Variable_Star,
    Planetary_Nebula_Star,
    Post_AGB_Star,
    Pulsar,
    Pulsating_Variable_Star,
    R_Crb_Variable_Star,
    Red_Giant_Branch_Star,
    Red_Supergiant_Star,
    Rotating_Variable_Star,
    RR_Lyrae_Variable_Star,
    RS_Cvn_Variable_Star,
    RV_Tauri_Variable_Star,
    S_Star,
    Spectroscopic_Binary,
    Star,
    Supernova,
    SX_Phe_Variable_Star,
    Symbiotic_Star,
    T_Tauri_Star,
    Type_II_Cepheid_Variable,
    Variable_Star,
    White_Dwarf_Star,
    Wolf_Rayet_Star,
    X_Ray_Binary_Star,
    Yellow_Supergiant_Star,
    Young_Stellar_Object,

   -- 2. SETS OF STARS
    Association_Of_Stars,
    Cluster_Of_Stars,
    Globular_Cluster,
    Open_Cluster,
    Moving_Group,
    Stellar_Stream,

    -- 3. INTERSTELLAR MEDIUM
    Cloud,
    Cometary_Globule_Pillar,
    HII_Region,
    Interstellar_Medium,
    Interstellar_Shell,
    Molecular_Cloud,
    Nebula,
    Reflection_Nebula,
    Supernova_Remnant,

    -- 4. GALAXIES
    BL_Lac,
    Blazar,
    Blue_Compact_Galaxy,
    Brightest_Cluster_Galaxy,
    Emission_Line_Galaxy,
    Galaxy,
    Galaxy_In_A_Pair,
    Galaxy_Towards_Cluster,
    Galaxy_Towards_Group,
    Group_Of_Galaxies,
    Low_Surface_Galaxy,
    Linear_Active_Nucleus,
    Pair_Of_Galaxies,
    Quasar,
    Radio_Galaxy,
    Starburst_Galaxy,
    HII_Galaxy,
    Seyfert_Galaxy,
    Seyfert_1_Galaxy,
    Seyfert_2_Galaxy,

    -- 5. SETS OF GALAXIES
    Interacting_Galaxies,

    -- 7. GENERAL SPECTRAL PROPERTIES
    Emission_Object,
    Radio_Source,
    Transient_Event);

  subtype Interstellar  is Object_Type range Cloud .. Supernova_Remnant;
  subtype Galaxy_Object is Object_Type range BL_Lac .. Seyfert_2_Galaxy;
  subtype Star_Object   is Object_Type range Alpha2_Cvn_Variable .. Young_Stellar_Object;
  subtype Stars_Object  is Object_Type range Association_Of_Stars .. Stellar_Stream;

  subtype Visual_Limited_Star is Star_Object with Predicate => Visual_Limited_Star /= Planetary_Nebula_Star;

  type Otype_Id is range 0 .. Object_Type'pos(Object_Type'last);


  Delta_Degrees   : constant := 0.000_000_000_000_1;
  Delta_Motion    : constant := 0.001;
  Delta_Parallax  : constant := 0.000_1;
  Delta_Magnitude : constant := 0.001;

  type Degrees_Ra  is delta Delta_Degrees range    0.0 .. 360.0 - Delta_Degrees with Small => Delta_Degrees;
  type Degrees_Dec is delta Delta_Degrees range -180.0 .. 180.0 - Delta_Degrees with Small => Delta_Degrees;

  type Motion    is delta Delta_Motion    range -10_000.0 .. 11_000.0; -- Bernhard's star: 10362.394
  type Parallax  is delta Delta_Parallax  range       0.0 .. 1000.0 - Delta_Parallax  with Small => Delta_Parallax;
  type Magnitude is delta Delta_Magnitude range     -10.0 .. 100.0  - Delta_Magnitude with Small => Delta_Magnitude;

  type Light_Years is range 0 .. 2**32-1;

  One_Parsec_In_Light_Years : constant Float := 3.261_56;

  No_Plx : constant := Parallax'first;
  No_Mag : constant := Magnitude'last;

  No_Distance : constant := Light_Years'first;


  type Star_Class is
    (O, B, A, F, G, K, L, M, R, S, T, N, C, DA, DB, DC, DF, DG, D0, DQ, DZ, WC, WO, WN, WR); -- D0 ≡ DO

  type Star_Subclass is (S0, S1, S2, S3, S4, S5, S6, S7, S8, S9);

  type Star_Luminosity_Class is (NO, Ia0, Ia, Ib, Iab, II, III, IV, V);

  type Star_Spec_Type is record
    Class      : Star_Class;
    Subclass   : Star_Subclass;
    Luminosity : Star_Luminosity_Class;
  end record;

  No_Spec_Type : constant Star_Spec_Type := (Star_Class'first, Star_Subclass'first, NO);

  Delta_Angle : constant := 0.000_0001;
  Delta_Size  : constant := 0.001;

  type Feature_Latitude  is delta Delta_Angle range -90.0 .. 90.0 - Delta_Angle with Small => Delta_Angle; -- in degrees

  type Feature_Longitude is delta Delta_Angle range 0.0 .. 360.0 - Delta_Angle with Small => Delta_Angle; -- in degrees

  type Feature_Size is delta Delta_Size range 0.0 .. 3000.0 - Delta_Size with Small => Delta_Size; -- in kilometers

  type Moon_Feature_Type is (Catena,       -- Mondkraterkette
                             Crater,       -- Mondkrater
                             Lacus,        -- Mondsee
                             Mare,         -- Mondmeer
                             Mons,         -- Mondberg
                             Oceanus,      -- Mondozean
                             Palus,        -- Mondsumpf
                             Promontorium, -- Mondvorgebirge
                             Rima,         -- Mondrille
                             Rupes,        -- Mondklippen
                             Sinus,        -- Mondbucht
                             Swirl,        -- Mondwirbel
                             Vallis);      -- Mondtal

  Minimum_Feature_Size : constant Feature_Size := 10.0;

end Database;
