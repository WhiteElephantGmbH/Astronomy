-- *********************************************************************************************************************
-- *                           (c) 2024  by White Elephant GmbH, Schaffhausen, Switzerland                             *
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Database;
with Strings;
with Traces;

package body Generator.Data is

  package Log is new Traces ("Generator.Data");

  Undefined : constant String := "";

  Skip_Object : exception;


  procedure Warning (Message : String) is
  begin
    Log.Warning (Message & " in Line" & The_Line_Number'image);
  end Warning;


  procedure Warning_And_Skip (Message : String) with No_Return is
  begin
    Warning ("skip - " & Message);
    raise Skip_Object;
  end Warning_And_Skip;


  subtype Object_Type is Database.Object_Type;

  function Otype_Of (Item : String) return Object_Type is
  -----------------------1. TAXONOMY OF STARS------------------------------
  -- * Star
  --    Ma*         Ma?   Massive Star
  --     bC*       bC?    beta Cep Variable
  --     sg*       sg?    Evolved Supergiant
  --       s*r     s?r    Red Supergiant
  --       s*y     s?y    Yellow Supergiant
  --       s*b     s?b    Blue Supergiant
  --         WR*   WR?    Wolf-Rayet
  --         ..2          LBV=Luminous Blue Variable
  --     N*        N*?    Neutron Star
  --       Psr            Pulsar
  --.....................Young Stellar Objects (Pre-Main Sequence Stars)...
  --   ..1                {pr*} Pre-Main Sequence Star
  --   Y*O         Y*?    Young Stellar Object
  --     or*              Orion Variable
  --       ..3            {FU*} FU Ori Variable
  --     TT*       TT?    T Tauri Star
  --     Ae*       Ae?    Herbig Ae/Be Star
  --     out       of?    Outflow
  --       HH             Herbig-Haro Object
  --.....................Main Sequence Stars...............................
  --   MS*         MS?    Main Sequence Star
  --     Be*       Be?    Be Star
  --     BS*       BS?    Blue Straggler
  --       SX*            SX Phe Variable
  --     gD*              gamma Dor Variable
  --     dS*              delta Sct Variable
  --.....................Evolved Stars.....................................
  --   Ev*         Ev?    Evolved Star
  --     RG*       RB?    Red Giant Branch star
  --     HS*       HS?    Hot Subdwarf
  --     HB*       HB?    Horizontal Branch Star
  --       RR*     RR?    RR Lyrae Variable
  --       ..4            Red Clump Star
  --     WV*       WV?    type II Cepheid Variable
  --     Ce*       Ce?    Cepheid Variable
  --       cC*            Classical Cepheid Variable
  --     C*        C*?    Carbon Star
  --     S*        S*?    S Star
  --     LP*       LP?    Long-Period Variable
  --       ..5            {sr*} Semi-Regular Variable
  --     AB*       AB?    Asymptotic Giant Branch Star
  --       Mi*     Mi?    Mira Variable
  --       ..6            O-rich AGB Star
  --     OH*       OH?    OH/IR Star
  --     pA*       pA?    Post-AGB Star
  --     RV*       RV?    RV Tauri Variable
  --     PN        PN?    Planetary Nebula
  --     WD*       WD?    White Dwarf
  --       ..7            {ZZ*} Pulsating White Dwarf
  --       ..8            ELMWD=Extremely Low Mass White Dwarf
  --.....................Chemically Peculiar Stars.........................
  --   Pe*         Pe?    Chemically Peculiar Star
  --     a2*       a2?    alpha2 CVn Variable
  --     RC*       RC?    R CrB Variable
  --     ..9              CH Star
  --     ..10             Barium Star
  --     ..11             Dwarf Carbon Star
  --     ..12             Carbon-Enhanced Metal Poor Star
  --.....................Interacting Binaries and close Common Proper Motio
  --   **          **?    Double or Multiple Star
  --     El*       El?    Ellipsoidal Variable
  --     EB*       EB?    Eclipsing Binary
  --       ..13           {Al*} Eclipsing Binary of Algol type
  --       ..14           {bL*}Eclipsing Binary of beta Lyr type
  --       ..15           {WU*} Eclipsing Binary of W UMa type
  --     SB*       SB?    Spectroscopic Binary
  --     BY*       BY?    BY Dra Variable
  --     RS*       RS?    RS CVn Variable
  --     Sy*       Sy?    Symbiotic Star
  --     XB*       XB?    X-ray Binary
  --       LXB     LX?    Low Mass X-ray Binary
  --       HXB     HX?    High Mass X-ray Binary
  --     CV*       CV?    Cataclysmic Binary
  --       No*     No?    Classical Nova
  --         ..16         {NL*} Nova-like Binary
  --         ..17         {DN*} Dwarf Nova
  --         ..18         {DQ*} CV of DQ Her type  Intermediate polar.
  --         ..19         {AM*} CV of AM CVn type
  --.....................SuperNovae........................................
  --   SN*         SN?    SuperNova
  --.....................Low mass Stars and substellar Objects.............
  --   LM*         LM?    Low-mass Star
  --     BD*       BD?    Brown Dwarf
  --   Pl          Pl?    Extra-solar Planet
  --.....................Properties . variability. spectral. kinematic
  --   V*          V*?    Variable Star
  --     Ir*              Irregular Variable
  --       ..20           Irregular Variable with rapid variations
  --     Er*       Er?    Eruptive Variable
  --       ..21           {Fl*} Flare Star
  --     Ro*       Ro?    Rotating Variable
  --     Pu*       Pu?    Pulsating Variable
  --     ..22             Star showing Eclipses by its Planet
  --.....................Spectral properties...............................
  --   Em*                Emission-line Star
  --.....................Kinematic and Environment Properties..............
  --   PM*                High Proper Motion Star
  --   HV*                High Velocity Star
  --   ..23               {*iC} Star towards a Cluster
  --   ..24               {*iA} Star towards an Association
  --   ..25               {*in} Star towards a Nebula
  --   ..26               {*i*} Star in double system
  --
  -----------------------2. SETS OF STARS----------------------------------
  -- Cl*           Cl?    Cluster of Stars
  --   GlC         Gl?    Globular Cluster
  --   OpC                Open Cluster
  -- As*           As?    Association of Stars
  --   St*                Stellar Stream
  --   MGr                Moving Group
  --
  -----------------------3. INTERSTELLAR MEDIUM----------------------------
  -- ISM                  Interstellar Medium Object
  --   SFR                Star Forming Region
  --   HII                HII Region
  --   Cld                Cloud
  --     GNe              Nebula
  --       RNe            Reflection Nebula
  --     MoC              Molecular Cloud
  --     DNe              Dark Cloud (nebula)
  --     glb              Globule (low-mass dark cloud)
  --     CGb              Cometary Globule / Pillar
  --     HVC              High-velocity Cloud
  --   cor                Dense Core
  --   bub                Bubble
  --   SNR         SR?    SuperNova Remnant
  --   sh                 Interstellar Shell
  --   flt                Interstellar Filament
  --
  -----------------------4. TAXONOMY OF GALAXIES---------------------------
  -- G             G?     Galaxy
  --   LSB                Low Surface Brightness Galaxy
  --   bCG                Blue Compact Galaxy
  --   SBG                Starburst Galaxy
  --   H2G                HII Galaxy
  --   EmG                Emission-line galaxy
  --   AGN         AG?    Active Galaxy Nucleus
  --     SyG              Seyfert Galaxy
  --       Sy1            Seyfert 1 Galaxy
  --       Sy2            Seyfert 2 Galaxy
  --     rG               Radio Galaxy
  --     LIN              LINER-type Active Galaxy Nucleus
  --     QSO       Q?     Quasar
  --       Bla     Bz?    Blazar
  --         BLL   BL?    BL Lac
  --   ..28               {HzG} Galaxy with high redshift
  --   ..29               {ERO} ERO/VRO, Extremely/Very Red Object
  --   ..30               ULIRG, Ultra Luninous Infrared Galaxy
  --   ..31               {LyA, DLA, mAL, LLS, BAL} Absorption Line System
  --.....................Environment properties............................
  --   GiP                Galaxy in Pair of Galaxies
  --   GiG                Galaxy towards a Group of Galaxies
  --   GiC                Galaxy towards a Cluster of Galaxies
  --     BiC              Brightest Galaxy in a Cluster (BCG)
  --
  -----------------------5. SETS OF GALAXIES-------------------------------
  -- IG                   Interacting Galaxies
  -- PaG                  Pair of Galaxies
  -- GrG           Gr?    Group of Galaxies
  --   CGG                Compact Group of Galaxies
  -- ClG           C?G    Cluster of Galaxies
  -- PCG           PCG?   Proto Cluster of Galaxies
  -- SCG           SC?    Supercluster of Galaxies
  -- vid                  Underdense Region of the Universe
  --
  -----------------------6. GRAVITATION------------------------------------
  -- grv                  Gravitational Source
  --   Lev                (Micro)Lensing Event
  --   gLS         LS?    Gravitational Lens System (lens+images)
  --     gLe       Le?    Gravitational Lens
  --     LeI       LI?    Gravitationally Lensed Image
  --       LeG            Gravitationally Lensed Image of a Galaxy
  --       LeQ            Gravitationally Lensed Image of a Quasar
  --   BH          BH?    Black Hole
  --   GWE                Gravitational Wave Event
  --
  -----------------------7. GENERAL SPECTRAL PROPERTIES--------------------
  -- ev                   Transient Event
  -- Rad                  Radio Source
  --   mR                 Metric Radio Source
  --   cm                 Centimetric Radio Source
  --   mm                 Millimetric Radio Source
  --   smm                Sub-Millimetric Source
  --   HI                 HI (21cm) Source
  --   rB                 Radio Burst
  --   Mas                Maser
  -- IR                   Infra-Red Source
  --   FIR                Far-IR source (λ >= 30 µm)
  --   MIR                Mid-IR Source (3 to 30 µm)
  --   NIR                Near-IR Source (λ < 3 µm)
  --   ..32               {red} Very Red Source
  -- Opt                  Optical Source
  --   EmO                Emission Object
  --   blu                Blue Object
  -- UV                   UV-emission Source
  -- X                    X-ray Source
  --   ULX         UX?    Ultra-luminous X-ray Source
  -- gam                  Gamma-ray Source
  --   gB                 Gamma-ray Burst
  --
  -----------------------8. BLENDS, ERRORS, NOT WELL DEFINED OBJECTS-------
  -- mul                  Composite Object, Blend
  -- err                  Not an Object (Error, Artefact, ...)
  -- PoC                  Part of Cloud
  -- PoG                  Part of a Galaxy
  -- ?                    Object of Unknown Nature
  -- reg                  Region defined in the Sky

    use all type Object_Type;

  begin -- Otype_Of
    if Item = Undefined then
      Warning_And_Skip ("Undefined Otype");

    -- 1. STARS
    elsif Item in "a2*" | "a2?" then
      return Alpha2_Cvn_Variable_Star;
    elsif Item in "AB*" | "AB?" then
      return Asymptotic_Giant_Branch_Star;
    elsif Item in "Be*" | "Be?" then
      return Be_Star;
    elsif Item in "bC*" | "bC?" then
      return Beta_Cep_Variable_Star;
    elsif Item in "BS*" | "BS?" then
      return Blue_Straggler_Star;
    elsif Item = "BY*" then
      return BY_Dra_Variable_Star;
    elsif Item in "s*b" | "s?b" then
      return Blue_Supergiant_Star;
    elsif Item in "BD*" | "BD?" then
      return Brown_Dwarf_Star;
    elsif Item in "C*" | "C*?" then
      return Carbon_Star;
    elsif Item = "CV*" then
      return Cataclysmic_Binary_Star;
    elsif Item in "Ce*" | "Ce?" then
      return Cepheid_Variable_Star;
    elsif Item = "Pe*" then
      return Chemically_Peculiar_Star;
    elsif Item = "cC*" then
      return Classical_Cepheid_Variable_Star;
    elsif Item = "No*" then
      return Classical_Nova_Star;
    elsif Item = "dS*" then
      return Delta_Sct_Variable_Star;
    elsif Item = "**" then
      return Double_Or_Multiple_Star;
    elsif Item in "EB*" | "EB?" then
      return Eclipsing_Binary_Star;
    elsif Item = "El*" then
      return Ellipsoidal_Variable_Star;
    elsif Item = "Em*" then
      return Emission_Line_Star;
    elsif Item = "Er*" then
      return Eruptive_Variable_Star;
    elsif Item in "sg*" | "sg?" then
      return Evolved_Supergiant_Star;
    elsif Item in "Pl" | "Pl?" then
      return Extra_Solar_Planet;
    elsif Item = "gD*" then
      return Gamma_Dor_Variable_Star;
    elsif Item in "Ae*" | "Ae?" then
      return Herbig_Ae_Be_Star;
    elsif Item = "HXB" then
      return High_Mass_X_Ray_Binary_Star;
    elsif Item = "PM*" then
      return High_Proper_Motion_Star;
    elsif Item = "HV*" then
      return High_Velocity_Star;
    elsif Item = "HB*" then
      return Horizontal_Branch_Star;
    elsif Item in "HS*" | "HS?" then
      return Hot_Subdwarf_Star;
    elsif Item = "HH" then
      return Herbig_Haro_Object;
    elsif Item = "Ir*" then
      return Irregular_Variable_Star;
    elsif Item in "LP*" | "LP?" then
      return Long_Period_Variable_Star;
    elsif Item = "LM*" then
      return Low_Mass_Star;
    elsif Item = "LXB" then
      return Low_Mass_X_Ray_Binary_Star;
    elsif Item in "MS*" | "MS?" then
      return Main_Sequence_Star;
    elsif Item = "Mi*" then
      return Mira_Variable_Star;
    elsif Item = "mul" then
      return Composite_Object_Blend;
    elsif Item = "OH*" then
      return OH_IR_Star;
    elsif Item = "Or*" then
      return Orion_Variable_Star;
    elsif Item in "PN" | "PN?" then
      return Planetary_Nebula_Star;
    elsif Item in "pA*" | "pA?" then
      return Post_AGB_Star;
    elsif Item = "Psr" then
      return Pulsar;
    elsif Item = "Pu*" then
      return Pulsating_Variable_Star;
    elsif Item in "RC*" | "RC?" then
      return R_Crb_Variable_Star;
    elsif Item in "RG*" | "RB?" then
      return Red_Giant_Branch_Star;
    elsif Item in "s*r" | "s?r" then
      return Red_Supergiant_Star;
    elsif Item = "Ro*" then
      return Rotating_Variable_Star;
    elsif Item in "RR*" | "RR?" then
      return RR_Lyrae_Variable_Star;
    elsif Item = "RS*" then
      return RS_Cvn_Variable_Star;
    elsif Item = "RV*" then
      return RV_Tauri_Variable_Star;
    elsif Item in "S*" | "S*?" then
      return S_Star;
    elsif Item in "SB*" | "SB?" then
      return Spectroscopic_Binary_Star;
    elsif Item = "*" then
      return Star;
    elsif Item in "SN*" | "SN?" then
      return Supernova;
    elsif Item = "SX*" then
      return SX_Phe_Variable_Star;
    elsif Item = "Sy*" then
      return Symbiotic_Star;
    elsif Item = "TT*" then
      return T_Tauri_Star;
    elsif Item = "WV*" then
      return Type_II_Cepheid_Variable_Star;
    elsif Item in "V*" | "V*?" then
      return Variable_Star;
    elsif Item = "WD*" then
      return White_Dwarf_Star;
    elsif Item = "WR*" then
      return Wolf_Rayet_Star;
    elsif Item = "XB*" then
      return X_Ray_Binary_Star;
    elsif Item in "s*y" | "s?y" then
      return Yellow_Supergiant_Star;
    elsif Item in "Y*O" | "Y*?" then
      return Young_Stellar_Object_Star;

  -- 2. SETS OF STARS
    elsif Item in "As*" | "As?" then
      return Association_Of_Stars;
    elsif Item in "Cl*" | "Cl?" then
      return Cluster_Of_Stars;
    elsif Item in "GlC" | "Gl?" then
      return Globular_Cluster;
    elsif Item = "OpC" then
      return Open_Cluster;
    elsif Item = "MGr" then
      return Moving_Group;
    elsif Item = "St*" then
      return Stellar_Stream;

    -- 3. INTERSTELLAR MEDIUM
    elsif Item = "Cld" then
      return Cloud;
    elsif Item = "CGb" then
      return Cometary_Globule_Pillar;
    elsif Item = "HII" then
      return HII_Region;
    elsif Item = "ISM" then
      return Interstellar_Medium_Object;
    elsif Item = "sh" then
      return Interstellar_Shell;
    elsif Item = "MoC" then
      return Molecular_Cloud;
    elsif Item = "GNe" then
      return Nebula;
    elsif Item = "RNe" then
      return Reflection_Nebula;
    elsif Item = "SNR" then
      return Supernova_Remnant;

    -- 4. GALAXIES
    elsif Item in "AGN" | "AG?" then
      return Galaxy_Towards_A_Group_Of_Galaxies;
    elsif Item in "BLL" | "BL?" then
      return BL_Lac;
    elsif Item in "Bla" | "Bz?" then
      return Blazar;
    elsif Item = "bCG" then
      return Blue_Compact_Galaxy;
    elsif Item = "BiC" then
      return Brightest_Galaxy_In_A_Cluster;
    elsif Item = "EmG" then
      return Emission_Line_Galaxy;
    elsif Item in "G" | "G?" then
      return Galaxy;
    elsif Item = "GiP" then
      return Galaxy_In_Pair_Of_Galaxies;
    elsif Item = "GiC" then
      return Galaxy_Towards_A_Cluster_Of_Galaxies;
    elsif Item = "GiG" then
      return Galaxy_Towards_A_Group_Of_Galaxies;
    elsif Item = "H2G" then
      return HII_Galaxy;
    elsif Item = "LSB" then
      return Low_Surface_Brightness_Galaxy;
    elsif Item = "LIN" then
      return LINER_Type_Active_Galaxy_Nucleus;
    elsif Item = "PaG" then
      return Pair_Of_Galaxies;
    elsif Item in "QSO" | "Q?"then
      return Quasar;
    elsif Item = "rG" then
      return Radio_Galaxy;
    elsif Item = "SBG" then
      return Starburst_Galaxy;
    elsif Item = "SyG" then
      return Seyfert_Galaxy;
    elsif Item = "Sy1" then
      return Seyfert_1_Galaxy;
    elsif Item = "Sy2" then
      return Seyfert_2_Galaxy;

    -- 5. SETS OF GALAXIES
    elsif Item = "IG" then
      return Interacting_Galaxies;
    elsif Item = "GrG" then
      return Group_Of_Galaxies;
    elsif Item = "PaG" then
      return Pair_Of_Galaxies;

    -- 7. GENERAL SPECTRAL PROPERTIES
    elsif Item = "EmO" then
      return Emission_Object;
    elsif Item = "Rad" then
      return Radio_Source;
    elsif Item = "ev" then
      return Transien_Event;

    -- 8. BLENDS, ERRORS, NOT WELL DEFINED OBJECTS
    elsif Item in "err" | "PoC" | "PoG" | "reg" | "?" then
      Warning_And_Skip ("Otype: " & Item);
      return Galaxy_Towards_A_Group_Of_Galaxies;
    else
      Warning_And_Skip ("Unknown Otype " & Item);
    end if;
  end Otype_Of;


  function Stype_Of (Item : String) return Database.Star_Spec_Type is

  -- O, B, A, F, G, K, M plus sub-type (0, 1, etc), and sometimes intermediate sub-type (for example F7.2, F7.5, F7.7):
  --   for the spectral types of `normal stars';

  -- R, S, N, C
  --   for carbon stars;

  -- DA, DB, DC, DF, DG:
  --   for white dwarfs;

  -- WR, WN, WC, WO:
  --   for Wolf-Rayet stars;

  -- For luminosity class, the following designations are used: Ia0, Ia, Iab, Ib for supergiants,
  -- II for bright giants, III for giants, IV for sub-giants, V for dwarfs.
  -- The sub-dwarfs are either noted sd followed by the spectral type, or class VI.

    Image : constant String := Item & "               "; -- no index errors
    Index : Natural := Image'first;

    The_Class            : Database.Star_Class;
    The_Subclass         : Database.Star_Subclass;
    The_Luminosity_Class : Database.Star_Luminosity_Class;

    use all type Database.Star_Class;
    use all type Database.Star_Subclass;
    use all type Database.Star_Luminosity_Class;

  begin -- Stype_Of
    if Item in Undefined | "-" then
      return Database.No_Spec_Type;
    elsif Item in "sd" | "wels" | "wels?" then
      return Database.No_Spec_Type; -- no spec type for Otype PN
    end if;
    while not
      (Image(Index) in 'O' | 'B' | 'A' | 'F' | 'G' | 'K' | 'L' | 'M' | 'R' | 'S' | 'T' | 'N' | 'C' | 'D' | 'W' | ' ')
    loop
      Index := @ + 1;
    end loop;
    case Image(Index) is
    when 'O' =>
      The_Class := O;
    when 'B' =>
      The_Class := B;
    when 'A' =>
      The_Class := A;
    when 'F' =>
      The_Class := F;
    when 'G' =>
      The_Class := G;
    when 'K' =>
      The_Class := K;
    when 'L' =>
      The_Class := L;
    when 'M' =>
      The_Class := M;
    when 'R' =>
      The_Class := R;
    when 'S' =>
      The_Class := S;
    when 'T' =>
      The_Class := T;
    when 'N' =>
      The_Class := N;
    when 'C' =>
      The_Class := C;
    when 'D' =>
      Index := @ + 1;
      case Image(Index) is
      when 'A' =>
        The_Class := DA;
      when 'B' =>
        The_Class := DB;
      when 'C' =>
        The_Class := DC;
      when 'F' =>
        The_Class := DF;
      when 'G' =>
        The_Class := DG;
      when 'O' =>
        The_Class := D0; -- DO
      when 'Q' =>
        The_Class := DQ;
      when 'Z' =>
        The_Class := DZ;
      when others =>
        Error ("Unknown White Dwarf type: " & Image(Index - 1.. Index));
      end case;
    when 'W' =>
      Index := @ + 1;
      case Image(Index) is
      when 'R' =>
        The_Class := WR;
      when 'N' =>
        The_Class := WN;
      when 'C' =>
        The_Class := WC;
      when 'O' =>
        The_Class := WO;
      when others =>
        Error ("Unknown Wolf-Rayet type: " & Image(Index - 1 .. Index));
      end case;
    when others =>
      Error ("Unknown Spec Type " & Image(Index));
    end case;
    Index := @ + 1;
    if Image(Index) in '1' .. '9' then
      The_Subclass := Database.Star_Subclass'val(Character'pos(Image(Index)) - Character'pos('0'));
      Index := @ + 1;
    else
      The_Subclass := S0;
    end if;
    while not (Image(Index) in 'I' | 'V' | ' ') loop
      Index := @ + 1;
    end loop;
    case Image(Index) is
    when 'I' =>
      Index := @ + 1;
      case Image(Index) is
      when 'a' =>
        Index := @ + 1;
        case Image(Index) is
        when '0' =>
          The_Luminosity_Class := Ia0;
        when 'b' =>
          The_Luminosity_Class := Iab;
        when others =>
          Index := @ - 1;
          The_Luminosity_Class := Ia;
        end case;
      when 'b' =>
        The_Luminosity_Class := Ib;
      when ' ' | ':' | '+' | '/'  | '(' | ')' |'e' | 'f' | '-' | '[' =>
        -- I | I: | I+ | I/II | I(n) | (I) | Ie | If | I-II(I) | I[e] -> II
        The_Luminosity_Class := II;
      when '?' =>
        The_Luminosity_Class := NO;
      when 'I' => -- II
        Index := @ + 1;
        case Image(Index) is
        when 'I' => -- III
          The_Luminosity_Class := III;
        when others =>
          Index := @ - 1;
          The_Luminosity_Class := II;
        end case;
      when 'V' =>
        The_Luminosity_Class := IV;
      when others =>
        Error ("Incorrect Spec Type: " & Image(Index - 1 .. Index));
      end case;
    when 'V' =>
      The_Luminosity_Class := V;
    when others =>
      The_Luminosity_Class := NO;
    end case;
    return (Class      => The_Class,
            Subclass   => The_Subclass,
            Luminosity => The_Luminosity_Class);
  end Stype_Of;


  function Removed_The_Of (Item : String) return String is
  begin
    if Item'length > 4 and then Item(Item'first.. Item'first + 3) = "the " then
      return Item(Item'first + 4 .. Item'last);
    else
      return Item;
    end if;
  end Removed_The_Of;


  function Base_Name_Of (Item : String) return String is

    Name_Image : constant String := Removed_The_Of (Item);
    Base_Image : String(1 .. Name_Image'length);
    The_Index  : Natural := Base_Image'first - 1;

    procedure Append (The_Character : Character) is
    begin
      The_Index := @ + 1;
      Base_Image(The_Index) := The_Character;
    end Append;

    Is_First  : Boolean := True;

  begin -- Base_Name_Of
    if Item = Undefined then
      return Item;
    elsif Strings.Is_Lowercase (Name_Image(Name_Image'first)) then
      Log.Warning ("Not a proper Name: " & Item);
      return Undefined;
    end if;
    for The_Character of Name_Image loop
      case Strings.Lowercase_Of (The_Character) is
      when '0'..'9' =>
        Append (The_Character);
        if The_Index /= Item'last then
          return Undefined; -- no number in the middle of a name
        end if;
      when 'a'..'z' =>
        if Is_First then
          Append (Strings.Uppercase_Of (The_Character));
          Is_First := False;
        else
          Append (Strings.Lowercase_Of (The_Character));
        end if;
      when ' ' | ''' | '_'=>
        if not Is_First then
          Append ('_');
          Is_First := True;
        end if;
      when others =>
        return Undefined;
      end case;
    end loop;
    return Base_Image(Base_Image'first .. The_Index);
  end Base_Name_Of;


  type Id is new Natural;

  Unknown_Id : constant := Database.Unknown_Id;
  First_Id   : constant Id := Unknown_Id + 1;

  No_Distance : constant Database.Light_Years := Database.No_Distance;

  type Header is
    (NC, SC, Name, HD, HIP, HR, M, NGC, IC, Ocl, Ra, Dec, Ra_PM, Dec_PM, Plx, DD, DU, Otype, Stype, Mr, Mg, Mb, Mv);

  -- NC -> No Catalog
  -- SC -> Star Catalog containing star names like Alfa Centauri

  MI : constant Header := SC; -- Main Id
  LN : constant Header := NC; -- Line_Number

  type Cat is new Header range NC .. Ocl;


  function Image_Of (Item : Cat) return String is
  begin
    case Item is
    when NC =>
      return Undefined;
    when SC =>
      return "*";
    when Ocl =>
      return "OCl";
    when others =>
      return Item'image;
    end case;
  end Image_Of;


  subtype Ids_Cat is Cat range HD .. Cat'last; -- catalogs containing data


  function Left_Adjusted (Text       : String;
                          Field_Size : Positive) return String is
    Postfix : constant String(1 .. Field_Size - Text'length) := [others => ' '];
  begin
    return Text & Postfix;
  end Left_Adjusted;


  function Right_Adjusted (Text       : String;
                           Field_Size : Positive) return String is
    Prefix : constant String(1 .. Field_Size - Text'length) := [others => ' '];
  begin
    return Prefix & Text;
  end Right_Adjusted;


  type Id_List is array (Cat) of Id;

  type Information is record
    Main_Cat  : Cat := NC;
    Star_Id   : Database.Star_Id := Database.Unknown_Star_Id;
    Ids       : Id_List := [others => Unknown_Id];
    Ra_J2000  : Database.Degrees_Ra;
    Dec_J2000 : Database.Degrees_Dec;
    Ra_PM     : Database.Motion;
    Dec_PM    : Database.Motion;
    Plx       : Database.Parallax;
    Distance  : Database.Light_Years;
    Mag       : Database.Magnitude;
    Otype     : Object_Type;
    Stype     : Database.Star_Spec_Type;
  end record;

  function "=" (Left, Right : Information) return Boolean is (Left.Ids(Left.Main_Cat) = Right.Ids(Right.Main_Cat));


  package Object_Data is new Ada.Containers.Doubly_Linked_Lists (Information);

  use type Database.Degrees_Ra;

  function "<" (Left, Right : Information) return Boolean is (Left.Ra_J2000 < Right.Ra_J2000);

  package Objects is new Object_Data.Generic_Sorting ("<" => "<");

  The_Objects : Object_Data.List;
  The_Names   : Strings.List;

  type Id_Flag is array (Id range 1 .. 500000) of Boolean with Pack;

  type Id_Flags is array (Cat) of Id_Flag;

  Used_Ids : Id_Flags := [others => [others => False]];

  Max_Name_Length : Natural := 0;

  type Last_Ids is array (Ids_Cat) of Id;

  Last_Id : Last_Ids := [others => Id'first];

  function Field_Size_Of (Item : Ids_Cat) return Natural is (Last_Id(Item)'image'length - 1);


  procedure Read is

    File : IO.File_Type;

    procedure Add_Next_Object is

      The_Object : Information;

      function Ra_J2000_Of (Part: String) return Database.Degrees_Ra is
      begin
        if Part = "" then
          Warning_And_Skip ("No Ra_J2000 value");
        end if;
        begin
          return Database.Degrees_Ra'value(Part);
        exception
        when others =>
          Error ("Incorrect Ra J2000 <" & Part & ">");
        end;
      end Ra_J2000_Of;


      function Dec_J2000_Of (Part: String) return Database.Degrees_Dec is
      begin
        if Part = "" then
          Warning_And_Skip ("No Ra_J2000 value");
        end if;
        begin
          return Database.Degrees_Dec'value(Part);
        exception
        when others =>
          Error ("Incorrect Dec J2000 <" & Part & ">");
        end;
      end Dec_J2000_Of;


      function Motion_Of (Item : String) return Database.Motion is
      begin
        if Item = Undefined then
          return 0.0;
        end if;
        return Database.Motion'value(Item);
      exception
      when others =>
        Error ("Incorrect Proper Motion: " & Item);
      end Motion_Of;


      function Parallax_Of (Item : String) return Database.Parallax is
      begin
        if Item = Undefined then
          return Database.Parallax'first;
        end if;
        begin
          return Database.Parallax'value(Item);
        exception
        when others =>
          Error ("Incorrect Parallax: " & Item);
        end;
      end Parallax_Of;


      function Image_Of (Item  : String;
                         After : Cat) return String is
        Cat_Name : constant String := Image_Of (After);
      begin
        if Strings.Location_Of (Cat_Name, In_String => Item) = Strings.Not_Found then
          Error (Cat_Name & " not found at begin of " & Item);
        end if;
        return Item (Item'first + Cat_Name'length + 1 .. Item'last);
      end Image_Of;


      function Name_Id_Of (Item : String) return Id is
      begin
        if Item = Undefined then
          return Unknown_Id;
        end if;
        declare
          Name_Image : constant String := Removed_The_Of (Image_Of (Item, After => Name));
        begin
          if Base_Name_Of (Name_Image) = Undefined then
            Log.Warning ("Undefined Name " & Name_Image);
            return Unknown_Id;
          else
            if Max_Name_Length < Name_Image'length then
              Max_Name_Length := Name_Image'length;
            end if;
            The_Names.Append (Name_Image);
            return Id(The_Names.Count);
          end if;
        end;
      end Name_Id_Of;


      function Id_Of (Item : String) return Id is
      begin
        return Id'value(Item);
      exception
      when others =>
        return Unknown_Id;
      end Id_Of;


      function Id_Of (Item    : String;
                      For_Cat : Ids_Cat) return Id is

        function Corrected_Ocl_Image return String is
        begin
          if Item'length > 6 and then Item(Item'last - 1) = '.' then
            if Item(Item'last) = '0' then
              return Item(Item'first .. Item'last - 2); -- remove ".0"
            else
              return Undefined; -- Undefined for ".1" .. ".9"
            end if;
          end if;
          return Item;
        end Corrected_Ocl_Image;

        Id_Image : constant String := (if For_Cat = Ocl then Corrected_Ocl_Image else Item);

      begin -- Id_Of
        if Id_Image = Undefined then
          return Unknown_Id;
        end if;
        declare
          Number_Image : constant String := Image_Of (Id_Image, After => For_Cat);
          Last         : Id renames Last_Id (For_Cat);
          The_Id       : Id;
        begin
          The_Id := Id_Of (Number_Image);
          if The_Id = Unknown_Id then
            return Unknown_Id;
          end if;
          if Used_Ids(For_Cat)(The_Id) then
            Warning_And_Skip (Item & " defined twice");
          end if;
          Used_Ids(For_Cat)(The_Id) := True;
          if The_Id > Last then
            Last := The_Id;
          end if;
          return The_Id;
        end;
      end Id_Of;


      function Corrected (Item : String) return String is
        The_Line        : String := Item;
        String_Position : constant Natural := Strings.Location_Of ('"', Item);
      begin
        if String_Position = Strings.Not_Found then
          return Item;
        else
          for The_Character of The_Line(String_Position + 1 .. The_Line'last) loop
            case The_Character is
            when ',' =>
              The_Character := ';'; -- replace comma in string;
            when '"' =>
              exit;
            when others =>
              null;
            end case;
          end loop;
          return The_Line;
        end if;
      end Corrected;

      Line  : constant String := IO.Get_Line (File);
      Parts : constant Strings.Item := Strings.Item_Of (Corrected(Line), ',', Purge => False);
      Count : constant Natural := Parts.Count;

      subtype Magnitude is Database.Magnitude;

      No_Magnitude : constant Magnitude := Database.No_Mag;

      use type Magnitude;

      function Part (Item : Header) return String is (Strings.Trimmed (Parts(Header'pos(Item) + Strings.First_Index)));

      function Cat_Of (Item : Header) return Cat is (Cat(Item));


      function Magnitude_Of (Mag : Header) return Magnitude is
        Item : constant String := Part(Mag);
      begin
        if Item = Undefined then
          return No_Magnitude;
        end if;
        begin
          return Magnitude'value(Item);
        exception
        when others =>
          Error ("Incorrect " & Mag'image & ": " & Item);
        end;
      end Magnitude_Of;


      function Visual_Magnitude return Magnitude is
        The_Magnitude : Magnitude := Magnitude_Of (Mv);
      begin
        if The_Magnitude /= No_Magnitude then
          return The_Magnitude;
        end if;
        declare
          R_Magnitude : constant Magnitude := Magnitude_Of (Mr);
          G_Magnitude : constant Magnitude := Magnitude_Of (Mg);
          B_Magnitude : constant Magnitude := Magnitude_Of (Mb);
        begin
          if R_Magnitude < G_Magnitude then
            The_Magnitude := R_Magnitude;
          else
            The_Magnitude := G_Magnitude;
          end if;
          if B_Magnitude < The_Magnitude then
            The_Magnitude := B_Magnitude;
          end if;
        end;
        return The_Magnitude;
      end Visual_Magnitude;


      function Distance_In_LY return Database.Light_Years is
        Distance_Image : constant String := Part (DD);
        Distance_Unit  : constant String := Part (DU);
        type Unit is (Pc, Kpc, Mpc);
        The_Distance   : Float;
      begin
        if Distance_Image = "" and then Distance_Unit = "" then
          return Database.No_Distance;
        end if;
        The_Distance := Float'value(Distance_Image);
        case Unit'value(Distance_Unit) is
        when Pc =>
          null;
        when Kpc =>
          The_Distance := @ * 1_000.0;
        when Mpc =>
          The_Distance := @ * 1_000_000.0;
        end case;
        return Database.Light_Years(The_Distance * Database.One_Parsec_In_Light_Years);
      end Distance_In_LY;


      Star_Image   : constant String := "*";
      V_Star_Image : constant String := "V*";
      NAME_Image   : constant String := "NAME";
      Cl_Image     : constant String := "Cl"; -- Cl Berkeley

      function Catalog_Id_Of (Main_Image : String) return Cat is
        Name_Parts : constant Strings.Item := Strings.Item_Of (Main_Image, ' ');
        Cat_Id     : constant String := Name_Parts(1);
      begin
        if Cat_Id in Star_Image | V_Star_Image then
          return SC;
        elsif Cat_Id = Cl_Image then
          return Cat_Of (Ocl);
        elsif Cat_Id = NAME_Image then
          return Cat_Of (Name);
        elsif Name_Parts.Count > 2 then
          return NC; -- ignore other main catalogues
        elsif not (Main_Image(Main_Image'last) in '0'..'9') then
          return NC; -- ignore catalogues not ending with a number
        else
          return Ids_Cat'value(Cat_Id);
        end if;
      exception
      when others =>
        return NC;
      end Catalog_Id_Of;


      subtype Greek_Letter is Database.Greek_Letter;
      subtype Star_Number  is Database.Star_Number;
      subtype Star_Index   is Database.Star_Index;

      Unknown_Star_Id : constant Database.Star_Id:= Database.Unknown_Star_Id;

      procedure Define_Star_Id_For (Id_Image : String) is
        Id_Parts : constant Strings.Item := Strings.Item_Of (Id_Image, ' ');
        The_Id   : Database.Star_Id; -- default unknown
      begin
        if Id_Parts.Count = 3 and then Id_Parts(1) = Star_Image then
          declare
            P1     : constant String := Id_Parts(2);
            P2     : constant String := Id_Parts(3);
            use all type Database.Star_Count_Type;
          begin
            if P1(P1'first) in '0' .. '9' then
              The_Id.Kind := Numeric;
              The_Id.Count := Star_Number'value(P1);
            else
              declare
                The_Last : Natural := P1'last;
              begin
                for The_Index in P1'first .. P1'last loop
                  if not (Strings.Lowercase_Of (P1(The_Index)) in 'a' .. 'z') then
                    The_Last := The_Index - 1;
                    exit;
                  end if;
                end loop;
                if The_Last = P1'first then
                  The_Id.Kind := Alphabetic;
                  The_Id.Count := Star_Number(Character'pos(P1(P1'first)) - Character'pos('A'));
                else
                  The_Id.Kind := Greek;
                  The_Id.Count := Star_Number(Greek_Letter'pos(Greek_Letter'value(P1(P1'first .. The_Last))));
                end if;
                if The_Last < P1'last then
                  declare
                    First : constant Positive := (if P1(The_Last + 1) = '.' then The_Last + 2 else The_Last + 1);
                    Index : constant String := P1(First .. P1'last);
                  begin
                    if Index /= "" then
                      The_Id.Index := Star_Index'value(Index);
                    end if;
                  end;
                end if;
              end;
            end if;
            The_Id.C_Id := Database.Constellation'value("C_" & P2);
          exception
          when others =>
            Error ("Incorrect Star Id: " & Id_Image);
          end;
        end if;
        The_Object.Star_Id := The_Id;
      end Define_Star_Id_For;


      procedure Define_Catalogs is
        The_Id        : Id;
        Found_Catalog : Boolean := False;
      begin
        for The_Catalog in Ids_Cat loop
          The_Id := Id_Of (Part(Header(The_Catalog)), The_Catalog);
          The_Object.Ids(The_Catalog) := The_Id;
          if The_Id /= Unknown_Id then
            Found_Catalog := True;
          end if;
        end loop;
        if not Found_Catalog then
          raise Skip_Object;
        end if;
      end Define_Catalogs;


      procedure Define_Main_And_Star_Catalogs is
        Main_Id          : constant String := Part (MI);
        Name_Id          : constant Id := Name_Id_Of (Part (Name));
        The_Main_Catalog : Cat := Catalog_Id_Of (Main_Id);
        use type Database.Star_Id;
      begin
        The_Object.Ids(Name) := Name_Id;
        case The_Main_Catalog is
        when Name =>
          if Name_Id = Unknown_Id then
            The_Main_Catalog := NC;
          end if;
        when SC =>
          Define_Star_Id_For (Main_Id);
          if The_Object.Star_Id = Unknown_Star_Id then
            The_Main_Catalog := NC;
          end if;
        when NC =>
          null;
        when others =>
          if The_Object.Ids(The_Main_Catalog) = Unknown_Id then
            The_Main_Catalog := NC;
          end if;
        end case;
        if The_Main_Catalog = NC then
          for The_Cat in Ids_Cat loop
            if The_Object.Ids(The_Cat) /= Unknown_Id then
              The_Main_Catalog := The_Cat;
              exit;
            end if;
          end loop;
          if The_Main_Catalog = NC then
            Error ("No catalog defined");
          end if;
        end if;
        The_Object.Main_Cat := The_Main_Catalog;
      end Define_Main_And_Star_Catalogs;

      The_Magnitude    : Magnitude;
      The_Object_Type  : Object_Type;

    begin -- Add_Next_Object
      if Count /= Strings.First_Index + Header'pos(Header'last) then
        Log.Write (Line);
        Error ("Incorrect column count (actual:" & Parts.Count'image & ")");
      end if;
      The_Line_Number := Natural'value(Part (LN));
      Define_Catalogs;
      The_Magnitude := Visual_Magnitude;
      The_Object.Mag := The_Magnitude;
      The_Object.Stype := Stype_Of (Part (Stype));
      The_Object_Type := Otype_Of (Part (Otype));
      The_Object.Otype := The_Object_Type;
      The_Object.Plx := Parallax_Of (Part (Plx));
      The_Object.Distance := Distance_In_LY;
      The_Object.Ra_J2000 := Ra_J2000_Of (Part(Ra));
      The_Object.Dec_J2000 := Dec_J2000_Of (Part(Dec));
      The_Object.Ra_PM := Motion_Of (Part (Ra_PM));
      The_Object.Dec_PM := Motion_Of (Part (Dec_PM));
      Define_Main_And_Star_Catalogs; -- must be last action befor append
      if The_Object_Type in Database.Visual_Limited_Star and then The_Magnitude > Database.Max_Star_Magnitude then
        if The_Object.Ids(HR) = Unknown_Id and The_Object.Ids(M) = Unknown_Id and The_Object.Ids(Name) = Unknown_Id then
          raise Skip_Object; -- !!! GNAT limitation (GNAT bug - heap overflow))
        end if;
      end if;
      The_Objects.Append (The_Object);
    exception
    when Skip_Object =>
      null;
    end Add_Next_Object;

    Filename : constant String := Simbad_Folder & "data.csv";

  begin -- Read
    Put_Line ("Generate sky data from " & Filename);
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
    Log.Write ("Header: " & IO.Get_Line (File));
    while not Ada.Text_IO.End_Of_File (File) loop
      Add_Next_Object;
    end loop;
    Ada.Text_IO.Close (File);
    Put_Line ("Sort...");
    Objects.Sort (The_Objects);
    Put_Line ("Done - Object count :" & The_Objects.Length'image);
    Put_Line ("     - Name count   :" & The_Names.Length'image);
    Put_Line ("     -  max length  :" & Max_Name_Length'image);
    for C in Ids_Cat loop
      Put_Line ("     - Last " & Left_Adjusted(Image_Of (C), 8) & ":" & Last_Id(C)'image);
    end loop;
  exception
  when others =>
    Ada.Text_IO.Close (File);
    raise;
  end Read;


  procedure Generate is

    Filename : constant String := Data_Folder & "Database-Objects.ads";

    File : IO.File_Type;

    Object_Index : Natural := 0;

    procedure Output (Line : String := "") is
    begin
      IO.Put_Line (File, Line);
    end Output;

    type Name_Map is array (Id range <>) of Id;

    The_Name_Map : Name_Map(First_Id .. Id(The_Names.Length)) := [others => Unknown_Id];

    type Name_Association is record
      Name : Strings.Element;
      Key  : Id;
    end record;

    package Name_Associations is new Ada.Containers.Doubly_Linked_Lists (Name_Association);

    The_Name_Associations : Name_Associations.List;


    procedure Sort_Names is

      use type Strings.Element;

      function "<" (Left, Right : Name_Association) return Boolean is (Left.Name < Right.Name);

      package Data_Handler is new Name_Associations.Generic_Sorting;

      The_Id : Id;

    begin -- Sort_Names
      The_Id := 0;
      for The_Name of The_Names loop
        The_Id := @ + 1;
        The_Name_Associations.Append (([The_Name], The_Id));
      end loop;
      Data_Handler.Sort (The_Name_Associations);
      The_Id := 0;
      for The_Associoaten of The_Name_Associations loop
        The_Id := @ + 1;
        The_Name_Map (The_Associoaten.Key) := The_Id;
      end loop;
      for Key of The_Name_Map loop
        if Key = Unknown_Id then
          Error ("No name defined for key" & Key'image);
        end if;
      end loop;
    end Sort_Names;


    pragma Style_Checks ("M165");

    procedure Put_Header is

      function Ids_Header return String is
        Header_Image : Strings.Element;
        use type Strings.Element;
      begin
        for C in Ids_Cat loop
          Strings.Append (Header_Image, Right_Adjusted (Image_Of (C), Field_Size_Of (C) + 2));
        end loop;
        return +Header_Image;
      end Ids_Header;

      The_Object_Id_For_Name : array (First_Id .. Id(The_Names.Length)) of Id;

      procedure Evaluate_Object_Id_For_Names is
        The_Object_Id : Id := 0;
      begin
        for The_Object of The_Objects loop
          The_Object_Id := @ + 1;
          if The_Object.Ids(Name) /= Unknown_Id then
            The_Object_Id_For_Name(The_Name_Map(The_Object.Ids(Name))) := The_Object_Id;
          end if;
        end loop;
      end Evaluate_Object_Id_For_Names;

      use type Strings.Element;

    begin -- Put_Header
      Output ("-- *********************************************************************************************************************");
      Output ("-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *");
      Output ("-- *                                               www.white-elephant.ch                                               *");
      Output ("-- *********************************************************************************************************************");
      Output ("-- Generated object information from the Astronomical Database - Université de Strasbourg");
      Output;
      Output ("pragma Style_White_Elephant;");
      Output;
      Output ("package Database.Objects is");
      Output;
      Output ("  pragma Style_Checks (""M191"");");
      Output;
      Output ("  type Data_Range is range 1 .." & The_Objects.Length'image & ";");
      Output;
      Output ("  type Name_Id is range Unknown_Id .." & The_Names.Length'image & ";");
      for C in Ids_Cat loop
        Output ("  type " & Left_Adjusted (C'image & "_Id", 8) & "is range Unknown_Id .." & Last_Id(C)'image & ";");
      end loop;
      Output;
      Output ("  type Catalog_Id is range 1 .." & Cat'pos(Cat'last)'image & ";");
      Output;
      Output ("  type Information is record");
      Output ("    Catalog_Index : Catalog_Id;");
      Output ("    Star_Id       : Natural;");
      Output ("    Name_Index    : Name_Id;");
      for C in Ids_Cat loop
        Output ("    " & Left_Adjusted (C'image & "_Number", 14) & ": " & C'image & "_Id;");
      end loop;
      Output ("    Ra_J2000      : Degrees_Ra;");
      Output ("    Dec_J2000     : Degrees_Dec;");
      Output ("    Ra_PM         : Motion;");
      Output ("    Dec_PM        : Motion;");
      Output ("    Plx           : Parallax;");
      Output ("    Distance      : Light_Years;");
      Output ("    Mag           : Magnitude;");
      Output ("    Otype         : Otype_Id;");
      Output ("    Stype         : Star_Spec_Type;");
      Output ("  end record with Pack;");
      Output;
      Output ("  type Names is (");
      Evaluate_Object_Id_For_Names;
      for The_Association of The_Name_Associations loop
        declare
          Name_Id         : constant Id := The_Name_Map(The_Association.Key);
          Prefix          : constant String := "    S_";
          Postfix         : constant String := (if Name_Id = Id(The_Names.Length) then " " else ",");
          Name_Field_Size : constant Natural := Prefix'length + Max_Name_Length + Postfix'length;
        begin
          Output (Left_Adjusted (Prefix & Base_Name_Of (+The_Association.Name) & Postfix, Name_Field_Size) & " --" & Name_Id'image);
        end;
      end loop;
      Output ("  );");
      Output;
      Output ("  type Name_Map is record");
      Output ("    Item   : Name;");
      Output ("    Object : Data_Range;");
      Output ("  end record;");
      Output;
      Output ("  type Name_List is array (Names) of Data_Range;");
      Output;
      Output ("  Name_Links : constant Name_List := [");
      for The_Association of The_Name_Associations loop
        declare
          Name_Id   : constant Id := The_Name_Map(The_Association.Key);
          Object_Id : constant String := The_Object_Id_For_Name(Name_Id)'image;
          Line_End  : constant String := (if Name_Id = Id(The_Names.Length) then " " else ",");
        begin
          Output (Right_Adjusted (Object_Id, 7) & Line_End & " -- " & (+The_Association.Name));
        end;
      end loop;
      Output ("  ];");
      Output;
      Output ("  type Data is array (Data_Range) of Information;");
      Output;
      Output ("  List : constant Data := [");
      Output ("-- Cat         Star Name" & Ids_Header & "           Ra J2000          Dec J2000      Ra PM     Dec PM       PLX    Distance    MagV Otype    Stype");
    end Put_Header;

    procedure Put (The_Object : Information;
                   Is_Last    : Boolean) is

      No_Information : constant String := "NI";
      Separator      : constant String := ", ";

      Main_Field_Size     : constant := 1;
      Name_Field_Size     : constant := 3;
      Ra_Field_Size       : constant := 17;
      Dec_Field_Size      : constant := 17;
      Pm_Field_Size       : constant := 9;
      Plx_Field_Size      : constant := 8;
      Distance_Field_Size : constant := 10;
      Mag_Field_Size      : constant := 6;
      Otype_Field_Size    : constant := 3;
      Stype_Field_Size    : constant := 16;

      function Image_Of (Item       : Id;
                         Field_Size : Positive) return String is
        Image : constant String := (if Item = Unknown_Id then No_Information else Strings.Trimmed(Item'image));
      begin
        return Right_Adjusted (Image, Field_Size);
      end Image_Of;

      function Image_Of (Item : Database.Star_Luminosity_Class) return String is
        Image : String := Item'image;
      begin
        for The_Character of Image loop
          case The_Character is
          when 'A' =>
            The_Character := 'a';
          when 'B' =>
            The_Character := 'b';
          when others =>
            null;
          end case;
        end loop;
        return Image;
      end Image_Of;

      function Value_Item (Image      : String;
                           Fiels_Size : Natural) return String is
      begin
        return Right_Adjusted (Strings.Trimmed (Image), Fiels_Size) & Separator;
      end Value_Item;

      function Main return String is (Value_Item (Cat'pos(The_Object.Main_Cat)'image, Main_Field_Size));

      function Name return String is
        Name_Id  : constant Id := The_Object.Ids(Name);
        Name_Key : constant Id := (if Name_Id = Unknown_Id then Unknown_Id else The_Name_Map(Name_Id));
      begin
        return Image_Of (Name_Key, Name_Field_Size) & Separator;
      end Name;

      function Star return String is
        Star_Id     : constant Database.Star_Id := The_Object.Star_Id;
        Kind_Image  : constant String := Strings.Trimmed (Database.Star_Count_Type'pos(Star_Id.Kind)'image);
        Count_Image : constant String := Strings.Trimmed (Star_Id.Count'image);
        Index_Image : constant String := Strings.Trimmed (Star_Id.Index'image);
        C_Id_Image  : constant String := Strings.Trimmed (Database.Constellation'pos(Star_Id.C_Id)'image);
                                   --  K Cnt Ix Con
        Image    :          String := "0_000_00_00, ";
                                   --  1   5  8  11
        Image_NI : constant String := "         NI, ";
        use type Database.Star_Id;
      begin
        if Star_Id = Database.Unknown_Star_Id then
          return Image_NI;
        end if;
        Image ( 1 -  Kind_Image'length + 1 ..  1) := Kind_Image;
        Image ( 5 - Count_Image'length + 1 ..  5) := Count_Image;
        Image ( 8 - Index_Image'length + 1 ..  8) := Index_Image;
        Image (11 -  C_Id_Image'length + 1 .. 11) := C_Id_Image;
        return Image;
      end Star;

      function Ids return String is
        Ids_Image : Strings.Element;
        use type Strings.Element;
      begin
        for C in Ids_Cat loop
          Strings.Append (Ids_Image, Image_Of (The_Object.Ids(C), Field_Size_Of (C)) & Separator);
        end loop;
        return +Ids_Image;
      end Ids;

      function Ra_J2000 return String is (Value_Item (The_Object.Ra_J2000'image, Ra_Field_Size));

      function Dec_J2000 return String is (Value_Item (The_Object.Dec_J2000'image, Dec_Field_Size));

      function Ra_Pm return String is (Value_Item (The_Object.Ra_PM'image, Pm_Field_Size));

      function Dec_Pm return String is (Value_Item (The_Object.Dec_PM'image, Pm_Field_Size));

      function Plx return String is
        Value : constant Database.Parallax := The_Object.Plx;
        Image : constant String := Value'image;
      begin
        return Value_Item (Image, Plx_Field_Size);
      end Plx;

      function Distance return String is
        use type Database.Light_Years;
        Distance_Value : constant Database.Light_Years := The_Object.Distance;
        Distance_Image : constant String := (if Distance_Value = No_Distance then "NI" else Distance_Value'image);
      begin
        return (Value_Item (Distance_Image, Distance_Field_Size));
      end Distance;

      function Magnitude (Value      : Database.Magnitude;
                          Field_Size : Positive) return String is
        Image : constant String := Value'image;
      begin
        return Value_Item (Image, Field_Size);
      end Magnitude;

      function Vmag return String is (Magnitude (The_Object.Mag, Mag_Field_Size));

      function Otype return String is (Value_Item (Object_Type'pos(The_Object.Otype)'image, Otype_Field_Size));

      function Stype return String is
        Kind       : constant Database.Star_Spec_Type := The_Object.Stype;
        Class      : constant String := Kind.Class'image;
        Subclass   : constant String := Kind.Subclass'image;
        Luminosity : constant String := Image_Of (Kind.Luminosity);
        Image      : constant String := "(" & Class & ", " & Subclass & ", " & Luminosity & "))";
      begin
        return Left_Adjusted (Image & (if Is_Last then "" else ","), Stype_Field_Size);
      end Stype;

      function Object_Id return String is
      begin
        Object_Index := @ + 1;
        return "--" & Object_Index'image;
      end Object_Id;

    begin -- Put
      Output ("    (" & Main & Star & Name & Ids & Ra_J2000 & Dec_J2000 & Ra_Pm & Dec_Pm & Plx & Distance & Vmag & Otype & Stype & Object_Id);
    end Put;

    procedure Put_Footer is
    begin
      Output ("  ];");
      Output;
      Output ("end Database.Objects;");
    end Put_Footer;

  begin -- Generate
    Put_Line ("Create " & Filename);
    IO.Create (File, Name => Filename);
    Sort_Names;
    Put_Header;
    for The_Object of The_Objects loop
      Put (The_Object, Is_Last => The_Object = The_Objects.Last_Element);
    end loop;
    Put_Footer;
    IO.Close (File);
    Put_Line (Filename & " created");
  exception
  when others =>
    IO.Close (File);
    raise;
  end Generate;

end Generator.Data;
