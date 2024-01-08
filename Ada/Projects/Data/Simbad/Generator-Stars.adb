-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024  by White Elephant GmbH, Schaffhausen, Switzerland                         *
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

package body Generator.Stars is

  package Log is new Traces ("Generator.Stars");

  Undefined : constant String := "";

  Skip_Object : exception;


  function Otype_Of (Item : String) return Database.Star_Type is
  -- * Star
  --    Ma*         Ma?    Massive Star
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

    use all type Database.Star_Type;

  begin -- Otype_Of
    if Item = Undefined then
      raise Skip_Object;
    elsif Item in "a2*" | "a2?" then
      return Alpha2_Cvn_Variable_Star;
    elsif Item in "AB*" | "AB?" then
      return Asymptotic_Giant_Branch_Star;
    elsif Item = "Be*" then
      return Be_Star;
    elsif Item in "bC*" | "bC?" then
      return Beta_Cep_Variable_Star;
    elsif Item in "BS*" | "BS?" then
      return Blue_Straggler;
    elsif Item = "BY*" then
      return BY_Dra_Variable_Star;
    elsif Item in "s*b" | "s?b" then
      return Blue_Supergiant_Star;
    elsif Item in "C*" | "C*?" then
      return Carbon_Star;
    elsif Item = "CV*" then
      return Cataclysmic_Binary_Star;
    elsif Item = "Ce*" then
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
    elsif Item = "sg*" then
      return Evolved_Supergiant_Star;
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
    elsif Item = "HS*" then
      return Hot_Subdwarf_Star;
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
    elsif Item = "OH*" then
      return OH_IR_Star;
    elsif Item = "Or*" then
      return Orion_Variable_Star;
    elsif Item = "PN" then
      return Planetary_Nebula_Star;
    elsif Item in "pA*" | "pA?" then
      return Post_AGB_Star;
    elsif Item = "Pu*" then
      return Pulsating_Variable_Star;
    elsif Item = "RC*" then
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
    elsif Item = "SX*" then
      return SX_Phe_Variable;
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
    elsif Item = "s*y" then
      return Yellow_Supergiant_Star;
    elsif Item in "Y*O" | "Y*?" then
      return Young_Stellar_Object_Star;
    else
      Error ("Unknown Otype: " & Item);
    end if;
  end Otype_Of;


  function Stype_Of (Item : String) return Database.Star_Spec_Type is

  -- O, B, A, F, G, K, M plus sub-type (0, 1, etc), and sometimes intermediate sub-type (for example F7.2, F7.5, F7.7):
  --   for the spectral types of `normal stars';

  -- R, S, N, C
  --   for carbon stars;

  -- DB, DA, DF, DG:
  --   for white dwarfs;

  -- WR, WN, WC:
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
    if Item = Undefined then
      raise Skip_Object;
    end if;
    while not (Image(Index) in 'O' | 'B' | 'A' | 'F' | 'G' | 'K' | 'M' | 'R' | 'S' | 'N' | 'C' | 'D' | 'W' | ' ') loop
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
    when 'M' =>
      The_Class := M;
    when 'R' =>
      The_Class := R;
    when 'S' =>
      The_Class := S;
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
      when 'F' =>
        The_Class := DF;
      when 'G' =>
        The_Class := DG;
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
      when ' ' | ':' | '+' | '/'  | '(' | ')' |'e' | 'f' =>
        -- I | I: | I+ | I/II | I(n) | (I) | Ie | If -> II
        The_Luminosity_Class := II;
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


  function Base_Name_Of (Item : String) return String is
    Image    : String  := Item;
    Is_First : Boolean := True;
  begin
    for The_Character of Image loop
      case Strings.Lowercase_Of (The_Character) is
      when 'a'..'z' =>
        if Is_First then
          The_Character := Strings.Uppercase_Of (@);
          Is_First := False;
        else
          The_Character := Strings.Lowercase_Of (@);
        end if;
      when ' ' | ''' | '_' =>
        The_Character := '_';
        Is_First := True;
      when others =>
        return Undefined;
      end case;
    end loop;
    return Image;
  end Base_Name_Of;


  type Id is new Natural;

  Unknown_Id : constant := Database.Unknown_Id;
  First_Id   : constant Id := Unknown_Id + 1;

  type Information is record
    Name_Id : Id := Unknown_Id;
    Hd_Id   : Id := Unknown_Id;
    Hip_Id  : Id := Unknown_Id;
    Hr_Id   : Id := Unknown_Id;
    Data    : Database.Star_Information;
  end record;

  function "=" (Left, Right : Information) return Boolean is
    ((Left.Hip_Id = Right.Hip_Id) and (Left.Hd_Id = Right.Hd_Id) and (Left.Hr_Id = Right.Hr_Id));


  package Star_Data is new Ada.Containers.Doubly_Linked_Lists (Information);

  use type Database.Magnitude;

  function "<" (Left, Right : Information) return Boolean is (Left.Data.Vmag < Right.Data.Vmag);

  package Data is new Star_Data.Generic_Sorting ("<" => "<");

  The_Stars : Star_Data.List;
  The_Names : Strings.List;

  Max_Name_Length : Natural := 0;

  HD_Last  : Id := Id'first;
  HIP_Last : Id := Id'first;
  HR_Last  : Id := Id'first;


  procedure Read is

    File : IO.File_Type;

    procedure Add_Next_Object is

      The_Star : Information;

      function Ra_J2000_Of (Part: String) return Database.Degrees_Ra is
      begin
        return Database.Degrees_Ra'value(Part);
      exception
      when others =>
        Error ("Incorrect Ra J2000 <" & Part & ">");
      end Ra_J2000_Of;


      function Dec_J2000_Of (Part: String) return Database.Degrees_Dec is
      begin
        return Database.Degrees_Dec'value(Part);
      exception
      when others =>
        Error ("Incorrect Dec J2000 <" & Part & ">");
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
          raise Skip_Object;
        end if;
        begin
          return Database.Parallax'value(Item);
        exception
        when others =>
          Error ("Incorrect Parallax: " & Item);
        end;
      end Parallax_Of;


      function Magnitude_Of (Item : String) return Database.Magnitude is
      begin
        if Item = Undefined then
          Error ("Undefined Magnitude");
        end if;
        return Database.Visual_Magnitude'(Database.Magnitude'value(Item));
      exception
      when Error_Occured =>
        raise;
      when others =>
        Error ("Incorrect Magnitude: " & Item);
      end Magnitude_Of;


      function Image_Of (Item  : String;
                         After : String) return String is
      begin
        if Strings.Location_Of (After, In_String => Item) = Strings.Not_Found then
          Error (After & " not found at begin of " & Item);
        end if;
        return Item (Item'first + After'length + 1 .. Item'last);
      end Image_Of;


      function Name_Id_Of (Item : String) return Id is
      begin
        if Item = Undefined then
          return Unknown_Id;
        end if;
        declare
          Name : constant String := Image_Of (Item, After => "NAME");
        begin
          if Base_Name_Of (Name) = Undefined then
            Log.Warning ("Undefined Name" & Name);
            return Unknown_Id;
          else
            if Max_Name_Length < Name'length then
              Max_Name_Length := Name'length;
            end if;
            The_Names.Append (Name);
            return Id(The_Names.Count);
          end if;
        end;
      end Name_Id_Of;


      function Id_Of (Item   :        String;
                      Prefix :        String;
                      Last   : in out Id) return Id is
      begin
        if Item = Undefined then
          return Unknown_Id;
        end if;
        declare
          Number_Image : constant String := Image_Of (Item, After => Prefix);
          The_Id : Id;
        begin
          The_Id := Id'value(Number_Image);
          if The_Id > Last then
            Last := The_Id;
          end if;
          return The_Id;
        exception
        when others =>
          raise Skip_Object;
        end;
      end Id_Of;


      function Hd_Id_Of (Item : String) return Id is (Id_Of (Item, "HD", HD_Last));

      function Hip_Id_Of (Item : String) return Id is (Id_Of (Item, "HIP", HIP_Last));

      function Hr_Id_Of (Item : String) return Id is (Id_Of (Item, "HR", HR_Last));


      type Header is (Main_Id, Id_Name, Id_HD, Id_HIP, Id_HR, Ra, Dec, Ra_PM, Dec_PM, Plx, Otype, Stype, Mag_V);

      Line  : constant String := IO.Get_Line (File);
      Parts : constant Strings.Item := Strings.Item_Of (Line, ',', Purge => False);
      Count : constant Natural := Parts.Count;

      function Part (Item : Header) return String is (Strings.Trimmed (Parts(Header'pos(Item) + Strings.First_Index)));

      Object_Name : constant String := Part(Main_Id);

    begin -- Add_Next_Object
      if Count /= Strings.First_Index + Header'pos(Header'last) then
        Error ("Not enough parts (actual:" & Parts.Count'image & ")");
      end if;
      Log.Write ("Object: " & Object_Name);
      The_Star.Data.Vmag := Magnitude_Of (Part (Mag_V));
      The_Star.Data.Stype := Stype_Of (Part (Stype));
      The_Star.Data.Otype := Otype_Of (Part (Otype));
      The_Star.Data.Plx := Parallax_Of (Part (Plx));
      The_Star.Data.Ra_J2000 := Ra_J2000_Of (Part(Ra));
      The_Star.Data.Dec_J2000 := Dec_J2000_Of (Part(Dec));
      The_Star.Data.Ra_PM := Motion_Of (Part (Ra_PM));
      The_Star.Data.Dec_PM := Motion_Of (Part (Dec_PM));
      The_Star.Hd_Id := Hd_Id_Of (Part (Id_HD));
      The_Star.Hip_Id := Hip_Id_Of (Part (Id_HIP));
      The_Star.Hr_Id := Hr_Id_Of (Part (Id_HR));
      if (The_Star.Hd_Id = Unknown_Id) and (The_Star.Hip_Id = Unknown_Id) and (The_Star.Hr_Id = Unknown_Id) then
        Log.Warning ("No id for " & Object_Name);
      else
        The_Star.Name_Id := Name_Id_Of (Part (Id_Name));
        The_Stars.Append (The_Star);
      end if;
    exception
    when Skip_Object =>
      Log.Write ("Skip " & Object_Name);
    end Add_Next_Object;

    Filename : constant String := Simbad_Folder & "stars.csv";

  begin -- Read
    Put_Line ("Generate sky star data from " & Filename);
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
    Log.Write ("Header: " & IO.Get_Line (File));
    The_Line_Number := 1;
    while not Ada.Text_IO.End_Of_File (File) loop
      The_Line_Number := @ + 1;
      Add_Next_Object;
    end loop;
    Put_Line ("Sort...");
    Ada.Text_IO.Close (File);
    Data.Sort (The_Stars);
    Put_Line ("Done - Star count  :" & The_Stars.Length'image);
    Put_Line ("     - Name count  :" & The_Names.Length'image);
    Put_Line ("     -  max length :" & Max_Name_Length'image);
    Put_Line ("     - Last HD     :" & HD_Last'image);
    Put_Line ("     - Last HIP    :" & HIP_Last'image);
    Put_Line ("     - Last HR     :" & HR_Last'image);
  exception
  when others =>
    Ada.Text_IO.Close (File);
    raise;
  end Read;


  procedure Generate_Database is

    Filename : constant String := Data_Folder & "Database-Stars.ads";

    File : IO.File_Type;

    Star_Index : Natural := 0;

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


    pragma Style_Checks ("M173");

    procedure Put_Header is

      The_Star_Id_For_Name : array (First_Id .. Id(The_Names.Length)) of Id;

      procedure Evaluate_Star_Id_For_Names is
        The_Star_Id : Id := 0;
      begin
        for The_Star of The_Stars loop
          The_Star_Id := @ + 1;
          if The_Star.Name_Id /= Unknown_Id then
            The_Star_Id_For_Name(The_Name_Map(The_Star.Name_Id)) := The_Star_Id;
          end if;
        end loop;
      end Evaluate_Star_Id_For_Names;

      use type Strings.Element;

    begin -- Put_Header
      Output ("-- *********************************************************************************************************************");
      Output ("-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *");
      Output ("-- *                                               www.white-elephant.ch                                               *");
      Output ("-- *********************************************************************************************************************");
      Output ("-- Generated Star information from the Astronomical Database - Université de Strasbourg");
      Output;
      Output ("pragma Style_White_Elephant;");
      Output;
      Output ("package Database.Stars is");
      Output;
      Output ("  pragma Style_Checks (""M174"");");
      Output;
      Output ("  type Data_Range is range 1 .." & The_Stars.Length'image & ";");
      Output;
      Output ("  type Name_Id is range Unknown_Id .." & The_Names.Length'image & ";");
      Output ("  type HD_Id   is range Unknown_Id .." & HD_Last'image & ";");
      Output ("  type HIP_Id  is range Unknown_Id .." & HIP_Last'image & ";");
      Output ("  type HR_Id   is range Unknown_Id .." & HR_Last'image & ";");
      Output;
      Output ("  type Information is record");
      Output ("    Name_Index : Name_Id;");
      Output ("    HD_Number  : HD_Id;");
      Output ("    HIP_Number : HIP_Id;");
      Output ("    HR_Number  : HR_Id;");
      Output ("    Info       : Star_Information;");
      Output ("  end record with Pack;");
      Output;
      Output ("  type Names is (");
      Evaluate_Star_Id_For_Names;
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
      Output ("    Item : Name;");
      Output ("    Star : Data_Range;");
      Output ("  end record;");
      Output;
      Output ("  type Name_List is array (Names) of Data_Range;");
      Output;
      Output ("  Name_Links : constant Name_List := [");
      for The_Association of The_Name_Associations loop
        declare
          Name_Id         : constant Id := The_Name_Map(The_Association.Key);
          Star_Id         : constant String := The_Star_Id_For_Name(Name_Id)'image;
          Line_End        : constant String := (if Name_Id = Id(The_Names.Length) then " " else ",");
        begin
          Output (Right_Adjusted (Star_Id, 7) & Line_End & " -- " & (+The_Association.Name));
        end;
      end loop;
      Output ("  ];");
      Output;
      Output ("  type Data is array (Data_Range) of Information;");
      Output;
      Output ("  List : constant Data := [");
      Output ("--  Name      HD     HIP    HR  Otype           Ra J2000          Dec J2000      Ra PM     Dec PM       PLX    MagV    Stype");
    end Put_Header;

    procedure Put (The_Star : Information;
                   Is_Last  : Boolean) is

      No_Information : constant String := "NI";
      Separator      : constant String := ", ";

      Name_Field_Size  : constant := 3;
      Hd_Field_Size    : constant := 6;
      Hip_Field_Size   : constant := 6;
      Hr_Field_Size    : constant := 4;
      Ra_Field_Size    : constant := 17;
      Dec_Field_Size   : constant := 17;
      Pm_Field_Size    : constant := 9;
      Plx_Field_Size   : constant := 8;
      Mag_Field_Size   : constant := 6;
      Stype_Field_Size : constant := 16;

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

      function Name return String is
        Name_Id  : constant Id := The_Star.Name_Id;
        Name_Key : constant Id := (if Name_Id = Unknown_Id then Unknown_Id else The_Name_Map(Name_Id));
      begin
        return Image_Of (Name_Key, Name_Field_Size) & Separator;
      end Name;

      function Hd return String is (Image_Of (The_Star.Hd_Id, Hd_Field_Size) & Separator);

      function Hip return String is (Image_Of (The_Star.Hip_Id, Hip_Field_Size) & Separator);

      function Hr return String is (Image_Of (The_Star.Hr_Id, Hr_Field_Size) & Separator);

      function Otype return String is
        Kind  : constant Database.Star_Type := The_Star.Data.Otype;
        Image : constant String := '0' & Strings.Trimmed (Database.Star_Type'pos(Kind)'image);
      begin
        return "(OT" & Image(Image'last - 1 .. Image'last) & Separator;
      end Otype;

      function Value_Item (Image      : String;
                           Fiels_Size : Natural) return String is
      begin
        return Right_Adjusted (Strings.Trimmed (Image), Fiels_Size) & Separator;
      end Value_Item;

      function Ra_J2000 return String is (Value_Item (The_Star.Data.Ra_J2000'image, Ra_Field_Size));

      function Dec_J2000 return String is (Value_Item (The_Star.Data.Dec_J2000'image, Dec_Field_Size));

      function Ra_Pm return String is (Value_Item (The_Star.Data.Ra_PM'image, Pm_Field_Size));

      function Dec_Pm return String is (Value_Item (The_Star.Data.Dec_PM'image, Pm_Field_Size));

      function Plx return String is
        Value : constant Database.Parallax := The_Star.Data.Plx;
        Image : constant String := Value'image;
      begin
        return Value_Item (Image, Plx_Field_Size);
      end Plx;

      function Magnitude (Value      : Database.Magnitude;
                          Field_Size : Positive) return String is
        Image : constant String := Value'image;
      begin
        return Value_Item (Image, Field_Size);
      end Magnitude;

      function Vmag return String is (Magnitude (The_Star.Data.Vmag, Mag_Field_Size));

      function Stype return String is
        Kind       : constant Database.Star_Spec_Type := The_Star.Data.Stype;
        Class      : constant String := Kind.Class'image;
        Subclass   : constant String := Kind.Subclass'image;
        Luminosity : constant String := Image_Of (Kind.Luminosity);
        Image      : constant String := "(" & Class & ", " & Subclass & ", " & Luminosity & ")))";
      begin
        return Left_Adjusted (Image & (if Is_Last then "" else ","), Stype_Field_Size);
      end Stype;

      function Star_Id return String is
      begin
        Star_Index := @ + 1;
        return "--" & Star_Index'image;
      end Star_Id;

    begin -- Put
      Output ("    (" & Name & Hd & Hip & Hr & Otype & Ra_J2000 & Dec_J2000 & Ra_Pm & Dec_Pm & Plx & Vmag & Stype & Star_Id);
    end Put;

    procedure Put_Footer is
    begin
      Output ("  ];");
      Output;
      Output ("end Database.Stars;");
    end Put_Footer;

  begin -- Generate_Database
    Put_Line ("Create " & Filename);
    IO.Create (File, Name => Filename);
    Sort_Names;
    Put_Header;
    for The_Star of The_Stars loop
      Put (The_Star, Is_Last => The_Star = The_Stars.Last_Element);
    end loop;
    Put_Footer;
    IO.Close (File);
    Put_Line (Filename & " created");
  exception
  when others =>
    IO.Close (File);
    raise;
  end Generate_Database;

end Generator.Stars;
