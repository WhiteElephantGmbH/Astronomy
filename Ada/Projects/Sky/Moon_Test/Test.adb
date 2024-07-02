-- *********************************************************************************************************************
-- *                            (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                             *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Angle;
with Astro;
with Exceptions;
with Log;
with Site;
with Space;
with Text;
with Time;

package body Test is

  package IO renames Ada.Text_IO;

  procedure Put_Line (Item : String) is
  begin
    IO.Put_Line (Item);
    Log.Write (Item);
  end Put_Line;

--  use Astro;
--  use MATLIB;
--  use MOOLIB;
--  use PNULIB;
--  use SPHLIB;
--
--  ER : constant REAL := Earth_Equatorial_Radius;
--  MR : constant REAL := 1737.4;
--
--
--  procedure Reduce (Item : in out REAL) is
--  begin
--    while Item < 180.0 loop
--      Item := @ + 360.0;
--    end loop;
--    while Item > 180.0 loop
--      Item := @ - 360.0;
--    end loop;
--  end Reduce;
--
--
--  function RED (Item : REAL) return REAL is
--    Result : REAL := Item;
--  begin
--    Reduce (Result);
--    if Result < 0.0 then
--      Result := @ + 360.0;
--    end if;
--    return Result;
--  end RED;
--
--
--  ------------------------------------------------------
--  -- Berechnung der Liberation des Mondes             --
--  -- nach Jean Meeus - Astronomische Algorithmen 1998 --
--  ------------------------------------------------------
--  procedure LIB (T  :     REAL;
--                 L  :     REAL; -- scheinbare geozentrische Länge des Mondes (ohne Nutation)
--                 B  :     REAL; -- scheinbare geozentrische Breite des Mondes
--                 LL : out REAL; -- liberation in Länge
--                 LB : out REAL) -- liberation in Breite
--  is
--    T2 : constant REAL := T * T;
--    T3 : constant REAL := T * T2;
--    T4 : constant REAL := T * T3;
--
--    -- Mittlere Elongation des Mondes -> Kapitel 45.2
--    D : constant REAL := RED(297.850_1921+445_267.111_4034*T-0.001_8819*T2+T3/545_868.0-T4/113_065_000.0); -- 1998
--  --D : constant REAL := RED(297.850_2042+445_267.111_5168*T-0.001_6300*T2+T3/545_868.0-T4/113_065_000.0); -- 1992
--
--    -- Mittlere Anomalie der Sonne -> Kapitel 45.3
--    MS : constant REAL := RED(357.529_1092 + 35_999.050_2909 * T - 0.000_1535 * T2 + T3 / 24_490_000.0); -- 1998
--  --MS : constant REAL := RED(357.529_1092 + 35_999.050_2909 * T - 0.000_1536 * T2 + T3 / 24_490_000.0); -- 1992
--
--    -- Mittlere Anomalie des Mondes -> Kapitel 45.4
--    MM : constant REAL := RED(134.963_3964+477_198.867_5055*T+0.008_7414*T2+T3/69_699.0-T4/14_712_000.0); -- 1998
--  --MM : constant REAL := RED(134.963_4114+477_198.867_6313*T+0.008_9970*T2+T3/69_699.0-T4/14_712_000.0); -- 1992
--
--    -- mittlerer Abstand des Mondes von seinem aussteigenden Knoten -> Kapitel 45.5
--    F : constant REAL := RED(93.272_095+483_202.017_5233*T-0.003_6539*T2-T3/3_526_000.0+T4/863_310_000.0);  -- 1998
--  --F : constant REAL := RED(93.272_0993+483_202.017_5273*T-0.003_4029*T2-T3/3_526_000.0+T4/863_310_000.0); -- 1992
--
--    --> Kapitel 45.6
--    E: constant REAL := RED(1.0-0.002_516*T-0.000_0074*T2);
--
--    -- Länge des mittleren aufsteigenden Knotens -> Kapitel 45.7
--    O : constant REAL := RED(125.044_5579-1934.136_2891*T+0.002_0762*T2+T3/467_410.0-T4/60_616_000.0); -- 1998
--  --O : constant REAL := RED(125.044_5550-1934.136_1849*T+0.002_0762*T2+T3/467_410.0-T4/60_616_000.0); -- 1992
--
--    A : REAL;
--
--    OLL,OLB : REAL; -- optische liberation in Länge und Breite
--    PLL,PLB : REAL; -- physische liberation in Länge und Breite
--
--
--    -- Optische Liberation
--    procedure OLIB is
--
--      I  : constant REAL := 1.54242; -- Neigung des mittleren Mondäquators gegen die Ekliptik
--      SI : constant REAL := SN(I);
--      CI : constant REAL := CS(I);
--
--      W : constant REAL := L-O;
--
--    begin -- OLIB
--      A := RED(ATN2(SN(W)*CS(B)*CI-SN(B)*SI,CS(W)*CS(B)));
--      OLL := A-F;
--      OLB := ASN(-SN(W)*CS(B)*SI-SN(B)*CI);
--      Reduce (OLL);
--      Reduce (OLB);
--    end OLIB;
--
--
--    -- Physische Liberation
--    procedure PLIB is
--
--      K1 : constant REAL := 119.75+131.849*T;
--      K2 : constant REAL := 72.56+20.186*T;
--
--      ROH : constant REAL := -0.027_52*CS(MM)-0.022_45*SN(F)+0.006_84*CS(MM-2.0*F)-0.002_93*CS(2.0*F)
--                             -0.000_85*CS(2.0*(F-D))-0.000_54*CS(MM-2.0*D)-0.000_2*SN(MM+F)-0.000_2*CS(MM+2.0*F)
--                             -0.000_2*CS(MM-F)+0.000_14*CS(MM+2.0*(F-D));
--
--      SIG : constant REAL := -0.028_16*SN(MM)+0.022_44*CS(F)-0.006_82*SN(MM-2.0*F)-0.002_79*SN(2.0*F)
--                             -0.000_83*SN(2.0*(F-D))+0.000_69*SN(MM-2.0*D)+0.000_4*CS(MM+F)-0.000_25*SN(2.0*MM)
--                             -0.000_23*SN(MM+2.0*F)+0.000_2*CS(MM-F)+0.000_19*SN(MM-F)+0.000_13*SN(MM+2.0*(F-D))
--                             -0.000_1*CS(MM-3.0*F);
--
--      TAU : constant REAL := +0.025_2 *SN(MS)*E+0.004_73*SN(2.0*(MM-F))-0.004_67*SN(MM)+0.003_96*SN(K1)
--                             +0.002_76*SN(2.0*(MM-D))+0.001_96*SN(O)-0.001_83*CS(MM-F)+0.001_15*SN(MM-2.0*D)
--                             -0.000_96*SN(MM-D)+0.000_46*SN(2.0*(F-D))-0.000_39*SN(MM-F)-0.000_32*SN(MM-MS-D)
--                             +0.000_27*SN(2.0*(MM-D)-MS)+0.000_23*SN(K2)-0.000_14*SN(2.0*D)+0.000_14*CS(2.0*(MM-F))
--                             -0.000_12*SN(MM-2.0*F)-0.000_12*SN(2.0*MM)+0.000_11*SN(2.0*(MM-MS-D));
--
--    begin -- PLIB
--      Put_Line ("A   =" & A'image);
--      Put_Line ("K1  =" & K1'image);
--      Put_Line ("K2  =" & K2'image);
--      Put_Line ("ROH =" & ROH'image);
--      Put_Line ("SIG =" & SIG'image);
--      Put_Line ("TAU =" & TAU'image);
--      PLL := -TAU+(ROH*CS(A)+SIG*SN(A))*TN(OLB);
--      PLB := SIG*CS(A)-ROH*SN(A);
--    end PLIB;
--
--  begin -- LIB
--    Put_Line ("T =" & T'image);
--    Put_Line ("D =" & D'image);
--    Put_Line ("M =" & MS'image);
--    Put_Line ("M'=" & MM'image);
--    Put_Line ("F =" & F'image);
--    Put_Line ("E =" & E'image);
--    Put_Line ("O =" & O'image);
--    OLIB;
--    Put_Line ("optische Liberation: OLL =" & OLL'image &
--                                " | OLB =" & OLB'image);
--    PLIB;
--    Put_Line ("physische Liberation: PLL =" & PLL'image &
--                                 " | PLB =" & PLB'image);
--    LL := OLL + PLL;
--    LB := OLB + PLB;
--  end LIB;
--
--
--  -----------------------------------------------------------------------------
--  -- MOONEQU: aequatoriale Mondkoordinaten
--  --          (Rektaszension RA und Deklination DEC in Grad, R in Erdradien)
--  --          T in julian.Jahrhndt. seit J2000 ( T:= (JD - 2451545.0)/36525 )
--  --          LS, LB: Selenografischen Länge (E+) und Breite (N+)
--  --          Die Koord. beziehen sich auf das wahre Aequinoktium des Datums.
--  -----------------------------------------------------------------------------
--  procedure MOONEQU(T:            REAL;
--                    LS,BS:        REAL;
--                    RA,DEC,R: out REAL) is
--    L,B,LL,LB,LSC,BSC,X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,XS,YS,ZS: REAL;
--  begin
--    MOON(T,L,B,R); -- ekliptikale Moondkoordinaten
--    Put_Line ("Ecliptic Moon Coorinates: L =" & L'image &
--                                     " | B =" & B'image &
--                                     " | R =" & REAL'(R*ER)'image & "km");
--
--    if LS = 0.0 and BS = 0.0 then
--      CART(R,B,L,X,Y,Z); -- mittleres Aequinoktium des Datums
--    else
--      -- Liberations Korrektur der selenografischen Koordinaten
--      LIB (T,L,B,LL,LB);
--      LSC := LL-LS; -- west negtive
--      BSC := BS-LB;
--      Put_Line ("Liberation: LL =" & LL'image &
--                         " | LB =" & LB'image);
--
--      CART(MR,BSC,LSC,X1,Y1,Z1); -- Umwandlung der selenografischen Koordinaten in kartesische Koordinaten
--      Put_Line ("Object Coorinates: X1 =" & X1'image &
--                                " | Y1 =" & Y1'image &
--                                " | Z1 =" & Z1'image);
--
--      -- Rotation um die ekliptikale Länge des Mondes
--      X2 := X1*CS(L)-Y1*SN(L);
--      Y2 := X1*SN(L)+Y1*CS(L);
--      Z2 := Z1;
--      Put_Line ("Object Coorinates: X2 =" & X2'image &
--                                " | Y2 =" & Y2'image &
--                                " | Z2 =" & Z2'image);
--
--      -- Rotation um die ekliptikale Breite des Mondes
--      XS := X2;
--      YS := Y2*CS(B)-Z2*SN(B);
--      ZS := Y2*SN(B)+Z2*CS(B);
--      Put_Line ("Object Coorinates: XS =" & XS'image &
--                                " | YS =" & YS'image &
--                                " | ZS =" & ZS'image);
--
--      -- Addition der geozentrischen Koordinaten des Mondes
--      CART(R*ER,B,L,X,Y,Z);
--      X := X+XS;
--      Y := Y+YS;
--      Z := Z+ZS;
--      Put_Line ("Moon Coorinates in km: X =" & X'image &
--                                    " | Y =" & Y'image &
--                                    " | Z =" & Z'image);
--
--      X := X/ER;
--      Y := Y/ER;
--      Z := Z/ER;
--    end if;
--
--    Put_Line ("Cartesian Moon Coorinates: X =" & X'image &
--                                      " | Y =" & Y'image &
--                                      " | Z =" & Z'image);
--
--    ECLEQU(T,X,Y,Z);       -- Umwandlung in aequatoriale Koordinaten
--    NUTEQU(T,X,Y,Z);       -- Nutation
--    POLAR(X,Y,Z,R,DEC,RA);
--  end MOONEQU;

  use Astro;
  use MATLIB;
  use MOOLIB;

  function Direction_Of (UT : Time.Ut) return Space.Direction is

    DEC, RA, R, RSPHI, RCPHI, LMST : REAL;
    G: VECTOR; -- geozentric moon vector
    L: VECTOR; -- Location vector
    T: VECTOR; -- Topocentric Vector
    use all type Angle.Value;

  begin -- Direction_Of
    MOONEQU (T   => Time.Tet_Of (UT),
             LS  =>  0.0, -- -REAL'(+Angle.Value_Of ("20d04'42""")), -- selenographic longitude (W-)
             BS  => 90.0, -- +REAL'(+Angle.Value_Of ("09d37'15""")), -- (Copernicus)  latitude  (N+)
             RA  => RA,
             DEC => DEC,
             R   => R);

    G := CART (R, DEC, RA);

    SPHLIB.SITE (PHI   => +Site.Latitude,
                 RCPHI => RCPHI,
                 RSPHI => RSPHI);

    LMST := +Time.Lmst_Of (UT);

    L := [X => RCPHI * CS (LMST),
          Y => RCPHI * SN (LMST),
          Z => RSPHI];

    T := G - L;

    POLAR (T, R, DEC, RA);

    return Space.Direction_Of (Dec => DEC,
                               Ra  => RA);
  end Direction_Of;


  procedure Execute is
  begin
    Put_Line ("Moon Test");
    Site.Define ((Latitude  => Angle.Value_Of ("47d42'19.79"""),
                  Longitude => Angle.Value_Of ("8d36'35.50"""),
                  Elevation => 540)); -- in meters
    declare
      Direction : constant Space.Direction := Direction_Of (Time.Universal);
    begin
      Put_Line ("Direction: " & Text.Ansi_Of_Utf8 (Space.Image_Of (Direction)));
    end;
  exception
  when Occurrence: others =>
    Put_Line (Exceptions.Information_Of (Occurrence));
  end Execute;

end Test;
