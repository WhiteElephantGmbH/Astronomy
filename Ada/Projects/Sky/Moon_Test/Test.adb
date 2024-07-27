-- *********************************************************************************************************************
-- *                            (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                             *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Angle;
with Astro;
with Database.Moon;
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

  use Astro;
  use MATLIB;
  use SPHLIB;
  use SUNLIB;

  -------------------
  -- FRAC: Fraction
  -------------------
  function FRAC (X:REAL) return REAL is
    VX : REAL := X-REAL'truncation(X);
  begin
    if VX<0.0 then
      VX:=VX+1.0;
    end if;
    return VX;
  end FRAC;

  package Numeric is new Ada.Numerics.Generic_Elementary_Functions (REAL);

  function SIN (X : REAL) return REAL renames Numeric.Sin;

  function COS (X : REAL) return REAL renames Numeric.Cos;

  ER : constant REAL := Earth_Equatorial_Radius;
  MR : constant REAL := Moon_Radius;
  AE : constant REAL := Astronomical_Unit;


  procedure Reduce (Item : in out REAL) is
  begin
    while Item < 180.0 loop
      Item := @ + 360.0;
    end loop;
    while Item > 180.0 loop
      Item := @ - 360.0;
    end loop;
  end Reduce;


  function RED (Item : REAL) return REAL is
    Result : REAL := Item;
  begin
    Reduce (Result);
    if Result < 0.0 then
      Result := @ + 360.0;
    end if;
    return Result;
  end RED;

  P2     : constant := 2.0 * Ada.Numerics.Pi;
  RADDEC : constant := 180.0 / Ada.Numerics.Pi;

  procedure NUT (T            :     REAL;
                 EPS,DEPS,DPSI: out REAL) is -- in RAD
    ARC : constant := 206264.8062; -- Bogensekunden pro radian = 3600*180/pi
    LS,D,F,N: REAL;
  begin
    LS  := P2*FRAC(0.993133+  99.997306*T);   -- mittl. Anomalie Sonne
    D   := P2*FRAC(0.827362+1236.853087*T);   -- Diff. Laenge Mond-Sonne
    F   := P2*FRAC(0.259089+1342.227826*T);   -- Knotenabstand
    N   := P2*FRAC(0.347346-   5.372447*T);   -- Laenge des aufst.Knotens
    EPS := 0.4090928-2.2696E-4*T;             -- Ekliptikschiefe
    DPSI := ( -17.200*SIN(N) - 1.319*SIN(2.0*(F-D+N)) - 0.227*SIN(2.0*(F+N))
              + 0.206*SIN(2.0*N) + 0.143*SIN(LS) ) / ARC;
    DEPS := ( + 9.203*COS(N) + 0.574*COS(2.0*(F-D+N)) + 0.098*COS(2.0*(F+N))
              - 0.090*COS(2.0*N) ) / ARC;
  end NUT;


  ------------------------------------------------------
  --          Berechnung der Mond Parameter           --
  -- nach Jean Meeus - Astronomische Algorithmen 1998 --
  ------------------------------------------------------
  procedure MOONPAR (T    :     REAL; -- T  : Zeit in Jahrhunderten seit dem Jahr 2000
                     RA   :     REAL; -- α  : topocentrische RA of moon center
                     LAM  :     REAL; -- λ  : scheinbare topocentrische Länge des Mondes
                     BET  :     REAL; -- β  : scheinbare topocentrische Breite des Mondes
                     DEL  :     REAL; -- Δ  : Distanz zum Mond in km
                     L    : out REAL; -- l  : Liberation in Länge
                     B    : out REAL; -- b  : Liberation in Breite
                     B0   : out REAL; -- b0 : subsolare Breite
                     C0   : out REAL; -- c0 : Colongitude
                     P    : out REAL) -- P  : Positionswinkel der Achse
  is
    EPS0 : REAL; -- ε0: mittlere Schiefe der Ekliptik
    DEPS : REAL; -- Δε: Nutation Delta Epsilon
    DPSI : REAL; -- Δψ: Nutation Delta Psi
    SL   : REAL; -- Solare Länge
    SB   : REAL; -- Solare Breite
    SR   : REAL; -- Distanz zur Sonne in km
  begin
    NUT (T,EPS0,DEPS,DPSI);
    EPS0 := EPS0 * RADDEC;
    DEPS := DEPS * RADDEC;
    DPSI := DPSI * RADDEC;

    SUN200 (T,SL,SB,SR);
    SR := SR*AE;

    declare
      T2 : constant REAL := T * T;
      T3 : constant REAL := T * T2;
      T4 : constant REAL := T * T3;

      EPS : constant REAL := EPS0+DEPS; -- wahre Schiefe der Ekliptik

      -- mittlere Elongation des Mondes -> Kapitel 45.2
      D : constant REAL := RED(297.850_1921+445_267.111_4034*T-0.001_8819*T2+T3/545_868.0-T4/113_065_000.0); -- 1998
    --D : constant REAL := RED(297.850_2042+445_267.111_5168*T-0.001_6300*T2+T3/545_868.0-T4/113_065_000.0); -- 1992

      -- mittlere Anomalie der Sonne -> Kapitel 45.3
      MS : constant REAL := RED(357.529_1092 + 35_999.050_2909 * T - 0.000_1535 * T2 + T3 / 24_490_000.0); -- 1998
    --MS : constant REAL := RED(357.529_1092 + 35_999.050_2909 * T - 0.000_1536 * T2 + T3 / 24_490_000.0); -- 1992

      -- mittlere Anomalie des Mondes -> Kapitel 45.4
      MM : constant REAL := RED(134.963_3964+477_198.867_5055*T+0.008_7414*T2+T3/69_699.0-T4/14_712_000.0); -- 1998
    --MM : constant REAL := RED(134.963_4114+477_198.867_6313*T+0.008_9970*T2+T3/69_699.0-T4/14_712_000.0); -- 1992

      -- mittlerer Abstand des Mondes von seinem aussteigenden Knoten -> Kapitel 45.5
      F : constant REAL := RED(93.272_095+483_202.017_5233*T-0.003_6539*T2-T3/3_526_000.0+T4/863_310_000.0);  -- 1998
    --F : constant REAL := RED(93.272_0993+483_202.017_5273*T-0.003_4029*T2-T3/3_526_000.0+T4/863_310_000.0); -- 1992

      --> Kapitel 45.6
      E: constant REAL := RED(1.0-0.002_516*T-0.000_0074*T2);

      -- Länge des mittleren aufsteigenden Knotens -> Kapitel 45.7
      OME : constant REAL := RED(125.044_5579-1934.136_2891*T+0.002_0762*T2+T3/467_410.0-T4/60_616_000.0); -- 1998
    --OME : constant REAL := RED(125.044_5550-1934.136_1849*T+0.002_0762*T2+T3/467_410.0-T4/60_616_000.0); -- 1992


      --  Ephemerde zur physischen Beobachtung (Kapitel 51)
      --  -------------------------------------------------

      I  : constant REAL := 1.54242; -- Neigung des mittleren Mondäquators gegen die Ekliptik
      SI : constant REAL := SN(I);
      CI : constant REAL := CS(I);

      -- optische Liberation in Länge und Breite

      W : constant REAL := RED(LAM-DPSI-OME);
      A : constant REAL := RED(ATN2(SN(W)*CS(BET)*CI-SN(BET)*SI,CS(W)*CS(BET)));

      L1 : constant REAL := A-F;
      B1 : constant REAL := ASN(-SN(W)*CS(BET)*SI-SN(BET)*CI);

      -- physische Liberation in Länge und Breite
      K1 : constant REAL := 119.75+131.849*T;
      K2 : constant REAL := 72.56+20.186*T;

      SIG : constant REAL := -0.028_16*SN(MM)+0.022_44*CS(F)-0.006_82*SN(MM-2.0*F)-0.002_79*SN(2.0*F)
                             -0.000_83*SN(2.0*(F-D))+0.000_69*SN(MM-2.0*D)+0.000_4*CS(MM+F)-0.000_25*SN(2.0*MM)
                             -0.000_23*SN(MM+2.0*F)+0.000_2*CS(MM-F)+0.000_19*SN(MM-F)+0.000_13*SN(MM+2.0*(F-D))
                             -0.000_1*CS(MM-3.0*F);

      RHO : constant REAL := -0.027_52*CS(MM)-0.022_45*SN(F)+0.006_84*CS(MM-2.0*F)-0.002_93*CS(2.0*F)
                             -0.000_85*CS(2.0*(F-D))-0.000_54*CS(MM-2.0*D)-0.000_2*SN(MM+F)-0.000_2*CS(MM+2.0*F)
                             -0.000_2*CS(MM-F)+0.000_14*CS(MM+2.0*(F-D));

      TAU : constant REAL := +0.025_2 *SN(MS)*E+0.004_73*SN(2.0*(MM-F))-0.004_67*SN(MM)+0.003_96*SN(K1)
                             +0.002_76*SN(2.0*(MM-D))+0.001_96*SN(OME)-0.001_83*CS(MM-F)+0.001_15*SN(MM-2.0*D)
                             -0.000_96*SN(MM-D)+0.000_46*SN(2.0*(F-D))-0.000_39*SN(MM-F)-0.000_32*SN(MM-MS-D)
                             +0.000_27*SN(2.0*(MM-D)-MS)+0.000_23*SN(K2)-0.000_14*SN(2.0*D)+0.000_14*CS(2.0*(MM-F))
                             -0.000_12*SN(MM-2.0*F)-0.000_12*SN(2.0*MM)+0.000_11*SN(2.0*(MM-MS-D));

      L2 : constant REAL := -TAU+(RHO*CS(A)+SIG*SN(A))*TN(B1);
      B2 : constant REAL := SIG*CS(A)-RHO*SN(A);

      -- Positionswinkel der Achse
      SIR : constant REAL := SN(I+RHO);
      V   : constant REAL := OME+DPSI+SIG/SI;
      Y   : constant REAL := SIR*SN(V);
      X   : constant REAL := SIR*CS(V)*CS(EPS)-CS(I+RHO)*SN(EPS);
      WA  : constant REAL := RED(ATN2(Y,X));

      -- selenografische Position der Sonne
      LH  : constant REAL := SL+180.0+DEL/SR*57.296*CS(BET)*SN(SL-LAM);
      BH  : constant REAL := DEL/SR*BET;
      WH  : constant REAL := RED(LH-OME);
      AH  : constant REAL := RED(ATN2(SN(WH)*CS(BH)*CI-SN(BH)*SI,CS(WH)*CS(BH)));
      L10 : constant REAL := AH-F;
      B10 : constant REAL := ASN(-SN(WH)*CS(BH)*SI-SN(BH)*CI);
      L20 : constant REAL := -TAU+(RHO*CS(AH)+SIG*SN(AH))*TN(B10);
      B20 : constant REAL := SIG*CS(AH)-RHO*SN(AH);

    begin -- MOONPAR
      Put_Line ("T  =" & T'image);
      Put_Line ("RA =" & RA'image);
      Put_Line ("LAM=" & LAM'image);
      Put_Line ("BET=" & BET'image);
      Put_Line ("DEL=" & DEL'image);
      Put_Line ("D  =" & D'image);
      Put_Line ("M  =" & MS'image);
      Put_Line ("M' =" & MM'image);
      Put_Line ("F  =" & F'image);
      Put_Line ("E  =" & E'image);
      Put_Line ("OME=" & OME'image);
      Put_Line ("W  =" & W'image);
      Put_Line ("A  =" & A'image);
      Put_Line ("EPS=" & EPS'image);
      Put_Line ("De =" & DEPS'image);
      Put_Line ("Dp =" & DPSI'image);
      Put_Line ("K1 =" & K1'image);
      Put_Line ("K2 =" & K2'image);
      Put_Line ("ROH=" & RHO'image);
      Put_Line ("SIG=" & SIG'image);
      Put_Line ("TAU=" & TAU'image);
      Put_Line ("L1 =" & L1'image);
      Put_Line ("B1 =" & B1'image);
      Put_Line ("L2 =" & L2'image);
      Put_Line ("B2 =" & B2'image);
      Put_Line ("V  =" & V'image);
      Put_Line ("X  =" & X'image);
      Put_Line ("Y  =" & Y'image);
      Put_Line ("WA =" & WA'image);
      Put_Line ("SL =" & SL'image);
      Put_Line ("SR =" & SR'image);
      Put_Line ("LH =" & LH'image);
      Put_Line ("BH =" & BH'image);
      Put_Line ("WH =" & WH'image);
      Put_Line ("AH =" & AH'image);
      Put_Line ("L10=" & L10'image);
      Put_Line ("B10=" & B10'image);
      Put_Line ("L20=" & L20'image);
      Put_Line ("B20=" & B20'image);

      L := L1 + L2;
      B := B1 + B2;
      Put_Line ("L =" & L'image);
      Put_Line ("B =" & B'image);

      P := ASN((SQRT(X*X+Y*Y)*CS(RA-WA))/CS(B));
      Put_Line ("P  =" & P'image);

      B0 := B10 + B20;
      C0 := RED(90.0-(L10 + L20));
      Put_Line ("B0 =" & B0'image);
      Put_Line ("C0 =" & C0'image);
    end;
  end MOONPAR;


  procedure MOONSUNH (ETA  :     REAL; -- η  : selenographische Länge
                      TET  :     REAL; -- θ  : selenographische Breite
                      B0   :     REAL; -- T  : Zeit in Jahrhunderten seit dem Jahr 2000
                      C0   :     REAL; -- α  : geozentrische rektazension des Mondes
                      H    : out REAL) -- H  : Höhe der Sonne
  is
  begin
    H := ASN(SN(B0)*SN(TET)+CS(B0)*CS(TET)*SN(C0+ETA));
  end MOONSUNH;


  procedure Rotate (Alpha, Beta : in out REAL;
                    By          :        REAL) is
    CA : constant REAL := CS (Alpha);
    SA : constant REAL := SN (Alpha);
    CB : constant REAL := CS (Beta);
    SB : constant REAL := SN (Beta);
    CR : constant REAL := CS (By);
    SR : constant REAL := SN (By);
  begin
    Alpha := ATN2 (CB * SA, CA * CB * CR - SB * SR);
    Beta  := ASN (CR * SB + CA * CB * SR);
  end Rotate;



  procedure Get_Moon_Parameters (UT                          : Time.Ut;
                                 RA, Dec, R, L, B, P, B0, C0 : out REAL) is

    T, Bet, Lam, Rc_Phi, Rs_Phi, LMST: REAL;

    Geo: VECTOR; -- geozentric moon vector
    Loc: VECTOR; -- Location vector
    Top: VECTOR; -- Topocentric Vector
    Tec: VECTOR; -- Topocentric ecliptical Vector

    use all type Angle.Value;

  begin -- Get_Moon_Parameters
    T := Time.Tet_Of (UT);

    MOOLIB.MOONEQU (T, RA, Dec, R);

    Geo := CART (R, Dec, RA);

    SPHLIB.SITE (PHI   => +Site.Latitude,
                 RCPHI => Rc_Phi,
                 RSPHI => Rs_Phi);

    LMST := +Time.Lmst_Of (UT);

    Loc := [X => Rc_Phi * CS (LMST),
            Y => Rc_Phi * SN (LMST),
            Z => Rs_Phi];

    Top := Geo - Loc;

    Tec := Top;
    EQUECL (T, Tec(X), Tec(Y), Tec(Z));

    POLAR (Top, R, Dec, RA);
    POLAR (Tec, R, Bet, Lam);
    R := R * ER;

    MOONPAR (T    => T,
             RA   => RA,
             LAM  => Lam,
             BET  => Bet,
             DEL  => R,
             L    => L,
             B    => B,
             P    => P,
             B0   => B0,
             C0   => C0);
  end Get_Moon_Parameters;


  Maximum_Sun_Altitude : constant Angle.Degrees := 60.0;

  RA, Dec, R, L, B, P, B0, C0: REAL;


  procedure Define (UT: Time.Ut) is
  begin
    Get_Moon_Parameters (UT  => UT,
                         RA  => RA,
                         Dec => Dec,
                         R   => R,
                         L   => L,
                         B   => B,
                         P   => P,
                         B0  => B0,
                         C0  => C0);
  end Define;


  subtype Feature_Name is Database.Moon.Feature_Name;

  Sun_Altitude : REAL;

  use type Angle.Value;

  function Img (Value : REAL;
                Size  : Positive := 11) return String is
    type Flt is delta 0.000001 range -1000000.0 .. 1000000.0;
    Image : constant String := "       " & Flt(Value)'image;
  begin
    return Image (Image'last - Size + 1 .. Image'last);
  end Img;


  procedure Calculate_Deltas (D, B, L, R, P : REAL) is
    X, Xo, Yo, Zo, Wo, M, N, T, Gam, Del, Lam, Ome : REAL;
  begin
    Put_Line ("D  :" & Img (D));
    Put_Line ("B  :" & Img (B));
    Put_Line ("L  :" & Img (L));
    Put_Line ("R  :" & Img (R));
    Put_Line ("P  :" & Img (P));
    CART (R     => R,
          THETA => B,
          PHI   => L,
          X     => Xo,
          Y     => Yo,
          Z     => Zo);
    Put_Line ("Xo :" & Img (Xo));
    Put_Line ("Yo :" & Img (Yo));
    Put_Line ("Zo :" & Img (Zo));
    X := (D - Xo);
    T := X * X + Yo * Yo;
    N := SQRT (T);
    M := SQRT (T + Zo * Zo);
    Put_Line ("M  :" & Img (M));
    Put_Line ("N  :" & Img (N));
    Gam := ATN2 (Zo, N);
    Del := ATN2 (Yo, X);
    Put_Line ("Gam:" & Img (Gam));
    Put_Line ("Del:" & Img (Del));
    Wo := SQRT (Yo * Yo + Zo * Zo);
    Put_Line ("Wo :" & Img (Wo));
    Lam := ATN2 (Zo, Yo);
    Put_Line ("Lam:" & Img (Lam));
    Ome := Lam - P;
    Put_Line ("Ome:" & Img (Ome));
    Yo := Wo * CS (Ome);
    Zo := Wo * SN (Ome);
    Put_Line ("Yo':" & Img (Yo));
    Put_Line ("Zo':" & Img (Zo));
    Gam := ASN (Zo / M);
    Del := ATN2 (Yo, X);
    Put_Line ("The:" & Img (Gam));
    Put_Line ("Iot:" & Img (Del));
  end Calculate_Deltas;


  procedure Evaluate_Deltas (D, B, L, R, P       : REAL;
                             Delta_Ra, Delta_Dec : out REAL) is
    X, Xo, Yo, Zo, Wo, M, T, Ome : REAL;
  begin
    CART (R     => R,
          THETA => B,
          PHI   => L,
          X     => Xo,
          Y     => Yo,
          Z     => Zo);
    X := (D - Xo);
    T := Yo * Yo + Zo * Zo;
    M := SQRT (X * X + T);
    Wo := SQRT (T);
    Ome := ATN2 (Zo, Yo) + P;
    Delta_Dec := ASN (Wo * SN (Ome) / M);
    Delta_Ra := -ATN2 (Wo * CS (Ome), X); -- west positive
  end Evaluate_Deltas;



  function Direction_Of (Feature : Feature_Name) return Space.Direction is

    function Normalized (Item : REAL) return REAL is
    begin
      if Item < -180.0 then
        return Item + 360.0 ;
      elsif Item > 180.0 then
        return Item - 360.0;
      end if;
      return Item;
    end Normalized;

    Liberation_L : constant REAL := L;
    Liberation_B : constant REAL := B;

    Feature_RA  : REAL;
    Feature_Dec : REAL;

  begin -- Direction_Of
    declare
      Item         : Database.Moon.Feature renames Database.Moon.List(Feature);
      Longitude    : constant REAL := REAL(Item.Longitude);
      Latitude     : constant REAL := REAL(Item.Latitude);
      CL, CB       : REAL;
      Delta_RA     : REAL;
      Delta_Dec    : REAL;
      X, MRS, DM, SX, SY, SR, SA : REAL;
    begin
      MOONSUNH (ETA => Longitude,
                TET => Latitude,
                B0  => B0,
                C0  => C0,
                H   => Sun_Altitude);
      Log.Write ("Sun_Altitude :" & Angle.Signed_Degrees_Image_Of (+Sun_Altitude));
      if Sun_Altitude < -2.0 or else Sun_Altitude > Maximum_Sun_Altitude then
        return Space.Unknown_Direction;
      end if;
      CL := Longitude - Liberation_L;
      CB := Latitude;
      Rotate (CL, CB, - Liberation_B);
      MRS := ASN (MR / R);
      DM := 90.0 - MRS;
      if CL /= Normalized (CL) then
        raise Program_Error;
      end if;
      if abs CL > DM then
        return Space.Unknown_Direction;
      end if;
      if CB /= Normalized (CB) then
        raise Program_Error;
      end if;
      if abs CB > DM then
        return Space.Unknown_Direction;
      end if;
      CART (MRS, CB, CL, X, SX, SY);
      SR := SQRT (SX * SX + SY * SY);
      SA := ATN2 (SY, SX) + P;
      Delta_RA := -SR * CS (SA); -- RA west positive
      Delta_Dec := SR * SN (SA);
      Log.Write ("Old Delta_RA  :" & Angle.Signed_Degrees_Image_Of (+Delta_RA));
      Log.Write ("Old Delta_Dec :" & Angle.Signed_Degrees_Image_Of (+Delta_Dec));
      Evaluate_Deltas (D        => R,
                       B        => CB,
                       L        => CL,
                       R        => MR,
                       P        => P,
                       Delta_Ra => Delta_RA,
                       Delta_Dec => Delta_Dec);
      Log.Write ("New Delta_RA  :" & Angle.Signed_Degrees_Image_Of (+Delta_RA));
      Log.Write ("New Delta_Dec :" & Angle.Signed_Degrees_Image_Of (+Delta_Dec));
      Feature_RA := RA + Delta_RA;
      Feature_Dec := Dec + Delta_Dec;
    end;

    return Space.Direction_Of (Dec => Feature_Dec,
                               Ra  => Feature_RA);
  end Direction_Of;


  procedure Execute is

    procedure Test_Rotate (L, B, LB : REAL) is
      Alpha : REAL := L;
      Beta  : REAL := B;
    begin
      Put_Line ("");
      Put_Line ("Alpha      : " & Img (Alpha));
      Put_Line ("Beta       : " & Img (Beta));
      Put_Line ("Rotated by : " & Img (LB));
      Rotate (Alpha, Beta, By => LB);
      Put_Line ("Alpha rotated : " & Img (Alpha));
      Put_Line ("Beta rotated  : " & Img (Beta));
    end Test_Rotate;

    Delta_Ra, Delta_Dec : REAL;

  begin -- Execute
    Put_Line ("*********************");
    Put_Line ("* M O O N   T E S T *");
    Put_Line ("*********************");
    Put_Line ("");
    Site.Define ((Latitude  => Angle.Value_Of ("47d42'19.79"""),
                  Longitude => Angle.Value_Of ("8d36'35.50"""),
                  Elevation => 540)); -- in meters
    Put_Line ("");
    Put_Line ("D E F I N E   M O O N   P A R A M E T E R S");
    Put_Line ("===========================================");
  --Define (Time.Universal);
  --Define (Time.Universal_Of (Time.Calendar_Value_Of ("12.4.1992 00:00:00")));
    Define (Time.Universal_Of (Time.Calendar_Value_Of ("18.7.2024 22:18:00")));
    Put_Line ("");
    Put_Line ("V I S I B L E   F E A T U R E S");
    Put_Line ("===============================");
    for The_Feature in Feature_Name loop
      declare
        Direction : constant Space.Direction := Direction_Of (The_Feature);
      begin
        if Space.Direction_Is_Known (Direction) then
          Put_Line (The_Feature'image & Text.Ansi_Of_Utf8 (Space.Image_Of (Direction))
                                      & " - SH:" & Angle.Signed_Degrees_Image_Of (+Sun_Altitude));
        end if;
      end;
    end loop;
    Put_Line ("");
    Put_Line ("D E L T A    R A   &   D E C");
    Put_Line ("============================");
    Calculate_Deltas (D => 350.0, B => 61.0, L => -71.0, R => 100.0, P => 52.0);
    Evaluate_Deltas (D        => 350.0,
                     B        => 61.0,
                     L        => -71.0,
                     R        => 100.0,
                     P        => 52.0,
                     Delta_Ra => Delta_Ra,
                     Delta_Dec => Delta_Dec);
    Put_Line ("Delta Dec : " & Img (Delta_Dec));
    Put_Line ("Delta RA  : " & Img (Delta_Ra));
    Put_Line ("");
    Put_Line ("R O T A T E");
    Put_Line ("===========");
    Test_Rotate (L => 43.0, B => 28.0, LB => 9.0);
    Test_Rotate (L => 0.0, B => 0.0, LB => 4.0);
    Test_Rotate (L => 0.0, B => 0.0, LB => -4.0);
    Test_Rotate (L => 0.0, B => 10.0, LB => 4.0);
    Test_Rotate (L => 0.0, B => -10.0, LB => -4.0);
    Test_Rotate (L => 60.0, B => 10.0, LB => 5.0);
    Test_Rotate (L => 60.0, B => 0.0, LB => 5.0);
    Test_Rotate (L => 60.0, B => -10.0, LB => -5.0);
    Test_Rotate (L => -60.0, B => 10.0, LB => 5.0);
    Test_Rotate (L => -60.0, B => 0.0, LB => 5.0);
    Test_Rotate (L => -60.0, B => -10.0, LB => -5.0);
  exception
  when Occurrence: others =>
    Put_Line (Exceptions.Information_Of (Occurrence));
  end Execute;

end Test;
