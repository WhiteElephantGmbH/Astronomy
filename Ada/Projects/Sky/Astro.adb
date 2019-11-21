-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Numerics.Generic_Elementary_Functions;

package body Astro is

  P2 : constant := 2.0 * Ada.Numerics.Pi;

  function "+" (Left, Right : VECTOR) return VECTOR is
  begin
    return (Left(X)+Right(X), Left(Y)+Right(Y), Left(Z)+Right(Z));
  end "+";

  function "-" (Left, Right : VECTOR) return VECTOR is
  begin
    return (Left(X)-Right(X), Left(Y)-Right(Y), Left(Z)-Right(Z));
  end "-";

  package Numeric is new Ada.Numerics.Generic_Elementary_Functions (REAL);

  function ARCTAN (X : REAL; Y : REAL := 1.0) return REAL renames Numeric.Arctan;

  function EXP (X : REAL) return REAL renames Numeric.Exp;

  function LN (X : REAL) return REAL renames Numeric.Log;

  function SQRT (X : REAL) return REAL renames Numeric.Sqrt;

  function SIN (X : REAL) return REAL renames Numeric.Sin;

  function COS (X : REAL) return REAL renames Numeric.Cos;

  ----------------------
  -- TRUNC: Truncation
  ----------------------
  function TRUNC (X : REAL) return Integer is
  begin
    return Integer(REAL'truncation(X));
  end TRUNC;

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


  --==========
  -- MATHLIB
  --==========

  package body MATLIB is

    RAD : constant REAL := 0.0174532925199433;
    C   : constant REAL := 90.0;

    -------------------------------------------
    -- ACS: Arcus-Cosinus-Funktion (Gradmass)
    -------------------------------------------
    function ACS (X : REAL) return REAL is
      EPS : constant REAL := 1.0E-7;
    begin
      if abs(X)=1.0 then
        return C-X*C;
      elsif (abs(X)>EPS) then
        return C-ARCTAN(X/SQRT((1.0-X)*(1.0+X)))/RAD;
      else
        return C-X/RAD;
      end if;
    end ACS;

    -----------------------------------------
    -- ASN: Arcus-Sinus-Funktion (Gradmass)
    -----------------------------------------
    function ASN (X:REAL) return REAL is
      EPS : constant REAL := 1.0E-7;
    begin
      if abs(X)=1.0 then
        return 90.0*X;
      elsif (abs(X)>EPS) then
        return ARCTAN(X/SQRT((1.0-X)*(1.0+X)))/RAD;
      else
        return X/RAD;
      end if;
    end ASN;

    -------------------------------------------
    -- ATN: Arcus-Tangens-Funktion (Gradmass)
    -------------------------------------------
    function ATN (X:REAL) return REAL is
    begin
      return ARCTAN(X)/RAD;
    end ATN;

    -----------------------------------------------------------------
    -- ATN2: Arcus-Tangens von y/x fuer zwei Argumente
    --       (quadrantenrichtig mit -180 Grad <= ATN2 <= +180 Grad)
    -----------------------------------------------------------------
    function ATN2 (Y,X:REAL) return REAL is
      AX,AY,PHI: REAL;
    begin
      if (X=0.0) and (Y=0.0) then
        return 0.0;
      end if;
      AX:=abs(X); AY:=abs(Y);
      if AX>AY then
        PHI:=ARCTAN(AY/AX)/RAD;
      else
        PHI:=90.0-ARCTAN(AX/AY)/RAD;
      end if;
      if X<0.0 then
        PHI:=180.0-PHI;
      end if;
      if Y<0.0 then
        PHI:=-PHI;
      end if;
      return PHI;
    end ATN2;

    -----------------------------------------------------------------------
    -- CART: Umwandlung von Polarkoordinaten (r,theta,phi)
    --       in kartesische Koordinaten (x,y,z)
    --       ( theta = [-90 Grad,+90 Grad]; phi = [-360 Grad,+360 Grad] )
    -----------------------------------------------------------------------
    procedure CART (R,THETA,PHI:     REAL;
                    X,Y,Z      : out REAL) is
      RCST : REAL;
    begin
      RCST := R*CS(THETA);
      X    := RCST*CS(PHI); Y := RCST*SN(PHI); Z := R*SN(THETA);
    end CART;

    function CART (R,THETA,PHI: REAL) return VECTOR is
      V : VECTOR;
    begin
      CART (R, THETA, PHI, V(X), V(Y), V(Z));
      return V;
    end CART;

    ----------------------------------------
    -- CROSS: Kreuzprodukt zweier Vektoren
    ----------------------------------------
    procedure CROSS (A,B:     VECTOR;
                     C  : out VECTOR) is
    begin
      C(X) := A(Y)*B(Z)-A(Z)*B(Y);
      C(Y) := A(Z)*B(X)-A(X)*B(Z);
      C(Z) := A(X)*B(Y)-A(Y)*B(X);
    end;

    ------------------------------------
    -- CS: Cosinus-Funktion (Gradmass)
    ------------------------------------
    function CS (X:REAL) return REAL is
    begin
      return COS(X*RAD);
    end CS;

    ------------------------
    -- CUBR: dritte Wurzel
    ------------------------
    function CUBR (X:REAL) return REAL is
    begin
      if X=0.0 then
        return 0.0;
      end if;
      return EXP(LN(X)/3.0);
    end CUBR;

    ------------------------------------------------------------------------------
    -- DDD: Umwandlung von Grad, Minuten und Sekunden in Bruchteile eines Grades
    ------------------------------------------------------------------------------
    procedure DDD (D,M:     Integer;
                   S  :     REAL;
                   DD : out REAL) is
      SIGN: REAL;
    begin
      if ( (D<0) or (M<0) or (S<0.0) ) then
        SIGN:=-1.0;
      else
        SIGN:=1.0;
      end if;
      DD:=SIGN*(REAL(abs(D))+REAL(abs(M)/60)+abs(S)/3600.0);
    end DDD;

    ----------------------------------------------------------------------------
    -- DMS: Umwandlung von Bruchteilen eines Grades in Grad, Minuten, Sekunden
    ----------------------------------------------------------------------------
    procedure DMS (DD :     REAL;
                   D,M: out Integer;
                   S  : out REAL) is
      D1:REAL;
    begin
      D1:=abs(DD);  D:=TRUNC(D1);
      D1:=(D1-REAL(D))*60.0;  M:=TRUNC(D1);  S:=(D1-REAL(M))*60.0;
      if (DD<0.0) then
        if (D/=0) then
          D:=-D;
        elsif (M/=0) then
          M:=-M;
        else
          S:=-S;
        end if;
      end if;
    end DMS;

    ----------------------------------------
    -- DOT: Skalarprodukt zweier Vektoren
    ----------------------------------------
    function DOT (A,B:VECTOR) return REAL is
    begin
      return A(X)*B(X)+A(Y)*B(Y)+A(Z)*B(Z);
    end DOT;

    --------------------------------------------------------------------------
    -- LSQFIT: Loesung des Gleichungssystems
    --         A[i,1]*s[1]+...A[i,m]*s[m] - A[i,m+1] = 0   (i=1,..,n)
    --         nach der Methode der kleinsten Quadrate mit Givens-Rotationen
    --      A: Koeffizientenmatrix
    --      N: Zahl der Gleichungen  (Zeilen von A)
    --      M: Zahl der Unbekannten  (M+1=Spalten von A, M=Laenge von S)
    --      S: Loesungsvektor
    --------------------------------------------------------------------------
    procedure LSQFIT (A   :     LSQMAT;
                      N, M:     Integer;
                      S   : out LSQVEC) is
      EPS  : constant := 1.0E-10; -- Rechnergenauigkeit
      VA   : LSQMAT := A;
      P,Q,H: REAL;
    begin
      for J in 1 .. M loop -- Schleife ueber die Spalten 1..M von A
        -- eliminiere Elemente A[i,j] mit i>j aus Spalte j
        for I in J+1 .. N loop
          if VA(I,J)/=0.0 then
            -- P, q und neues A[j,j] berechnen; A[i,j]=0 setzen
            if abs(VA(J,J))<EPS*abs(VA(I,J)) then
              P:=0.0; Q:=1.0; VA(J,J):=-VA(I,J); VA(I,J):=0.0;
            else
              H:=SQRT(VA(J,J)*VA(J,J)+VA(I,J)*VA(I,J));
              if VA(J,J)<0.0 then
                H:=-H;
              end if;
              P:=VA(J,J)/H; Q:=-VA(I,J)/H; VA(J,J):=H; VA(I,J):=0.0;
            end if;
            -- Rest der Zeile bearbeiten
            for K in J+1 .. M+1 loop
              begin
                H       := P*VA(J,K) - Q*VA(I,K);
                VA(I,K) := Q*VA(J,K) + P*VA(I,K);
                VA(J,K) := H;
              end;
            end loop;
          end if;
        end loop;
      end loop;
      -- Ruecksubstitution
      for I in reverse 1 .. M loop
        S(I) := 0.0; -- keep compiler happy
        H:=VA(I,M+1);
        for K in I+1 .. M loop
          H:=H-VA(I,K)*S(K);
        end loop;
        S(I) := H/VA(I,I);
      end loop;
    end LSQFIT;

    -------------------------------
    -- NORM: Betrag eines Vektors
    -------------------------------
    function NORM (A:VECTOR) return REAL is
    begin
      return SQRT(DOT(A,A));
    end NORM;

    --------------------------------------------------------------------
    -- POLAR: Umwandlung von kartesischen Koordinaten (x,y,z)
    --        in Polarkoordinaten (r,theta,phi)
    --        ( theta = [-90 Grad,+90 Grad]; phi = [0 Grad,+360 Grad])
    --------------------------------------------------------------------
    procedure POLAR (X,Y,Z      :     REAL;
                     R,THETA,PHI: out REAL) is
      RHO: REAL;
    begin
      RHO:=X*X+Y*Y;  R:=SQRT(RHO+Z*Z);
      PHI:=ATN2(Y,X);
      if PHI<0.0 then
        PHI:=PHI+360.0;
      end if;
      RHO:=SQRT(RHO); THETA:=ATN2(Z,RHO);
    end POLAR;

    procedure POLAR (V          :     VECTOR;
                     R,THETA,PHI: out REAL) is
    begin
      POLAR (V(X), V(Y), V(Z), R, THETA, PHI);
    end POLAR;

    -------------------------------------------------------------------------
    -- QUAD: bestimmt die Parabel durch 3 Punkte
    --       (-1,Y_MINUS), (0,Y_0) und (1,Y_PLUS),
    --       die nicht auf einer Geraden liegen.
    --
    --      Y_MINUS,Y_0,Y_PLUS: drei y-Werte
    --      XE,YE   : x und y Wert des Extremums der Parabel
    --      ZERO1   : erste Nullstelle im Intervall [-1,+1] (fuer NZ=1,2)
    --      ZERO2   : zweite Nullstelle im Intervall [-1,+1] (fuer NZ=2)
    --      NZ      : Zahl der Nullstellen der Parabel im Intervall [-1,+1]
    -------------------------------------------------------------------------
    procedure QUAD (Y_MINUS,Y_0,Y_PLUS:     REAL;
                    XE,YE,ZERO1,ZERO2 : out REAL;
                    NZ                : out Integer) is
      A,B,C,DIS,DX: REAL;
    begin
      NZ := 0;
      A  := 0.5*(Y_MINUS+Y_PLUS)-Y_0; B := 0.5*(Y_PLUS-Y_MINUS); C := Y_0;
      XE := -B/(2.0*A); YE := (A*XE + B) * XE + C;
      DIS := B*B - 4.0*A*C; -- Diskriminante von y = axx+bx+c
      if (DIS >= 0.0) then  -- Parabel hat Nullstellen
        DX := 0.5*SQRT(DIS)/abs(A); ZERO1 := XE-DX; ZERO2 := XE+DX;
        if abs(ZERO1) <= +1.0 then
          NZ := NZ + 1;
        end if;
        if abs(ZERO2) <= +1.0 then
          NZ := NZ + 1;
        end if;
        if ZERO1<-1.0 then
          ZERO1:=ZERO2;
        end if;
      end if;
    end QUAD;

    ----------------------------------
    -- SN: Sinus-Funktion (Gradmass)
    ----------------------------------
    function SN (X:REAL) return REAL is
    begin
      return SIN(X*RAD);
    end SN;

    ------------------------------------------------------------------------------
    -- T_EVAL: Berechnet Funktionswerte fuer eine im Intervall [F.A,F.B] durch
    --         eine Tschebyscheff-Entwickl. vom Grad F.M approximierte Funktion.
    --   F: Tschebyscheff-Polynom
    --   X: Argument
    ------------------------------------------------------------------------------
    function T_EVAL (F: TPOLYNOM; X: REAL) return REAL is
      F1,F2,OLD_F1,XX,XX2 : REAL;
    begin
      if ( (X<F.A) or (F.B<X) ) then
        raise Program_Error; -- T_EVAL : x nicht in [a,b]
      end if;
      F1 := 0.0;  F2 := 0.0;
      XX := (2.0*X-F.A-F.B)/(F.B-F.A);  XX2 := 2.0*XX;
      for I in reverse 1 .. F.M loop
        OLD_F1 := F1; F1 := XX2*F1-F2+F.C(I);  F2 := OLD_F1;
      end loop;
      return XX*F1-F2+0.5*F.C(0);
    end T_EVAL;

    -----------------------------------------------------------------------------
    -- T_FIT_LBR: Tschebyscheff-Entwicklung der Koordinaten des Mondes oder
    --            eines Planeten (Reihen fuer Laenge, Breite und Radius).
    --
    --       POSITION: Prozedur zur Berechnung der Koordinaten L,B,R
    --       TA      : Startzeitpunkt des Entwicklungsintervalls
    --       TB      : Ende des Entwicklungsintervalls
    --       N       : Ordnung der Entwicklung; (N<=MAX_TP_DEG)
    --       L_POLY,B_POLY,R_POLY: Tschebyscheff-Polynome fuer L,B,R
    --
    -- Hinweise:
    --  . Das Intervall [TA,TB] muss kuerzer als ein Umlauf sein!
    --  . Es duerfen nur heliozentrische Planetenkoordinaten oder geozentrische
    --    Mondkoordinaten entwickelt werden!
    -----------------------------------------------------------------------------
    procedure T_FIT_LBR (Position_Access:          access procedure (T:REAL; LL,BB,RR: out REAL);
                         TA,TB:                    REAL;
                         N:                        Integer;
                         L_POLY,B_POLY,R_POLY: out TPOLYNOM) is
      PI   : constant := Ada.Numerics.Pi;
      NDIM : constant := 27; -- must be >= 2*MAX_TP_DEG+1
      FAC,BPA,BMA,PHI: REAL;
      T,H,L,B,R      : array (0..NDIM) of REAL;
    begin
      if (N>MAX_TP_DEG) then
        raise Program_Error; -- N zu gross in T_FIT_LBR
      end if;
      L_POLY.M := N;    B_POLY.M := N;    R_POLY.M := N;
      L_POLY.A := TA;   B_POLY.A := TA;   R_POLY.A := TA;
      L_POLY.B := TB;   B_POLY.B := TB;   R_POLY.B := TB;
      BMA := (TB-TA)/2.0;   BPA := (TB+TA)/2.0;
      FAC := 2.0/REAL(N+1);
      PHI:=PI/REAL(2*N+2);       -- H(K)=cos(pi*k/N/2)
      H(0):=1.0; H(1):=COS(PHI);
      for I in 2 .. (2*N+1) loop
        H(I):=2.0*H(1)*H(I-1)-H(I-2);
      end loop;
      for K in 1 .. N+1 loop
        T(K) := H(2*K-1)*BMA+BPA; -- Stuetzstellen
      end loop;
      for K in 1 .. N+1 loop
        Position_Access(T(K),L(K),B(K),R(K));
      end loop;
      for K in 2 .. N+1 loop
        if L(K-1)<L(K) then       -- stetig machen in
          L(K):=L(K)-360.0;       -- [-360,+360]
        end if;
      end loop;
      for J in 0 .. N loop        -- Tscheb.-Koeffizienten C(J) berechnen
        PHI:=PI*REAL(J)/REAL(2*N+2); H(1):=COS(PHI);
        for I in 2 .. (2*N+1) loop
          H(I) := 2.0*H(1)*H(I-1)-H(I-2);
        end loop;
        L_POLY.C(J):=0.0; B_POLY.C(J):=0.0; R_POLY.C(J):=0.0;
        for K in 1 .. N+1 loop
          L_POLY.C(J) := L_POLY.C(J) + H(2*K-1)*L(K);
          B_POLY.C(J) := B_POLY.C(J) + H(2*K-1)*B(K);
          R_POLY.C(J) := R_POLY.C(J) + H(2*K-1)*R(K);
        end loop;
        L_POLY.C(J):=L_POLY.C(J)*FAC; B_POLY.C(J):=B_POLY.C(J)*FAC;
        R_POLY.C(J):=R_POLY.C(J)*FAC;
      end loop;
    end T_FIT_LBR;

    ------------------------------------
    -- TN: Tangens-Funktion (Gradmass)
    ------------------------------------
    function TN (X:REAL) return REAL is
      XX: constant REAL := X*RAD;
    begin
      return SIN(XX)/COS(XX);
    end TN;

    ---------------------------------------
    -- CTN: Cotangens-Funktion (Gradmass)
    ---------------------------------------
    function CTN (X:REAL) return REAL is
      XX: constant REAL := X*RAD;
    begin
      return COS(XX)/SIN(XX);
    end CTN;

  end MATLIB;


  --============================================
  -- Unit PLALIB: Merkur, Venus, Mars, Jupiter
  --============================================
  package body PLALIB is

    ---------------------------------------------------------------------------------
    --
    -- GEOCEN: geozentrische Koordinaten (geometrisch oder retardiert)
    --
    --   T:        Zeit in julian. Jahrh. seit J2000; T=(JD-2451545.0)/36525.0
    --   LP,BP,RP: ekliptikale heliozentrische Planetenkoordinaten
    --   LS,BS,RS: ekliptikale geozentrische Sonnenkoordinaten
    --   PLAN:     Planets
    --   MODE:     Modus von (X,Y,Z)
    --   XP,YP,ZP: ekliptikale heliozentrische Planetenkoordinaten
    --   XS,YS,ZS: ekliptikale geozentrische Sonnenkoordinaten
    --   X, Y, Z : ekliptikale geozentrische Planetenkoordinaten
    --             geometrisch , astrometrisch oder scheinbar
    --   DELTA0:   geozentrische Entfernung (geometrisch)
    --
    --   (alle Winkel in Grad, Entfernungen in AE)
    --
    ---------------------------------------------------------------------------------

    procedure GEOCEN (T, LP,BP,RP, LS,BS,RS           :     REAL;
                      PLAN                            :     PLANET;
                      MODE                            :     MODUS;
                      XP,YP,ZP, XS,YS,ZS, X,Y,Z,DELTA0: out REAL) is

      DL,DB,DR, DLS,DBS,DRS, FAC: REAL;
      VX,VY,VZ, VXS,VYS,VZS, M  : REAL;

      procedure POSVEL (L,B,R,DLL,DBL,DRL:     REAL;
                        X,Y,Z,VXL,VYL,VZL: out REAL) is
        CL,SL,CB,SB: REAL;
      begin
        CL:=CS(L); SL:=SN(L); CB:=CS(B); SB:=SN(B);
        X := R*CL*CB;  VXL := DRL*CL*CB-DLL*R*SL*CB-DBL*R*CL*SB;
        Y := R*SL*CB;  VYL := DRL*SL*CB+DLL*R*CL*CB-DBL*R*SL*SB;
        Z := R*SB;     VZL := DRL*SB+DBL*R*CB;
      end POSVEL;

    begin -- GEOCEN

      DL:=0.0; DB:=0.0; DR:=0.0; DLS:=0.0; DBS:=0.0; DRS:=0.0;

      if MODE /= Geometric then

        M := P2*FRAC(0.9931266+ 99.9973604*T); -- Sonne
        DLS := 172.00+5.75*SIN(M);  DRS := 2.87*COS(M); DBS := 0.0;

        --DL,db in 1.0e-4 rad/d, dr in 1.0e-4 AE/d
        case PLAN is
        when Sun =>
          DL:=0.0; DB:=0.0; DR:=0.0;
        when Mercury =>
          M := P2*FRAC(0.4855407+415.2014314*T);
          DL := 714.00+292.66*COS(M)+71.96*COS(2.0*M)+18.16*COS(3.0*M)+
                4.61*COS(4.0*M)+3.81*SIN(2.0*M)+2.43*SIN(3.0*M)+1.08*SIN(4.0*M);
          DR := 55.94*SIN(M)+11.36*SIN(2.0*M)+2.60*SIN(3.0*M);
          DB := 73.40*COS(M)+29.82*COS(2.0*M)+10.22*COS(3.0*M)+3.28*COS(4.0*M)
               -40.44*SIN(M)-16.55*SIN(2.0*M)-5.56*SIN(3.0*M)-1.72*SIN(4.0*M);
        when Venus =>
          M := P2*FRAC(0.1400197+162.5494552*T);
          DL := 280.00+3.79*COS(M);   DR := 1.37*SIN(M);
          DB := 9.54*COS(M)-13.57*SIN(M);
        when Earth =>
          DL:=DLS; DR:=DRS; DB:=-DBS;
        when Mars =>
          M := P2*FRAC(0.0538553+53.1662736*T);
          DL := 91.50+17.07*COS(M)+2.03*COS(2.0*M);
          DR := 12.98*SIN(M)+1.21*COS(2.0*M);
          DB :=  0.83*COS(M)+2.80*SIN(M);
        when Jupiter =>
          M := P2*FRAC(0.0565314+8.4302963*T);
          DL := 14.50+1.41*COS(M); DR := 3.66*SIN(M); DB := 0.33*SIN(M);
        when Saturn =>
          M := P2*FRAC(0.8829867+3.3947688*T);
          DL := 5.84+0.65*COS(M); DR := 3.09*SIN(M); DB := 0.24*COS(M);
        when Uranus =>
          M := P2*FRAC(0.3967117+1.1902849*T);
          DL := 2.05+0.19*COS(M); DR:=1.86*SIN(M); DB:=-0.03*SIN(M);
        when Neptune =>
          M := P2*FRAC(0.7214906+0.6068526*T);
          DL := 1.04+0.02*COS(M); DR:=0.27*SIN(M); DB:=0.03*SIN(M);
        when Pluto =>
          M := P2*FRAC(0.0385795+0.4026667*T);
          DL := 0.69+0.34*COS(M)+0.12*COS(2.0*M)+0.05*COS(3.0*M);
          DR := 6.66*SIN(M)+1.64*SIN(2.0*M);
          DB := -0.08*COS(M)-0.17*SIN(M)-0.09*SIN(2.0*M);
        end case;

      end if;

      POSVEL (LS,BS,RS,DLS,DBS,DRS,XS,YS,ZS,VXS,VYS,VZS);
      POSVEL (LP,BP,RP,DL ,DB ,DR, XP,YP,ZP,VX ,VY ,VZ );
      X:=XP+XS;  Y:=YP+YS;  Z:=ZP+ZS;   DELTA0 := SQRT(X*X+Y*Y+Z*Z);
      if PLAN = Earth then
        X:=0.0; Y:=0.0; Z:=0.0; DELTA0:=0.0;
      end if;

      FAC := 0.00578 * DELTA0 * 1.0E-4;
      case MODE is
      when Astrometric =>
        X:=X-FAC*VX;  Y:=Y-FAC*VY;  Z:=Z-FAC*VZ;
      when Apparent =>
        X:=X-FAC*(VX+VXS); Y:=Y-FAC*(VY+VYS); Z:=Z-FAC*(VZ+VZS);
      when Geometric =>
        null;
      end case;

    end GEOCEN;

    -------------------------------------------------------------------
    -- MER200: Merkur; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure MER200 (T:         REAL;
                      L,B,R: out REAL) is

      C1,S1:          array (-1..9) of REAL;
      C,S:            array (-5..0) of REAL;
      M1,M2,M3,M5,M6: REAL;
      U,V, DL,DR,DB:  REAL;

      procedure ADDTHE (C1L,S1L,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1L*C2-S1L*S2; S:=S1L*C2+C1L*S2;
      end ADDTHE;

      procedure TERM (I1,I,IT:Integer;DLC,DLS,DRC,DRS,DBC,DBS:REAL) is
      begin
        if IT=0 then
          ADDTHE(C1(I1),S1(I1),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTVEN is -- Keplerterme und Stoerungen durch Venus
      begin
        C(0):=1.0; S(0):=0.0;  C(-1):=COS(M2); S(-1):=-SIN(M2);
        for I in reverse -4 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM( 1, 0,0, 259.74,84547.39,-78342.34, 0.01,11683.22,21203.79);
        TERM( 1, 0,1,   2.30,    5.04,    -7.52, 0.02,  138.55,  -71.01);
        TERM( 1, 0,2,   0.01,   -0.01,     0.01, 0.01,   -0.19,   -0.54);
        TERM( 2, 0,0,-549.71,10394.44, -7955.45, 0.00, 2390.29, 4306.79);
        TERM( 2, 0,1,  -4.77,    8.97,    -1.53, 0.00,   28.49,  -14.18);
        TERM( 2, 0,2,   0.00,    0.00,     0.00, 0.00,   -0.04,   -0.11);
        TERM( 3, 0,0,-234.04, 1748.74, -1212.86, 0.00,  535.41,  984.33);
        TERM( 3, 0,1,  -2.03,    3.48,    -0.35, 0.00,    6.56,   -2.91);
        TERM( 4, 0,0, -77.64,  332.63,  -219.23, 0.00,  124.40,  237.03);
        TERM( 4, 0,1,  -0.70,    1.10,    -0.08, 0.00,    1.59,   -0.59);
        TERM( 5, 0,0, -23.59,   67.28,   -43.54, 0.00,   29.44,   58.77);
        TERM( 5, 0,1,  -0.23,    0.32,    -0.02, 0.00,    0.39,   -0.11);
        TERM( 6, 0,0,  -6.86,   14.06,    -9.18, 0.00,    7.03,   14.84);
        TERM( 6, 0,1,  -0.07,    0.09,    -0.01, 0.00,    0.10,   -0.02);
        TERM( 7, 0,0,  -1.94,    2.98,    -2.02, 0.00,    1.69,    3.80);
        TERM( 8, 0,0,  -0.54,    0.63,    -0.46, 0.00,    0.41,    0.98);
        TERM( 9, 0,0,  -0.15,    0.13,    -0.11, 0.00,    0.10,    0.25);
        TERM(-1,-2,0,  -0.17,   -0.06,    -0.05, 0.14,   -0.06,   -0.07);
        TERM( 0,-1,0,   0.24,   -0.16,    -0.11,-0.16,    0.04,   -0.01);
        TERM( 0,-2,0,  -0.68,   -0.25,    -0.26, 0.73,   -0.16,   -0.18);
        TERM( 0,-5,0,   0.37,    0.08,     0.06,-0.28,    0.13,    0.12);
        TERM( 1,-1,0,   0.58,   -0.41,     0.26, 0.36,    0.01,   -0.01);
        TERM( 1,-2,0,  -3.51,   -1.23,     0.23,-0.63,   -0.05,   -0.06);
        TERM( 1,-3,0,   0.08,    0.53,    -0.11, 0.04,    0.02,   -0.09);
        TERM( 1,-5,0,   1.44,    0.31,     0.30,-1.39,    0.34,    0.29);
        TERM( 2,-1,0,   0.15,   -0.11,     0.09, 0.12,    0.02,   -0.04);
        TERM( 2,-2,0,  -1.99,   -0.68,     0.65,-1.91,   -0.20,    0.03);
        TERM( 2,-3,0,  -0.34,   -1.28,     0.97,-0.26,    0.03,    0.03);
        TERM( 2,-4,0,  -0.33,    0.35,    -0.13,-0.13,   -0.01,    0.00);
        TERM( 2,-5,0,   7.19,    1.56,    -0.05, 0.12,    0.06,    0.05);
        TERM( 3,-2,0,  -0.52,   -0.18,     0.13,-0.39,   -0.16,    0.03);
        TERM( 3,-3,0,  -0.11,   -0.42,     0.36,-0.10,   -0.05,   -0.05);
        TERM( 3,-4,0,  -0.19,    0.22,    -0.23,-0.20,   -0.01,    0.02);
        TERM( 3,-5,0,   2.77,    0.49,    -0.45, 2.56,    0.40,   -0.12);
        TERM( 4,-5,0,   0.67,    0.12,    -0.09, 0.47,    0.24,   -0.08);
        TERM( 5,-5,0,   0.18,    0.03,    -0.02, 0.12,    0.09,   -0.03);
      end PERTVEN;

      procedure PERTEAR is -- Stoerungen durch die Erde
      begin
        C(-1):=COS(M3); S(-1):=-SIN(M3);
        for I in reverse -3 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM( 0,-4,0,  -0.11,   -0.07,    -0.08, 0.11,   -0.02,   -0.04);
        TERM( 1,-1,0,   0.10,   -0.20,     0.15, 0.07,    0.00,    0.00);
        TERM( 1,-2,0,  -0.35,    0.28,    -0.13,-0.17,   -0.01,    0.00);
        TERM( 1,-4,0,  -0.67,   -0.45,     0.00, 0.01,   -0.01,   -0.01);
        TERM( 2,-2,0,  -0.20,    0.16,    -0.16,-0.20,   -0.01,    0.02);
        TERM( 2,-3,0,   0.13,   -0.02,     0.02, 0.14,    0.01,    0.00);
        TERM( 2,-4,0,  -0.33,   -0.18,     0.17,-0.31,   -0.04,    0.00);
      end PERTEAR;

      procedure PERTJUP is -- Stoerungen durch Jupiter
      begin
        C(-1):=COS(M5); S(-1):=-SIN(M5);
        for I in reverse -2 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(-1,-1,0,  -0.08,    0.16,     0.15, 0.08,   -0.04,    0.01);
        TERM(-1,-2,0,   0.10,   -0.06,    -0.07,-0.12,    0.07,   -0.01);
        TERM( 0,-1,0,  -0.31,    0.48,    -0.02, 0.13,   -0.03,   -0.02);
        TERM( 0,-2,0,   0.42,   -0.26,    -0.38,-0.50,    0.20,   -0.03);
        TERM( 1,-1,0,  -0.70,    0.01,    -0.02,-0.63,    0.00,    0.03);
        TERM( 1,-2,0,   2.61,   -1.97,     1.74, 2.32,    0.01,    0.01);
        TERM( 1,-3,0,   0.32,   -0.15,     0.13, 0.28,    0.00,    0.00);
        TERM( 2,-1,0,  -0.18,    0.01,     0.00,-0.13,   -0.03,    0.03);
        TERM( 2,-2,0,   0.75,   -0.56,     0.45, 0.60,    0.08,   -0.17);
        TERM( 3,-2,0,   0.20,   -0.15,     0.10, 0.14,    0.04,   -0.08);
      end PERTJUP;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(-2):=COS(2.0*M6); S(-2):=-SIN(2.0*M6);
        TERM( 1,-2,0,  -0.19,    0.33,     0.00, 0.00,    0.00,    0.00);
      end PERTSAT;

    begin -- MER200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M1:=P2*FRAC(0.4855407+415.2014314*T); M2:=P2*FRAC(0.1394222+162.5490444*T);
      M3:=P2*FRAC(0.9937861+ 99.9978139*T); M5:=P2*FRAC(0.0558417+  8.4298417*T);
      M6:=P2*FRAC(0.8823333+  3.3943333*T);
      C1(0):=1.0;     S1(0):=0.0;
      C1(1):=COS(M1); S1(1):=SIN(M1);  C1(-1):=C1(1); S1(-1):=-S1(1);
      for I in 2 .. 9 loop
        ADDTHE(C1(I-1),S1(I-1),C1(1),S1(1),C1(I),S1(I));
      end loop;
      PERTVEN; PERTEAR; PERTJUP; PERTSAT;
      DL := DL + (2.8+3.2*T);
      L:= 360.0*FRAC(0.2151379 + M1/P2 + ((5601.7+1.1*T)*T+DL)/1296.0E3 );
      R:= 0.3952829 + 0.0000016*T  +  DR*1.0E-6;
      B:= ( -2522.15 + (-30.18 + 0.04*T) * T  +  DB ) / 3600.0;
    end MER200;

    ------------------------------------------------------------------
    -- VEN200: Venus; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    ------------------------------------------------------------------
    procedure VEN200 (T:         REAL;
                      L,B,R: out REAL) is

      C2,S2:             array ( 0..8) of REAL;
      C,S:               array (-8..0) of REAL;
      M1,M2,M3,M4,M5,M6: REAL;
      U,V, DL,DR,DB:     REAL;

      procedure ADDTHE (C1,S1,C2L,S2L:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2L-S1*S2L; S:=S1*C2L+C1*S2L;
      end ADDTHE;

      procedure TERM (I1,I,IT:                 Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS: REAL) is
      begin
        if IT=0 then
          ADDTHE(C2(I1),S2(I1),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTMER is -- Stoerungen durch Merkur
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M1); S(-1):=-SIN(M1);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM(1,-1,0,   0.00,   0.00,    0.06, -0.09,   0.01,   0.00);
        TERM(2,-1,0,   0.25,  -0.09,   -0.09, -0.27,   0.00,   0.00);
        TERM(4,-2,0,  -0.07,  -0.08,   -0.14,  0.14,  -0.01,  -0.01);
        TERM(5,-2,0,  -0.35,   0.08,    0.02,  0.09,   0.00,   0.00);
      end PERTMER;

      procedure PERTEAR is -- Keplerterme und Stoerungen durch die Erde
      begin
        C(-1):=COS(M3); S(-1):=-SIN(M3);
        for I in reverse -7 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(1, 0,0,   2.37,2793.23,-4899.07,  0.11,9995.27,7027.22);
        TERM(1, 0,1,   0.10, -19.65,   34.40,  0.22,  64.95, -86.10);
        TERM(1, 0,2,   0.06,   0.04,   -0.07,  0.11,  -0.55,  -0.07);
        TERM(2, 0,0,-170.42,  73.13,  -16.59,  0.00,  67.71,  47.56);
        TERM(2, 0,1,   0.93,   2.91,    0.23,  0.00,  -0.03,  -0.92);
        TERM(3, 0,0,  -2.31,   0.90,   -0.08,  0.00,   0.04,   2.09);
        TERM(1,-1,0,  -2.38,  -4.27,    3.27, -1.82,   0.00,   0.00);
        TERM(1,-2,0,   0.09,   0.00,   -0.08,  0.05,  -0.02,  -0.25);
        TERM(2,-2,0,  -9.57,  -5.93,    8.57,-13.83,  -0.01,  -0.01);
        TERM(2,-3,0,  -2.47,  -2.40,    0.83, -0.95,   0.16,   0.24);
        TERM(3,-2,0,  -0.09,  -0.05,    0.08, -0.13,  -0.28,   0.12);
        TERM(3,-3,0,   7.12,   0.32,   -0.62, 13.76,  -0.07,   0.01);
        TERM(3,-4,0,  -0.65,  -0.17,    0.18, -0.73,   0.10,   0.05);
        TERM(3,-5,0,  -1.08,  -0.95,   -0.17,  0.22,  -0.03,  -0.03);
        TERM(4,-3,0,   0.06,   0.00,   -0.01,  0.08,   0.14,  -0.18);
        TERM(4,-4,0,   0.93,  -0.46,    1.06,  2.13,  -0.01,   0.01);
        TERM(4,-5,0,  -1.53,   0.38,   -0.64, -2.54,   0.27,   0.00);
        TERM(4,-6,0,  -0.17,  -0.05,    0.03, -0.11,   0.02,   0.00);
        TERM(5,-5,0,   0.18,  -0.28,    0.71,  0.47,  -0.02,   0.04);
        TERM(5,-6,0,   0.15,  -0.14,    0.30,  0.31,  -0.04,   0.03);
        TERM(5,-7,0,  -0.08,   0.02,   -0.03, -0.11,   0.01,   0.00);
        TERM(5,-8,0,  -0.23,   0.00,    0.01, -0.04,   0.00,   0.00);
        TERM(6,-6,0,   0.01,  -0.14,    0.39,  0.04,   0.00,  -0.01);
        TERM(6,-7,0,   0.02,  -0.05,    0.12,  0.04,  -0.01,   0.01);
        TERM(6,-8,0,   0.10,  -0.10,    0.19,  0.19,  -0.02,   0.02);
        TERM(7,-7,0,  -0.03,  -0.06,    0.18, -0.08,   0.00,   0.00);
        TERM(8,-8,0,  -0.03,  -0.02,    0.06, -0.08,   0.00,   0.00);
      end PERTEAR;

      procedure PERTMAR is -- Stoerungen durch Mars
      begin
        C(-1):=COS(M4); S(-1):=-SIN(M4);
        for I in reverse -2 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(1,-3,0,  -0.65,   1.02,   -0.04, -0.02,  -0.02,   0.00);
        TERM(2,-2,0,  -0.05,   0.04,   -0.09, -0.10,   0.00,   0.00);
        TERM(2,-3,0,  -0.50,   0.45,   -0.79, -0.89,   0.01,   0.03);
      end PERTMAR;

      procedure PERTJUP is -- Stoerungen durch Jupiter
      begin
        C(-1):=COS(M5); S(-1):=-SIN(M5);
        for I in reverse -2 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(0,-1,0,  -0.05,   1.56,    0.16,  0.04,  -0.08,  -0.04);
        TERM(1,-1,0,  -2.62,   1.40,   -2.35, -4.40,   0.02,   0.03);
        TERM(1,-2,0,  -0.47,  -0.08,    0.12, -0.76,   0.04,  -0.18);
        TERM(2,-2,0,  -0.73,  -0.51,    1.27, -1.82,  -0.01,   0.01);
        TERM(2,-3,0,  -0.14,  -0.10,    0.25, -0.34,   0.00,   0.00);
        TERM(3,-3,0,  -0.01,   0.04,   -0.11, -0.02,   0.00,   0.00);
      end PERTJUP;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(-1):=COS(M6); S(-1):=-SIN(M6);
        TERM(0,-1,0,   0.00,   0.21,    0.00,  0.00,   0.00,  -0.01);
        TERM(1,-1,0,  -0.11,  -0.14,    0.24, -0.20,   0.01,   0.00);
      end PERTSAT;

    begin -- VEN200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M1:=P2*FRAC(0.4861431+415.2018375*T); M2:=P2*FRAC(0.1400197+162.5494552*T);
      M3:=P2*FRAC(0.9944153+ 99.9982208*T); M4:=P2*FRAC(0.0556297+ 53.1674631*T);
      M5:=P2*FRAC(0.0567028+  8.4305083*T); M6:=P2*FRAC(0.8830539+  3.3947206*T);
      C2(0):=1.0; S2(0):=0.0; C2(1):=COS(M2); S2(1):=SIN(M2);
      for I in 2 .. 8 loop
        ADDTHE(C2(I-1),S2(I-1),C2(1),S2(1),C2(I),S2(I));
      end loop;
      PERTMER; PERTEAR; PERTMAR; PERTJUP; PERTSAT;
      DL:=DL + 2.74*SIN(P2*(0.0764+0.4174*T)) + 0.27*SIN(P2*(0.9201+0.3307*T));
      DL:=DL + (1.9+1.8*T);
      L:= 360.0*FRAC(0.3654783 + M2/P2 + ((5071.2+1.1*T)*T+DL)/1296.0E3 );
      R:= 0.7233482 - 0.0000002*T  +  DR*1.0E-6;
      B:= ( -67.70 + ( 0.04 + 0.01*T) * T  +  DB ) / 3600.0;
    end VEN200;

    -----------------------------------------------------------------
    -- MAR200: Mars; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -----------------------------------------------------------------
    procedure MAR200(T:         REAL;
                     L,B,R: out REAL) is

      C4,S4:          array (-2..16) of REAL;
      C,S:            array (-9.. 0) of REAL;
      M2,M3,M4,M5,M6: REAL;
      U,V, DL,DR,DB:  REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2-S1*S2; S:=S1*C2+C1*S2;
      end ADDTHE;

      procedure TERM (I1,I,IT:Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS:REAL) is
      begin
        if IT=0 then
          ADDTHE(C4(I1),S4(I1),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTVEN is -- Stoerungen durch Venus
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M2); S(-1):=-SIN(M2);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM( 0,-1,0, -0.01,   -0.03,      0.10, -0.04,    0.00,   0.00);
        TERM( 1,-1,0,  0.05,    0.10,     -2.08,  0.75,    0.00,   0.00);
        TERM( 2,-1,0, -0.25,   -0.57,     -2.58,  1.18,    0.05,  -0.04);
        TERM( 2,-2,0,  0.02,    0.02,      0.13, -0.14,    0.00,   0.00);
        TERM( 3,-1,0,  3.41,    5.38,      1.87, -1.15,    0.01,  -0.01);
        TERM( 3,-2,0,  0.02,    0.02,      0.11, -0.13,    0.00,   0.00);
        TERM( 4,-1,0,  0.32,    0.49,     -1.88,  1.21,   -0.07,   0.07);
        TERM( 4,-2,0,  0.03,    0.03,      0.12, -0.14,    0.00,   0.00);
        TERM( 5,-1,0,  0.04,    0.06,     -0.17,  0.11,   -0.01,   0.01);
        TERM( 5,-2,0,  0.11,    0.09,      0.35, -0.43,   -0.01,   0.01);
        TERM( 6,-2,0, -0.36,   -0.28,     -0.20,  0.25,    0.00,   0.00);
        TERM( 7,-2,0, -0.03,   -0.03,      0.11, -0.13,    0.00,  -0.01);
      end PERTVEN;

      procedure PERTEAR is -- Keplerterme und Stoerungen durch die Erde
      begin
        C(-1):=COS(M3); S(-1):=-SIN(M3);
        for I in reverse -8 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM( 1, 0,0, -5.32,38481.97,-141856.04,  0.40,-6321.67,1876.89);
        TERM( 1, 0,1, -1.12,   37.98,   -138.67, -2.93,   37.28, 117.48);
        TERM( 1, 0,2, -0.32,   -0.03,      0.12, -1.19,    1.04,  -0.40);
        TERM( 2, 0,0, 28.28, 2285.80,  -6608.37,  0.00, -589.35, 174.81);
        TERM( 2, 0,1,  1.64,    3.37,    -12.93,  0.00,    2.89,  11.10);
        TERM( 2, 0,2,  0.00,    0.00,      0.00,  0.00,    0.10,  -0.03);
        TERM( 3, 0,0,  5.31,  189.29,   -461.81,  0.00,  -61.98,  18.53);
        TERM( 3, 0,1,  0.31,    0.35,     -1.36,  0.00,    0.25,   1.19);
        TERM( 4, 0,0,  0.81,   17.96,    -38.26,  0.00,   -6.88,   2.08);
        TERM( 4, 0,1,  0.05,    0.04,     -0.15,  0.00,    0.02,   0.14);
        TERM( 5, 0,0,  0.11,    1.83,     -3.48,  0.00,   -0.79,   0.24);
        TERM( 6, 0,0,  0.02,    0.20,     -0.34,  0.00,   -0.09,   0.03);
        TERM(-1,-1,0,  0.09,    0.06,      0.14, -0.22,    0.02,  -0.02);
        TERM( 0,-1,0,  0.72,    0.49,      1.55, -2.31,    0.12,  -0.10);
        TERM( 1,-1,0,  7.00,    4.92,     13.93,-20.48,    0.08,  -0.13);
        TERM( 2,-1,0, 13.08,    4.89,     -4.53, 10.01,   -0.05,   0.13);
        TERM( 2,-2,0,  0.14,    0.05,     -0.48, -2.66,    0.01,   0.14);
        TERM( 3,-1,0,  1.38,    0.56,     -2.00,  4.85,   -0.01,   0.19);
        TERM( 3,-2,0, -6.85,    2.68,      8.38, 21.42,    0.00,   0.03);
        TERM( 3,-3,0, -0.08,    0.20,      1.20,  0.46,    0.00,   0.00);
        TERM( 4,-1,0,  0.16,    0.07,     -0.19,  0.47,   -0.01,   0.05);
        TERM( 4,-2,0, -4.41,    2.14,     -3.33, -7.21,   -0.07,  -0.09);
        TERM( 4,-3,0, -0.12,    0.33,      2.22,  0.72,   -0.03,  -0.02);
        TERM( 4,-4,0, -0.04,   -0.06,     -0.36,  0.23,    0.00,   0.00);
        TERM( 5,-2,0, -0.44,    0.21,     -0.70, -1.46,   -0.06,  -0.07);
        TERM( 5,-3,0,  0.48,   -2.60,     -7.25, -1.37,    0.00,   0.00);
        TERM( 5,-4,0, -0.09,   -0.12,     -0.66,  0.50,    0.00,   0.00);
        TERM( 5,-5,0,  0.03,    0.00,      0.01, -0.17,    0.00,   0.00);
        TERM( 6,-2,0, -0.05,    0.03,     -0.07, -0.15,   -0.01,  -0.01);
        TERM( 6,-3,0,  0.10,   -0.96,      2.36,  0.30,    0.04,   0.00);
        TERM( 6,-4,0, -0.17,   -0.20,     -1.09,  0.94,    0.02,  -0.02);
        TERM( 6,-5,0,  0.05,    0.00,      0.00, -0.30,    0.00,   0.00);
        TERM( 7,-3,0,  0.01,   -0.10,      0.32,  0.04,    0.02,   0.00);
        TERM( 7,-4,0,  0.86,    0.77,      1.86, -2.01,    0.01,  -0.01);
        TERM( 7,-5,0,  0.09,   -0.01,     -0.05, -0.44,    0.00,   0.00);
        TERM( 7,-6,0, -0.01,    0.02,      0.10,  0.08,    0.00,   0.00);
        TERM( 8,-4,0,  0.20,    0.16,     -0.53,  0.64,   -0.01,   0.02);
        TERM( 8,-5,0,  0.17,   -0.03,     -0.14, -0.84,    0.00,   0.01);
        TERM( 8,-6,0, -0.02,    0.03,      0.16,  0.09,    0.00,   0.00);
        TERM( 9,-5,0, -0.55,    0.15,      0.30,  1.10,    0.00,   0.00);
        TERM( 9,-6,0, -0.02,    0.04,      0.20,  0.10,    0.00,   0.00);
        TERM(10,-5,0, -0.09,    0.03,     -0.10, -0.33,    0.00,  -0.01);
        TERM(10,-6,0, -0.05,    0.11,      0.48,  0.21,   -0.01,   0.00);
        TERM(11,-6,0,  0.10,   -0.35,     -0.52, -0.15,    0.00,   0.00);
        TERM(11,-7,0, -0.01,   -0.02,     -0.10,  0.07,    0.00,   0.00);
        TERM(12,-6,0,  0.01,   -0.04,      0.18,  0.04,    0.01,   0.00);
        TERM(12,-7,0, -0.05,   -0.07,     -0.29,  0.20,    0.01,   0.00);
        TERM(13,-7,0,  0.23,    0.27,      0.25, -0.21,    0.00,   0.00);
        TERM(14,-7,0,  0.02,    0.03,     -0.10,  0.09,    0.00,   0.00);
        TERM(14,-8,0,  0.05,    0.01,      0.03, -0.23,    0.00,   0.03);
        TERM(15,-8,0, -1.53,    0.27,      0.06,  0.42,    0.00,   0.00);
        TERM(16,-8,0, -0.14,    0.02,     -0.10, -0.55,   -0.01,  -0.02);
        TERM(16,-9,0,  0.03,   -0.06,     -0.25, -0.11,    0.00,   0.00);
      end PERTEAR;

      procedure PERTJUP is -- Stoerungen durch Jupiter
      begin
        C(-1):=COS(M5); S(-1):=-SIN(M5);
        for I in reverse -4 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(-2,-1,0,  0.05,    0.03,      0.08, -0.14,    0.01,  -0.01);
        TERM(-1,-1,0,  0.39,    0.27,      0.92, -1.50,   -0.03,  -0.06);
        TERM(-1,-2,0, -0.16,    0.03,      0.13,  0.67,   -0.01,   0.06);
        TERM(-1,-3,0, -0.02,    0.01,      0.05,  0.09,    0.00,   0.01);
        TERM( 0,-1,0,  3.56,    1.13,     -5.41, -7.18,   -0.25,  -0.24);
        TERM( 0,-2,0, -1.44,    0.25,      1.24,  7.96,    0.02,   0.31);
        TERM( 0,-3,0, -0.21,    0.11,      0.55,  1.04,    0.01,   0.05);
        TERM( 0,-4,0, -0.02,    0.02,      0.11,  0.11,    0.00,   0.01);
        TERM( 1,-1,0, 16.67,  -19.15,     61.00, 53.36,   -0.06,  -0.07);
        TERM( 1,-2,0,-21.64,    3.18,     -7.77,-54.64,   -0.31,   0.50);
        TERM( 1,-3,0, -2.82,    1.45,     -2.53, -5.73,    0.01,   0.07);
        TERM( 1,-4,0, -0.31,    0.28,     -0.34, -0.51,    0.00,   0.00);
        TERM( 2,-1,0,  2.15,   -2.29,      7.04,  6.94,    0.33,   0.19);
        TERM( 2,-2,0,-15.69,    3.31,    -15.70,-73.17,   -0.17,  -0.25);
        TERM( 2,-3,0, -1.73,    1.95,     -9.19, -7.20,    0.02,  -0.03);
        TERM( 2,-4,0, -0.01,    0.33,     -1.42,  0.08,    0.01,  -0.01);
        TERM( 2,-5,0,  0.03,    0.03,     -0.13,  0.12,    0.00,   0.00);
        TERM( 3,-1,0,  0.26,   -0.28,      0.73,  0.71,    0.08,   0.04);
        TERM( 3,-2,0, -2.06,    0.46,     -1.61, -6.72,   -0.13,  -0.25);
        TERM( 3,-3,0, -1.28,   -0.27,      2.21, -6.90,   -0.04,  -0.02);
        TERM( 3,-4,0, -0.22,    0.08,     -0.44, -1.25,    0.00,   0.01);
        TERM( 3,-5,0, -0.02,    0.03,     -0.15, -0.08,    0.00,   0.00);
        TERM( 4,-1,0,  0.03,   -0.03,      0.08,  0.08,    0.01,   0.01);
        TERM( 4,-2,0, -0.26,    0.06,     -0.17, -0.70,   -0.03,  -0.05);
        TERM( 4,-3,0, -0.20,   -0.05,      0.22, -0.79,   -0.01,  -0.02);
        TERM( 4,-4,0, -0.11,   -0.14,      0.93, -0.60,    0.00,   0.00);
        TERM( 4,-5,0, -0.04,   -0.02,      0.09, -0.23,    0.00,   0.00);
        TERM( 5,-4,0, -0.02,   -0.03,      0.13, -0.09,    0.00,   0.00);
        TERM( 5,-5,0,  0.00,   -0.03,      0.21,  0.01,    0.00,   0.00);
      end PERTJUP;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(-1):=COS(M6); S(-1):=-SIN(M6);
        for I in reverse -3 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(-1,-1,0,  0.03,    0.13,      0.48, -0.13,    0.02,   0.00);
        TERM( 0,-1,0,  0.27,    0.84,      0.40, -0.43,    0.01,  -0.01);
        TERM( 0,-2,0,  0.12,   -0.04,     -0.33, -0.55,   -0.01,  -0.02);
        TERM( 0,-3,0,  0.02,   -0.01,     -0.07, -0.08,    0.00,   0.00);
        TERM( 1,-1,0,  1.12,    0.76,     -2.66,  3.91,   -0.01,   0.01);
        TERM( 1,-2,0,  1.49,   -0.95,      3.07,  4.83,    0.04,  -0.05);
        TERM( 1,-3,0,  0.21,   -0.18,      0.55,  0.64,    0.00,   0.00);
        TERM( 2,-1,0,  0.12,    0.10,     -0.29,  0.34,   -0.01,   0.02);
        TERM( 2,-2,0,  0.51,   -0.36,      1.61,  2.25,    0.03,   0.01);
        TERM( 2,-3,0,  0.10,   -0.10,      0.50,  0.43,    0.00,   0.00);
        TERM( 2,-4,0,  0.01,   -0.02,      0.11,  0.05,    0.00,   0.00);
        TERM( 3,-2,0,  0.07,   -0.05,      0.16,  0.22,    0.01,   0.01);
      end PERTSAT;

    begin -- MAR200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M2:=P2*FRAC(0.1382208+162.5482542*T); M3:=P2*FRAC(0.9926208+99.9970236*T);
      M4:=P2*FRAC(0.0538553+ 53.1662736*T); M5:=P2*FRAC(0.0548944+ 8.4290611*T);
      M6:=P2*FRAC(0.8811167+  3.3935250*T);
      C4(0):=1.0; S4(0):=0.0;  C4(1):=COS(M4); S4(1):=SIN(M4);
      for I in 2 .. 16 loop
        ADDTHE(C4(I-1),S4(I-1),C4(1),S4(1),C4(I),S4(I));
      end loop;
      for I in -2 .. -1 loop
        C4(I):=C4(-I); S4(I):=-S4(-I);
      end loop;
      PERTVEN; PERTEAR; PERTJUP; PERTSAT;
      DL:=DL + 52.49*SIN(P2*(0.1868+0.0549*T)) + 0.61*SIN(P2*(0.9220+0.3307*T))
             +  0.32*SIN(P2*(0.4731+2.1485*T)) + 0.28*SIN(P2*(0.9467+0.1133*T));
      DL:=DL + (0.14+0.87*T-0.11*T*T);
      L:= 360.0*FRAC(0.9334591 + M4/P2 + ((6615.5+1.1*T)*T+DL)/1296.0E3 );
      R:= 1.5303352 + 0.0000131*T  +  DR*1.0E-6;
      B:= ( 596.32 + (-2.92 - 0.10*T) * T  +  DB ) / 3600.0;
    end MAR200;

    --------------------------------------------------------------------
    -- JUP200: Jupiter; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    --------------------------------------------------------------------
    procedure JUP200 (T:         REAL;
                      L,B,R: out REAL) is
      C5,S5:        array (-1..5) of REAL;
      C,S:          array (-10..0) of REAL;
      M5,M6,M7:     REAL;
      U,V,DL,DR,DB: REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2-S1*S2; S:=S1*C2+C1*S2;
      end ADDTHE;

      procedure TERM (I5,I,IT:                 Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS: REAL) is
      begin
        if IT=0 then
          ADDTHE(C5(I5),S5(I5),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTSAT is -- Keplerterme und Stoerungen durch Saturn
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M6); S(-1):=-SIN(M6);
        for I in reverse -9 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(-1, -1,0,  -0.2,    1.4,     2.0,   0.6,    0.1, -0.2);
        TERM( 0, -1,0,   9.4,    8.9,     3.9,  -8.3,   -0.4, -1.4);
        TERM( 0, -2,0,   5.6,   -3.0,    -5.4,  -5.7,   -2.0,  0.0);
        TERM( 0, -3,0,  -4.0,   -0.1,     0.0,   5.5,    0.0,  0.0);
        TERM( 0, -5,0,   3.3,   -1.6,    -1.6,  -3.1,   -0.5, -1.2);
        TERM( 1,  0,0,-113.1,19998.6,-25208.2,-142.2,-4670.7,288.9);
        TERM( 1,  0,1, -76.1,   66.9,   -84.2, -95.8,   21.6, 29.4);
        TERM( 1,  0,2,  -0.5,   -0.3,     0.4,  -0.7,    0.1, -0.1);
        TERM( 1, -1,0,  78.8,  -14.5,    11.5,  64.4,   -0.2,  0.2);
        TERM( 1, -2,0,  -2.0, -132.4,    28.8,   4.3,   -1.7,  0.4);
        TERM( 1, -2,1,  -1.1,   -0.7,     0.2,  -0.3,    0.0,  0.0);
        TERM( 1, -3,0,  -7.5,   -6.8,    -0.4,  -1.1,    0.6, -0.9);
        TERM( 1, -4,0,   0.7,    0.7,     0.6,  -1.1,    0.0, -0.2);
        TERM( 1, -5,0,  51.5,  -26.0,   -32.5, -64.4,   -4.9,-12.4);
        TERM( 1, -5,1,  -1.2,   -2.2,    -2.7,   1.5,   -0.4,  0.3);
        TERM( 2,  0,0,  -3.4,  632.0,  -610.6,  -6.5, -226.8, 12.7);
        TERM( 2,  0,1,  -4.2,    3.8,    -4.1,  -4.5,    0.2,  0.6);
        TERM( 2, -1,0,   5.3,   -0.7,     0.7,   6.1,    0.2,  1.1);
        TERM( 2, -2,0, -76.4, -185.1,   260.2,-108.0,    1.6,  0.0);
        TERM( 2, -3,0,  66.7,   47.8,   -51.4,  69.8,    0.9,  0.3);
        TERM( 2, -3,1,   0.6,   -1.0,     1.0,   0.6,    0.0,  0.0);
        TERM( 2, -4,0,  17.0,    1.4,    -1.8,   9.6,    0.0, -0.1);
        TERM( 2, -5,0,1066.2, -518.3,    -1.3, -23.9,    1.8, -0.3);
        TERM( 2, -5,1, -25.4,  -40.3,    -0.9,   0.3,    0.0,  0.0);
        TERM( 2, -5,2,  -0.7,    0.5,     0.0,   0.0,    0.0,  0.0);
        TERM( 3,  0,0,  -0.1,   28.0,   -22.1,  -0.2,  -12.5,  0.7);
        TERM( 3, -2,0,  -5.0,  -11.5,    11.7,  -5.4,    2.1, -1.0);
        TERM( 3, -3,0,  16.9,   -6.4,    13.4,  26.9,   -0.5,  0.8);
        TERM( 3, -4,0,   7.2,  -13.3,    20.9,  10.5,    0.1, -0.1);
        TERM( 3, -5,0,  68.5,  134.3,  -166.9,  86.5,    7.1, 15.2);
        TERM( 3, -5,1,   3.5,   -2.7,     3.4,   4.3,    0.5, -0.4);
        TERM( 3, -6,0,   0.6,    1.0,    -0.9,   0.5,    0.0,  0.0);
        TERM( 3, -7,0,  -1.1,    1.7,    -0.4,  -0.2,    0.0,  0.0);
        TERM( 4,  0,0,   0.0,    1.4,    -1.0,   0.0,   -0.6,  0.0);
        TERM( 4, -2,0,  -0.3,   -0.7,     0.4,  -0.2,    0.2, -0.1);
        TERM( 4, -3,0,   1.1,   -0.6,     0.9,   1.2,    0.1,  0.2);
        TERM( 4, -4,0,   3.2,    1.7,    -4.1,   5.8,    0.2,  0.1);
        TERM( 4, -5,0,   6.7,    8.7,    -9.3,   8.7,   -1.1,  1.6);
        TERM( 4, -6,0,   1.5,   -0.3,     0.6,   2.4,    0.0,  0.0);
        TERM( 4, -7,0,  -1.9,    2.3,    -3.2,  -2.7,    0.0, -0.1);
        TERM( 4, -8,0,   0.4,   -1.8,     1.9,   0.5,    0.0,  0.0);
        TERM( 4, -9,0,  -0.2,   -0.5,     0.3,  -0.1,    0.0,  0.0);
        TERM( 4,-10,0,  -8.6,   -6.8,    -0.4,   0.1,    0.0,  0.0);
        TERM( 4,-10,1,  -0.5,    0.6,     0.0,   0.0,    0.0,  0.0);
        TERM( 5, -5,0,  -0.1,    1.5,    -2.5,  -0.8,   -0.1,  0.1);
        TERM( 5, -6,0,   0.1,    0.8,    -1.6,   0.1,    0.0,  0.0);
        TERM( 5, -9,0,  -0.5,   -0.1,     0.1,  -0.8,    0.0,  0.0);
        TERM( 5,-10,0,   2.5,   -2.2,     2.8,   3.1,    0.1, -0.2);
      end PERTSAT;

      procedure PERTURA is -- Stoerungen durch Uranus
      begin
        C(-1):=COS(M7); S(-1):=-SIN(M7);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM( 1, -1,0,   0.4,    0.9,     0.0,   0.0,    0.0,  0.0);
        TERM( 1, -2,0,   0.4,    0.4,    -0.4,   0.3,    0.0,  0.0);
      end PERTURA;

      procedure PERTSUR is -- Stoerungen Saturn und Uranus
        PHI,X,Y: REAL;
      begin
        PHI:=(2.0*M5-6.0*M6+3.0*M7); X:=COS(PHI); Y:=SIN(PHI);
        DL:=DL-0.8*X+8.5*Y; DR:=DR-0.1*X;
        ADDTHE(X,Y,C5(1),S5(1),X,Y);
        DL:=DL+0.4*X+0.5*Y; DR:=DR-0.7*X+0.5*Y; DB:=DB-0.1*X;
      end PERTSUR;

    begin -- JUP200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M5:=P2*FRAC(0.0565314+8.4302963*T); M6:=P2*FRAC(0.8829867+3.3947688*T);
      M7:=P2*FRAC(0.3969537+1.1902586*T);
      C5(0):=1.0;     S5(0):=0.0;
      C5(1):=COS(M5); S5(1):=SIN(M5);  C5(-1):=C5(1); S5(-1):=-S5(1);
      for I in 2 .. 5 loop
        ADDTHE(C5(I-1),S5(I-1),C5(1),S5(1),C5(I),S5(I));
      end loop;
      PERTSAT; PERTURA; PERTSUR;
      L:= 360.0*FRAC(0.0388910 + M5/P2 + ((5025.2+0.8*T)*T+DL)/1296.0E3 );
      R:= 5.208873 + 0.000041*T  +  DR*1.0E-5;
      B:= ( 227.3 - 0.3*T + DB ) / 3600.0;
    end JUP200;

    -------------------------------------------------------------------
    -- SAT200: Saturn; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure SAT200 (T:         REAL;
                      L,B,R: out REAL) is
      C6,S6:         array ( 0..11) of REAL;
      C,S:           array (-6.. 1) of REAL;
      M5,M6,M7,M8:   REAL;
      U,V, DL,DR,DB: REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2-S1*S2; S:=S1*C2+C1*S2;
      end ADDTHE;

      procedure TERM (I6,I,IT:                 Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS: REAL) is
      begin
        if IT=0 then
          ADDTHE(C6(I6),S6(I6),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTJUP is -- Keplerterme und Stoerungen durch Jupiter
      begin
        C(0):=1.0; S(0):=0.0; C(1):=COS(M5); S(1):=SIN(M5);
        for I in reverse -5 .. -0 loop
          ADDTHE(C(I),S(I),C(1),-S(1),C(I-1),S(I-1));
        end loop;
        TERM( 0,-1,0,   12.0,   -1.4,   -13.9,    6.4,    1.2,  -1.8);
        TERM( 0,-2,0,    0.0,   -0.2,    -0.9,    1.0,    0.0,  -0.1);
        TERM( 1, 1,0,    0.9,    0.4,    -1.8,    1.9,    0.2,   0.2);
        TERM( 1, 0,0, -348.3,22907.7,-52915.5, -752.2,-3266.5,8314.4);
        TERM( 1, 0,1, -225.2, -146.2,   337.7, -521.3,   79.6,  17.4);
        TERM( 1, 0,2,    1.3,   -1.4,     3.2,    2.9,    0.1,  -0.4);
        TERM( 1,-1,0,   -1.0,  -30.7,   108.6, -815.0,   -3.6,  -9.3);
        TERM( 1,-2,0,   -2.0,   -2.7,    -2.1,  -11.9,   -0.1,  -0.4);
        TERM( 2, 1,0,    0.1,    0.2,    -1.0,    0.3,    0.0,   0.0);
        TERM( 2, 0,0,   44.2,  724.0, -1464.3,  -34.7, -188.7, 459.1);
        TERM( 2, 0,1,  -17.0,  -11.3,    18.9,  -28.6,    1.0,  -3.7);
        TERM( 2,-1,0,   -3.5, -426.6,  -546.5,  -26.5,   -1.6,  -2.7);
        TERM( 2,-1,1,    3.5,   -2.2,    -2.6,   -4.3,    0.0,   0.0);
        TERM( 2,-2,0,   10.5,  -30.9,  -130.5,  -52.3,   -1.9,   0.2);
        TERM( 2,-3,0,   -0.2,   -0.4,    -1.2,   -0.1,   -0.1,   0.0);
        TERM( 3, 0,0,    6.5,   30.5,   -61.1,    0.4,  -11.6,  28.1);
        TERM( 3, 0,1,   -1.2,   -0.7,     1.1,   -1.8,   -0.2,  -0.6);
        TERM( 3,-1,0,   29.0,  -40.2,    98.2,   45.3,    3.2,  -9.4);
        TERM( 3,-1,1,    0.6,    0.6,    -1.0,    1.3,    0.0,   0.0);
        TERM( 3,-2,0,  -27.0,  -21.1,   -68.5,    8.1,  -19.8,   5.4);
        TERM( 3,-2,1,    0.9,   -0.5,    -0.4,   -2.0,   -0.1,  -0.8);
        TERM( 3,-3,0,   -5.4,   -4.1,   -19.1,   26.2,   -0.1,  -0.1);
        TERM( 4, 0,0,    0.6,    1.4,    -3.0,   -0.2,   -0.6,   1.6);
        TERM( 4,-1,0,    1.5,   -2.5,    12.4,    4.7,    1.0,  -1.1);
        TERM( 4,-2,0, -821.9,   -9.6,   -26.0, 1873.6,  -70.5,  -4.4);
        TERM( 4,-2,1,    4.1,  -21.9,   -50.3,   -9.9,    0.7,  -3.0);
        TERM( 4,-3,0,   -2.0,   -4.7,   -19.3,    8.2,   -0.1,  -0.3);
        TERM( 4,-4,0,   -1.5,    1.3,     6.5,    7.3,    0.0,   0.0);
        TERM( 5,-2,0,-2627.6,-1277.3,   117.4, -344.1,  -13.8,  -4.3);
        TERM( 5,-2,1,   63.0,  -98.6,    12.7,    6.7,    0.1,  -0.2);
        TERM( 5,-2,2,    1.7,    1.2,    -0.2,    0.3,    0.0,   0.0);
        TERM( 5,-3,0,    0.4,   -3.6,   -11.3,   -1.6,    0.0,  -0.3);
        TERM( 5,-4,0,   -1.4,    0.3,     1.5,    6.3,   -0.1,   0.0);
        TERM( 5,-5,0,    0.3,    0.6,     3.0,   -1.7,    0.0,   0.0);
        TERM( 6,-2,0, -146.7,  -73.7,   166.4, -334.3,  -43.6, -46.7);
        TERM( 6,-2,1,    5.2,   -6.8,    15.1,   11.4,    1.7,  -1.0);
        TERM( 6,-3,0,    1.5,   -2.9,    -2.2,   -1.3,    0.1,  -0.1);
        TERM( 6,-4,0,   -0.7,   -0.2,    -0.7,    2.8,    0.0,   0.0);
        TERM( 6,-5,0,    0.0,    0.5,     2.5,   -0.1,    0.0,   0.0);
        TERM( 6,-6,0,    0.3,   -0.1,    -0.3,   -1.2,    0.0,   0.0);
        TERM( 7,-2,0,   -9.6,   -3.9,     9.6,  -18.6,   -4.7,  -5.3);
        TERM( 7,-2,1,    0.4,   -0.5,     1.0,    0.9,    0.3,  -0.1);
        TERM( 7,-3,0,    3.0,    5.3,     7.5,   -3.5,    0.0,   0.0);
        TERM( 7,-4,0,    0.2,    0.4,     1.6,   -1.3,    0.0,   0.0);
        TERM( 7,-5,0,   -0.1,    0.2,     1.0,    0.5,    0.0,   0.0);
        TERM( 7,-6,0,    0.2,    0.0,     0.2,   -1.0,    0.0,   0.0);
        TERM( 8,-2,0,   -0.7,   -0.2,     0.6,   -1.2,   -0.4,  -0.4);
        TERM( 8,-3,0,    0.5,    1.0,    -2.0,    1.5,    0.1,   0.2);
        TERM( 8,-4,0,    0.4,    1.3,     3.6,   -0.9,    0.0,  -0.1);
        TERM( 9,-4,0,    4.0,   -8.7,   -19.9,   -9.9,    0.2,  -0.4);
        TERM( 9,-4,1,    0.5,    0.3,     0.8,   -1.8,    0.0,   0.0);
        TERM(10,-4,0,   21.3,  -16.8,     3.3,    3.3,    0.2,  -0.2);
        TERM(10,-4,1,    1.0,    1.7,    -0.4,    0.4,    0.0,   0.0);
        TERM(11,-4,0,    1.6,   -1.3,     3.0,    3.7,    0.8,  -0.2);
      end PERTJUP;

      procedure PERTURA is -- Stoerungen durch Uranus
      begin
        C(-1):=COS(M7); S(-1):=-SIN(M7);
        for I in reverse -4 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM( 0,-1,0,    1.0,    0.7,     0.4,   -1.5,    0.1,   0.0);
        TERM( 0,-2,0,    0.0,   -0.4,    -1.1,    0.1,   -0.1,  -0.1);
        TERM( 0,-3,0,   -0.9,   -1.2,    -2.7,    2.1,   -0.5,  -0.3);
        TERM( 1,-1,0,    7.8,   -1.5,     2.3,   12.7,    0.0,   0.0);
        TERM( 1,-2,0,   -1.1,   -8.1,     5.2,   -0.3,   -0.3,  -0.3);
        TERM( 1,-3,0,  -16.4,  -21.0,    -2.1,    0.0,    0.4,   0.0);
        TERM( 2,-1,0,    0.6,   -0.1,     0.1,    1.2,    0.1,   0.0);
        TERM( 2,-2,0,   -4.9,  -11.7,    31.5,  -13.3,    0.0,  -0.2);
        TERM( 2,-3,0,   19.1,   10.0,   -22.1,   42.1,    0.1,  -1.1);
        TERM( 2,-4,0,    0.9,   -0.1,     0.1,    1.4,    0.0,   0.0);
        TERM( 3,-2,0,   -0.4,   -0.9,     1.7,   -0.8,    0.0,  -0.3);
        TERM( 3,-3,0,    2.3,    0.0,     1.0,    5.7,    0.3,   0.3);
        TERM( 3,-4,0,    0.3,   -0.7,     2.0,    0.7,    0.0,   0.0);
        TERM( 3,-5,0,   -0.1,   -0.4,     1.1,   -0.3,    0.0,   0.0);
      end PERTURA;

      procedure PERTNEP is -- Stoerungen durch Neptun
      begin
        C(-1):=COS(M8); S(-1):=-SIN(M8);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM( 1,-1,0,   -1.3,   -1.2,     2.3,   -2.5,    0.0,   0.0);
        TERM( 1,-2,0,    1.0,   -0.1,     0.1,    1.4,    0.0,   0.0);
        TERM( 2,-2,0,    1.1,   -0.1,     0.2,    3.3,    0.0,   0.0);
      end PERTNEP;

      procedure PERTJUR is -- Stoerungen durch Jupiter und Uranus
        PHI,X,Y: REAL;
      begin
        PHI:=(-2.0*M5+5.0*M6-3.0*M7); X:=COS(PHI); Y:=SIN(PHI);
        DL:=DL-0.8*X-0.1*Y; DR:=DR-0.2*X+1.8*Y; DB:=DB+0.3*X+0.5*Y;
        ADDTHE(X,Y,C6(1),S6(1),X,Y);
        DL:=DL+(+2.4-0.7*T)*X+(27.8-0.4*T)*Y; DR:=DR+2.1*X-0.2*Y;
        ADDTHE(X,Y,C6(1),S6(1),X,Y);
        DL:=DL+0.1*X+1.6*Y; DR:=DR-3.6*X+0.3*Y; DB:=DB-0.2*X+0.6*Y;
      end PERTJUR;

    begin -- SAT200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M5:=P2*FRAC(0.0565314+8.4302963*T); M6:=P2*FRAC(0.8829867+3.3947688*T);
      M7:=P2*FRAC(0.3969537+1.1902586*T); M8:=P2*FRAC(0.7208473+0.6068623*T);
      C6(0):=1.0; S6(0):=0.0;  C6(1):=COS(M6); S6(1):=SIN(M6);
      for I in 2 .. 11 loop
        ADDTHE(C6(I-1),S6(I-1),C6(1),S6(1),C6(I),S6(I));
      end loop;
      PERTJUP; PERTURA; PERTNEP; PERTJUR;
      L:= 360.0*FRAC(0.2561136 + M6/P2 + ((5018.6+T*1.9)*T +DL)/1296.0E3 );
      R:= 9.557584 - 0.000186*T  +  DR*1.0E-5;
      B:= ( 175.1 - 10.2*T + DB ) / 3600.0;
    end SAT200;

    -------------------------------------------------------------------
    -- URA200: Uranus; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure URA200 (T:         REAL;
                      L,B,R: out REAL) is

      C7,S7:         array (-2..7) of REAL;
      C,S:           array (-8..0) of REAL;
      M5,M6,M7,M8:   REAL;
      U,V, DL,DR,DB: REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2-S1*S2; S:=S1*C2+C1*S2;
      end ADDTHE;

      procedure TERM (I7,I,IT:Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS:REAL) is
      begin
        if IT=0 then
          ADDTHE(C7(I7),S7(I7),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTJUP is -- Stoerungen durch Jupiter
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M5); S(-1):=-SIN(M5);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM(-1,-1,0,  0.0,    0.0,    -0.1,   1.7,  -0.1,   0.0);
        TERM( 0,-1,0,  0.5,   -1.2,    18.9,   9.1,  -0.9,   0.1);
        TERM( 1,-1,0,-21.2,   48.7,  -455.5,-198.8,   0.0,   0.0);
        TERM( 1,-2,0, -0.5,    1.2,   -10.9,  -4.8,   0.0,   0.0);
        TERM( 2,-1,0, -1.3,    3.2,   -23.2, -11.1,   0.3,   0.1);
        TERM( 2,-2,0, -0.2,    0.2,     1.1,   1.5,   0.0,   0.0);
        TERM( 3,-1,0,  0.0,    0.2,    -1.8,   0.4,   0.0,   0.0);
      end PERTJUP;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(-1):=COS(M6); S(-1):=-SIN(M6);
        for I in reverse -3 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM( 0,-1,0,  1.4,   -0.5,    -6.4,   9.0,  -0.4,  -0.8);
        TERM( 1,-1,0,-18.6,  -12.6,    36.7,-336.8,   1.0,   0.3);
        TERM( 1,-2,0, -0.7,   -0.3,     0.5,  -7.5,   0.1,   0.0);
        TERM( 2,-1,0, 20.0, -141.6,  -587.1,-107.0,   3.1,  -0.8);
        TERM( 2,-1,1,  1.0,    1.4,     5.8,  -4.0,   0.0,   0.0);
        TERM( 2,-2,0,  1.6,   -3.8,   -35.6, -16.0,   0.0,   0.0);
        TERM( 3,-1,0, 75.3, -100.9,   128.9,  77.5,  -0.8,   0.1);
        TERM( 3,-1,1,  0.2,    1.8,    -1.9,   0.3,   0.0,   0.0);
        TERM( 3,-2,0,  2.3,   -1.3,    -9.5, -17.9,   0.0,   0.1);
        TERM( 3,-3,0, -0.7,   -0.5,    -4.9,   6.8,   0.0,   0.0);
        TERM( 4,-1,0,  3.4,   -5.0,    21.6,  14.3,  -0.8,  -0.5);
        TERM( 4,-2,0,  1.9,    0.1,     1.2, -12.1,   0.0,   0.0);
        TERM( 4,-3,0, -0.1,   -0.4,    -3.9,   1.2,   0.0,   0.0);
        TERM( 4,-4,0, -0.2,    0.1,     1.6,   1.8,   0.0,   0.0);
        TERM( 5,-1,0,  0.2,   -0.3,     1.0,   0.6,  -0.1,   0.0);
        TERM( 5,-2,0, -2.2,   -2.2,    -7.7,   8.5,   0.0,   0.0);
        TERM( 5,-3,0,  0.1,   -0.2,    -1.4,  -0.4,   0.0,   0.0);
        TERM( 5,-4,0, -0.1,    0.0,     0.1,   1.2,   0.0,   0.0);
        TERM( 6,-2,0, -0.2,   -0.6,     1.4,  -0.7,   0.0,   0.0);
      end PERTSAT;

      procedure PERTNEP is -- Keplerterme und Stoerungen durch Neptun
      begin
        C(-1):=COS(M8); S(-1):=-SIN(M8);
        for I in reverse -7 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM( 1, 0,0,-78.1,19518.1,-90718.2,-334.7,2759.5,-311.9);
        TERM( 1, 0,1,-81.6,  107.7,  -497.4,-379.5,  -2.8, -43.7);
        TERM( 1, 0,2, -6.6,   -3.1,    14.4, -30.6,  -0.4,  -0.5);
        TERM( 1, 0,3,  0.0,   -0.5,     2.4,   0.0,   0.0,   0.0);
        TERM( 2, 0,0, -2.4,  586.1, -2145.2, -15.3, 130.6, -14.3);
        TERM( 2, 0,1, -4.5,    6.6,   -24.2, -17.8,   0.7,  -1.6);
        TERM( 2, 0,2, -0.4,    0.0,     0.1,  -1.4,   0.0,   0.0);
        TERM( 3, 0,0,  0.0,   24.5,   -76.2,  -0.6,   7.0,  -0.7);
        TERM( 3, 0,1, -0.2,    0.4,    -1.4,  -0.8,   0.1,  -0.1);
        TERM( 4, 0,0,  0.0,    1.1,    -3.0,   0.1,   0.4,   0.0);
        TERM(-1,-1,0, -0.2,    0.2,     0.7,   0.7,  -0.1,   0.0);
        TERM( 0,-1,0, -2.8,    2.5,     8.7,  10.5,  -0.4,  -0.1);
        TERM( 1,-1,0,-28.4,   20.3,   -51.4, -72.0,   0.0,   0.0);
        TERM( 1,-2,0, -0.6,   -0.1,     4.2, -14.6,   0.2,   0.4);
        TERM( 1,-3,0,  0.2,    0.5,     3.4,  -1.6,  -0.1,   0.1);
        TERM( 2,-1,0, -1.8,    1.3,    -5.5,  -7.7,   0.0,   0.3);
        TERM( 2,-2,0, 29.4,   10.2,   -29.0,  83.2,   0.0,   0.0);
        TERM( 2,-3,0,  8.8,   17.8,   -41.9,  21.5,  -0.1,  -0.3);
        TERM( 2,-4,0,  0.0,    0.1,    -2.1,  -0.9,   0.1,   0.0);
        TERM( 3,-2,0,  1.5,    0.5,    -1.7,   5.1,   0.1,  -0.2);
        TERM( 3,-3,0,  4.4,   14.6,   -84.3,  25.2,   0.1,  -0.1);
        TERM( 3,-4,0,  2.4,   -4.5,    12.0,   6.2,   0.0,   0.0);
        TERM( 3,-5,0,  2.9,   -0.9,     2.1,   6.2,   0.0,   0.0);
        TERM( 4,-3,0,  0.3,    1.0,    -4.0,   1.1,   0.1,  -0.1);
        TERM( 4,-4,0,  2.1,   -2.7,    17.9,  14.0,   0.0,   0.0);
        TERM( 4,-5,0,  3.0,   -0.4,     2.3,  17.6,  -0.1,  -0.1);
        TERM( 4,-6,0, -0.6,   -0.5,     1.1,  -1.6,   0.0,   0.0);
        TERM( 5,-4,0,  0.2,   -0.2,     1.0,   0.8,   0.0,   0.0);
        TERM( 5,-5,0, -0.9,   -0.1,     0.6,  -7.1,   0.0,   0.0);
        TERM( 5,-6,0, -0.5,   -0.6,     3.8,  -3.6,   0.0,   0.0);
        TERM( 5,-7,0,  0.0,   -0.5,     3.0,   0.1,   0.0,   0.0);
        TERM( 6,-6,0,  0.2,    0.3,    -2.7,   1.6,   0.0,   0.0);
        TERM( 6,-7,0, -0.1,    0.2,    -2.0,  -0.4,   0.0,   0.0);
        TERM( 7,-7,0,  0.1,   -0.2,     1.3,   0.5,   0.0,   0.0);
        TERM( 7,-8,0,  0.1,    0.0,     0.4,   0.9,   0.0,   0.0);
      end PERTNEP;

      procedure PERTJSU is -- Stoerungen durch Jupiter und Saturn
      begin
        C(-1):=COS(M6);         S(-1):=-SIN(M6);
        C(-4):=COS(-4.0*M6+2.0*M5); S(-4):= SIN(-4.0*M6+2.0*M5);
        for I in reverse -5 .. -4 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(-2,-4,0, -0.7,    0.4,    -1.5,  -2.5,   0.0,   0.0);
        TERM(-1,-4,0, -0.1,   -0.1,    -2.2,   1.0,   0.0,   0.0);
        TERM( 1,-5,0,  0.1,   -0.4,     1.4,   0.2,   0.0,   0.0);
        TERM( 1,-6,0,  0.4,    0.5,    -0.8,  -0.8,   0.0,   0.0);
        TERM( 2,-6,0,  5.7,    6.3,    28.5, -25.5,   0.0,   0.0);
        TERM( 2,-6,1,  0.1,   -0.2,    -1.1,  -0.6,   0.0,   0.0);
        TERM( 3,-6,0, -1.4,   29.2,   -11.4,   1.1,   0.0,   0.0);
        TERM( 3,-6,1,  0.8,   -0.4,     0.2,   0.3,   0.0,   0.0);
        TERM( 4,-6,0,  0.0,    1.3,    -6.0,  -0.1,   0.0,   0.0);
      end PERTJSU;

    begin -- URA200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M5:=P2*FRAC(0.0564472+8.4302889*T); M6:=P2*FRAC(0.8829611+3.3947583*T);
      M7:=P2*FRAC(0.3967117+1.1902849*T); M8:=P2*FRAC(0.7216833+0.6068528*T);
      C7(0):=1.0; S7(0):=0.0; C7(1):=COS(M7); S7(1):=SIN(M7);
      for I in 2 .. 7 loop
        ADDTHE(C7(I-1),S7(I-1),C7(1),S7(1),C7(I),S7(I));
      end loop;
      for I in 1 .. 2 loop
        C7(-I):=C7(I); S7(-I):=-S7(I);
      end loop;
      PERTJUP; PERTSAT; PERTNEP; PERTJSU;
      L:= 360.0*FRAC(0.4734843 + M7/P2 + ((5082.3+34.2*T)*T+DL)/1296.0E3 );
      R:= 19.211991 + (-0.000333-0.000005*T)*T  +  DR*1.0E-5;
      B:= (-130.61 + (-0.54+0.04*T)*T + DB ) / 3600.0;
    end URA200;

    -------------------------------------------------------------------
    -- NEP200: Neptun; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure NEP200 (T:         REAL;
                      L,B,R: out REAL) is

      C8,S8:         array ( 0..6) of REAL;
      C,S:           array (-6..0) of REAL;
      M5,M6,M7,M8:   REAL;
      U,V, DL,DR,DB: REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2-S1*S2; S:=S1*C2+C1*S2;
      end;

      procedure TERM (I1,I,IT:Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS:REAL) is
      begin
        if IT=0 then
          ADDTHE(C8(I1),S8(I1),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTJUP is -- Stoerungen durch Jupiter
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M5); S(-1):=-SIN(M5);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM(0,-1,0,  0.1,   0.1,    -3.0,   1.8,   -0.3, -0.3);
        TERM(1, 0,0,  0.0,   0.0,   -15.9,   9.0,    0.0,  0.0);
        TERM(1,-1,0,-17.6, -29.3,   416.1,-250.0,    0.0,  0.0);
        TERM(1,-2,0, -0.4,  -0.7,    10.4,  -6.2,    0.0,  0.0);
        TERM(2,-1,0, -0.2,  -0.4,     2.4,  -1.4,    0.4, -0.3);
      end;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M6); S(-1):=-SIN(M6);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM(0,-1,0, -0.1,   0.0,     0.2,  -1.8,   -0.1, -0.5);
        TERM(1, 0,0,  0.0,   0.0,    -8.3, -10.4,    0.0,  0.0);
        TERM(1,-1,0, 13.6, -12.7,   187.5, 201.1,    0.0,  0.0);
        TERM(1,-2,0,  0.4,  -0.4,     4.5,   4.5,    0.0,  0.0);
        TERM(2,-1,0,  0.4,  -0.1,     1.7,  -3.2,    0.2,  0.2);
        TERM(2,-2,0, -0.1,   0.0,    -0.2,   2.7,    0.0,  0.0);
      end PERTSAT;

      procedure PERTURA is -- Keplerterme und Stoerungen durch Uranus
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M7); S(-1):=-SIN(M7);
        for I in reverse -5 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(1, 0,0, 32.3,3549.5,-25880.2, 235.8,-6360.5,374.0);
        TERM(1, 0,1, 31.2,  34.4,  -251.4, 227.4,   34.9, 29.3);
        TERM(1, 0,2, -1.4,   3.9,   -28.6, -10.1,    0.0, -0.9);
        TERM(2, 0,0,  6.1,  68.0,  -111.4,   2.0,  -54.7,  3.7);
        TERM(2, 0,1,  0.8,  -0.2,    -2.1,   2.0,   -0.2,  0.8);
        TERM(3, 0,0,  0.1,   1.0,    -0.7,   0.0,   -0.8,  0.1);
        TERM(0,-1,0, -0.1,  -0.3,    -3.6,   0.0,    0.0,  0.0);
        TERM(1, 0,0,  0.0,   0.0,     5.5,  -6.9,    0.1,  0.0);
        TERM(1,-1,0, -2.2,  -1.6,  -116.3, 163.6,    0.0, -0.1);
        TERM(1,-2,0,  0.2,   0.1,    -1.2,   0.4,    0.0, -0.1);
        TERM(2,-1,0,  4.2,  -1.1,    -4.4, -34.6,   -0.2,  0.1);
        TERM(2,-2,0,  8.6,  -2.9,   -33.4, -97.0,    0.2,  0.1);
        TERM(3,-1,0,  0.1,  -0.2,     2.1,  -1.2,    0.0,  0.1);
        TERM(3,-2,0, -4.6,   9.3,    38.2,  19.8,    0.1,  0.1);
        TERM(3,-3,0, -0.5,   1.7,    23.5,   7.0,    0.0,  0.0);
        TERM(4,-2,0,  0.2,   0.8,     3.3,  -1.5,   -0.2, -0.1);
        TERM(4,-3,0,  0.9,   1.7,    17.9,  -9.1,   -0.1,  0.0);
        TERM(4,-4,0, -0.4,  -0.4,    -6.2,   4.8,    0.0,  0.0);
        TERM(5,-3,0, -1.6,  -0.5,    -2.2,   7.0,    0.0,  0.0);
        TERM(5,-4,0, -0.4,  -0.1,    -0.7,   5.5,    0.0,  0.0);
        TERM(5,-5,0,  0.2,   0.0,     0.0,  -3.5,    0.0,  0.0);
        TERM(6,-4,0, -0.3,   0.2,     2.1,   2.7,    0.0,  0.0);
        TERM(6,-5,0,  0.1,  -0.1,    -1.4,  -1.4,    0.0,  0.0);
        TERM(6,-6,0, -0.1,   0.1,     1.4,   0.7,    0.0,  0.0);
      end PERTURA;

    begin -- NEP200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M5:=P2*FRAC(0.0563867+8.4298907*T); M6:=P2*FRAC(0.8825086+3.3957748*T);
      M7:=P2*FRAC(0.3965358+1.1902851*T); M8:=P2*FRAC(0.7214906+0.6068526*T);
      C8(0):=1.0; S8(0):=0.0; C8(1):=COS(M8); S8(1):=SIN(M8);
      for I in 2 .. 6 loop
        ADDTHE(C8(I-1),S8(I-1),C8(1),S8(1),C8(I),S8(I));
      end loop;
      PERTJUP; PERTSAT; PERTURA;
      L:= 360.0*FRAC(0.1254046 + M8/P2 + ((4982.8-21.3*T)*T+DL)/1296.0E3 );
      R:= 30.072984 + (0.001234+0.000003*T) * T  +  DR*1.0E-5;
      B:= (  54.77 + ( 0.26 + 0.06*T) * T  +  DB ) / 3600.0;
    end NEP200;

    ----------------------------------------------------------------------
    -- PLU200: Pluto; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums; nur gueltig von ca. 1890-2100 !!
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    ----------------------------------------------------------------------
    procedure PLU200 (T:         REAL;
                      L,B,R: out REAL) is

      C9,S9:    array ( 0..6) of REAL;
      C,S:      array (-3..2) of REAL;
      M5,M6,M9: REAL;
      DL,DR,DB: REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S:         out REAL) is
      begin
        C:=C1*C2-S1*S2; S:=S1*C2+C1*S2;
      end ADDTHE;

      procedure TERM (I9,I:Integer;DLC,DLS,DRC,DRS,DBC,DBS:REAL) is
        U,V: REAL;
      begin
        ADDTHE(C9(I9),S9(I9),C(I),S(I),U,V);
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;

      procedure PERTJUP is -- Keplerterme und Stoerungen durch Jupiter
      begin
        C(0):=1.0; S(0):=0.0;  C(1):=COS(M5); S(1):=SIN(M5);
        for I in reverse -1 .. -0 loop
          ADDTHE(C(I),S(I),C(1),-S(1),C(I-1),S(I-1));
        end loop;
        ADDTHE(C(1),S(1),C(1),S(1),C(2),S(2));
        TERM(1, 0,   0.06,100924.08,-960396.0,15965.1,51987.68,-24288.76);
        TERM(2, 0,3274.74, 17835.12,-118252.2, 3632.4,12687.49, -6049.72);
        TERM(3, 0,1543.52,  4631.99, -21446.6, 1167.0, 3504.00, -1853.10);
        TERM(4, 0, 688.99,  1227.08,  -4823.4,  213.5, 1048.19,  -648.26);
        TERM(5, 0, 242.27,   415.93,  -1075.4,  140.6,  302.33,  -209.76);
        TERM(6, 0, 138.41,   110.91,   -308.8,  -55.3,  109.52,   -93.82);
        TERM(3,-1,  -0.99,     5.06,    -25.6,   19.8,    1.26,    -1.96);
        TERM(2,-1,   7.15,     5.61,    -96.7,   57.2,    1.64,    -2.16);
        TERM(1,-1,  10.79,    23.13,   -390.4,  236.4,   -0.33,     0.86);
        TERM(0, 1,  -0.23,     4.43,    102.8,   63.2,    3.15,     0.34);
        TERM(1, 1,  -1.10,    -0.92,     11.8,   -2.3,    0.43,     0.14);
        TERM(2, 1,   0.62,     0.84,      2.3,    0.7,    0.05,    -0.04);
        TERM(3, 1,  -0.38,    -0.45,      1.2,   -0.8,    0.04,     0.05);
        TERM(4, 1,   0.17,     0.25,      0.0,    0.2,   -0.01,    -0.01);
        TERM(3,-2,   0.06,     0.07,     -0.6,    0.3,    0.03,    -0.03);
        TERM(2,-2,   0.13,     0.20,     -2.2,    1.5,    0.03,    -0.07);
        TERM(1,-2,   0.32,     0.49,     -9.4,    5.7,   -0.01,     0.03);
        TERM(0,-2,  -0.04,    -0.07,      2.6,   -1.5,    0.07,    -0.02);
      end PERTJUP;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(1):=COS(M6); S(1):=SIN(M6);
        for I in reverse -1 .. -0 loop
          ADDTHE(C(I),S(I),C(1),-S(1),C(I-1),S(I-1));
        end loop;
        TERM(1,-1, -29.47,    75.97,   -106.4, -204.9,  -40.71,   -17.55);
        TERM(0, 1, -13.88,    18.20,     42.6,  -46.1,    1.13,     0.43);
        TERM(1, 1,   5.81,   -23.48,     15.0,   -6.8,   -7.48,     3.07);
        TERM(2, 1, -10.27,    14.16,     -7.9,    0.4,    2.43,    -0.09);
        TERM(3, 1,   6.86,   -10.66,      7.3,   -0.3,   -2.25,     0.69);
        TERM(2,-2,   4.32,     2.00,      0.0,   -2.2,   -0.24,     0.12);
        TERM(1,-2,  -5.04,    -0.83,     -9.2,   -3.1,    0.79,    -0.24);
        TERM(0,-2,   4.25,     2.48,     -5.9,   -3.3,    0.58,     0.02);
      end PERTSAT;

      procedure PERTJUS is -- Stoerungen durch Jupiter und Saturn
        PHI,X,Y: REAL;
      begin
        PHI:=(M5-M6); X:=COS(PHI); Y:=SIN(PHI);
        DL:=DL-9.11*X+0.12*Y; DR:=DR-3.4*X-3.3*Y; DB:=DB+0.81*X+0.78*Y;
        ADDTHE(X,Y,C9(1),S9(1),X,Y);
        DL:=DL+5.92*X+0.25*Y; DR:=DR+2.3*X-3.8*Y; DB:=DB-0.67*X-0.51*Y;
      end PERTJUS;

      procedure PREC (T:          REAL;
                      L,B: in out REAL) is -- Praez. (1950->Aequin.d.Dat.)
        DEG : constant := 57.2957795;
        D,PPI,PI,P,C1,S1,C2,S2,C3,S3,X,Y,Z: REAL;
      begin
        D:=T+0.5; L:=L/DEG; B:=B/DEG;
        PPI:=3.044; PI:=2.28E-4*D; P:=(0.0243764+5.39E-6*D)*D;
        C1:=COS(PI); C2:=COS(B); C3:=COS(PPI-L);
        S1:=SIN(PI); S2:=SIN(B); S3:=SIN(PPI-L);
        X:=C2*C3; Y:=C1*C2*S3-S1*S2; Z:=S1*C2*S3+C1*S2;
        B := DEG * ARCTAN( Z / SQRT((1.0-Z)*(1.0+Z)) );
        if X>0.0 then
          L:=360.0*FRAC((PPI+P-ARCTAN(Y/X))/P2);
        else
          L:=360.0*FRAC((PPI+P-ARCTAN(Y/X))/P2+0.5);
        end if;
      end PREC;

    begin -- PLU200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M5:=P2*FRAC(0.0565314+8.4302963*T); M6:=P2*FRAC(0.8829867+3.3947688*T);
      M9:=P2*FRAC(0.0385795+0.4026667*T);
      C9(0):=1.0; S9(0):=0.0;  C9(1):=COS(M9); S9(1):=SIN(M9);
      for I in 2 .. 6 loop
        ADDTHE(C9(I-1),S9(I-1),C9(1),S9(1),C9(I),S9(I));
      end loop;
      PERTJUP; PERTSAT; PERTJUS;
      L:= 360.0*FRAC( 0.6232469 + M9/P2 + DL/1296.0E3 );
      R:= 40.7247248  +  DR * 1.0E-5;
      B:= -3.909434  +  DB / 3600.0;
      PREC(T,L,B);
    end PLU200;

  end PLALIB;


  --========================================
  -- Unit PNULIB: Praezession und Nutation
  --========================================
  package body PNULIB is

    --------------------------------------------------------------
    -- NUTEQU: Transformation von mittleren in wahre aequatoriale
    --         Koordinaten (Terme >0.1" nach IAU 1980)
    --         T = (JD-2451545.0)/36525.0
    --------------------------------------------------------------
    procedure NUTEQU (T    :        REAL;
                      X,Y,Z: in out REAL) is
      ARC : constant := 206264.8062; -- Bogensekunden pro radian = 3600*180/pi
      LS,D,F,N,EPS : REAL;
      DPSI,DEPS,C,S: REAL;
      DX,DY,DZ     : REAL;
    begin
      LS  := P2*FRAC(0.993133+  99.997306*T);   -- mittl. Anomalie Sonne
      D   := P2*FRAC(0.827362+1236.853087*T);   -- Diff. Laenge Mond-Sonne
      F   := P2*FRAC(0.259089+1342.227826*T);   -- Knotenabstand
      N   := P2*FRAC(0.347346-   5.372447*T);   -- Laenge des aufst.Knotens
      EPS := 0.4090928-2.2696E-4*T;             -- Ekliptikschiefe
      DPSI := ( -17.200*SIN(N)   - 1.319*SIN(2.0*(F-D+N)) - 0.227*SIN(2.0*(F+N))
                + 0.206*SIN(2.0*N) + 0.143*SIN(LS) ) / ARC;
      DEPS := ( + 9.203*COS(N)   + 0.574*COS(2.0*(F-D+N)) + 0.098*COS(2.0*(F+N))
                - 0.090*COS(2.0*N)                 ) / ARC;
      C := DPSI*COS(EPS);  S := DPSI*SIN(EPS);
      DX := -(C*Y+S*Z); DY := (C*X-DEPS*Z); DZ := (S*X+DEPS*Y);
      X  :=  X + DX;    Y  := Y + DY;       Z  := Z + DZ;
    end NUTEQU;

    -----------------------------------------------------------------
    -- PMATECL: Berechnung der Praezessionsmatrix A[i,j] fuer
    --          ekliptikale Koordinaten vom Aequinoktium T1 nach T2
    --          ( T=(JD-2451545.0)/36525 )
    -----------------------------------------------------------------
    procedure PMATECL (T1,T2:     REAL;
                       A    : out REAL33) is
      SEC : constant := 3600.0;
      DT,PPI,PI,PA     : REAL;
      C1,S1,C2,S2,C3,S3: REAL;
    begin
      DT:=T2-T1;
      PPI := 174.876383889 +( ((3289.4789+0.60622*T1)*T1) +
                ((-869.8089-0.50491*T1) + 0.03536*DT)*DT )/SEC;
      PI  := ( (47.0029-(0.06603-0.000598*T1)*T1)+
               ((-0.03302+0.000598*T1)+0.000060*DT)*DT )*DT/SEC;
      PA  := ( (5029.0966+(2.22226-0.000042*T1)*T1)+
               ((1.11113-0.000042*T1)-0.000006*DT)*DT )*DT/SEC;
      C1:=CS(PPI+PA);  C2:=CS(PI);  C3:=CS(PPI);
      S1:=SN(PPI+PA);  S2:=SN(PI);  S3:=SN(PPI);
      A(1,1):=+C1*C3+S1*C2*S3; A(1,2):=+C1*S3-S1*C2*C3; A(1,3):=-S1*S2;
      A(2,1):=+S1*C3-C1*C2*S3; A(2,2):=+S1*S3+C1*C2*C3; A(2,3):=+C1*S2;
      A(3,1):=+S2*S3;          A(3,2):=-S2*C3;          A(3,3):=+C2;
    end PMATECL;

    ------------------------------------------------------------------
    -- PMATEQU: Berechnung der Praezessionsmatrix A[i,j] fuer
    --          aequatoriale Koordinaten vom Aequinoktium T1 nach T2
    --          ( T=(JD-2451545.0)/36525 )
    ------------------------------------------------------------------
    procedure PMATEQU (T1,T2:     REAL;
                       A    : out REAL33) is
      SEC : constant := 3600.0;
      DT,ZETA,Z,THETA: REAL;
      C1,S1,C2,S2,C3,S3: REAL;
    begin
      DT:=T2-T1;
      ZETA  :=  ( (2306.2181+(1.39656-0.000139*T1)*T1)+
                  ((0.30188-0.000345*T1)+0.017998*DT)*DT )*DT/SEC;
      Z     :=  ZETA + ( (0.79280+0.000411*T1)+0.000205*DT)*DT*DT/SEC;
      THETA :=  ( (2004.3109-(0.85330+0.000217*T1)*T1)-
                  ((0.42665+0.000217*T1)+0.041833*DT)*DT )*DT/SEC;
      C1:=CS(Z);  C2:=CS(THETA);  C3:=CS(ZETA);
      S1:=SN(Z);  S2:=SN(THETA);  S3:=SN(ZETA);
      A(1,1):=-S1*S3+C1*C2*C3; A(1,2):=-S1*C3-C1*C2*S3; A(1,3):=-C1*S2;
      A(2,1):=+C1*S3+S1*C2*C3; A(2,2):=+C1*C3-S1*C2*S3; A(2,3):=-S1*S2;
      A(3,1):=+S2*C3;          A(3,2):=-S2*S3;          A(3,3):=+C2;
    end;

    ----------------------------------------------------------------------------
    -- PN_MATRIX: Matrix fuer Praezession und Nutation vom mittleren
    --            Aequinoktium T0 ins wahre Aequinoktium T.
    -- T0  Ausgangsepoche in julian. Jahrh. seit J2000; T0=(JD0-2451545)/36525
    -- T   Endepoche in julianischen Jahrh. seit J2000; T=(JD-2451545)/36525
    ----------------------------------------------------------------------------
    procedure PN_MATRIX (T0,T:     REAL;
                         A   : out REAL33) is
    begin
      PMATEQU(T0,T,A);                  -- Praezessionsmatrix T0->T
      NUTEQU(T,A(1,1),A(2,1),A(3,1));
      NUTEQU(T,A(1,2),A(2,2),A(3,2));   -- Nutations-Transformation der
      NUTEQU(T,A(1,3),A(2,3),A(3,3));   -- drei Spaltenvektoren von A
    end PN_MATRIX;

    ----------------------------------------------------------------------
    -- PRECART: Praezessions-Transformation  bei bekannter Matrix A[i,j]
    --          fuer ekliptikale und aequatoriale Koordinaten
    --          ( zu verwenden in Verbindung mit PMATECL und PMATEQU )
    ----------------------------------------------------------------------
    procedure PRECART (A    :        REAL33;
                       X,Y,Z: in out REAL) is
      U,V,W: REAL;
    begin
      U := A(1,1)*X+A(1,2)*Y+A(1,3)*Z;
      V := A(2,1)*X+A(2,2)*Y+A(2,3)*Z;
      W := A(3,1)*X+A(3,2)*Y+A(3,3)*Z;
      X:=U; Y:=V; Z:=W;
    end PRECART;

    procedure PRECART (A:        REAL33;
                       V: in out VECTOR) is
    begin
      PRECART (A, V(X), V(Y), V(Z));
    end PRECART;

  end PNULIB;


  --======================================
  -- Unit SPHLIB: sphaerische Astronomie
  --======================================
  package body SPHLIB is

    ------------------------------------------------------------------
    -- ABERRAT: Geschwindigkeitsvektor der Erde
    --          (aequatorial, in Einheiten der Lichtgeschwindigkeit)
    ------------------------------------------------------------------
    procedure ABERRAT (T:     REAL;
                       V: out VECTOR) is
      L,CL: REAL;
    begin
      L := P2*FRAC(0.27908+100.00214*T);  CL:=COS(L);
      V(X) := -0.994E-4*SIN(L); V(Y) := +0.912E-4*CL; V(Z) := +0.395E-4*CL;
    end ABERRAT;

    ---------------------------------------------------------------------------
    -- APPARENT:  scheinbare Sternkoordinaten
    --   PNMAT:   Matrix fuer Praezession und Nutation
    --   V:       Geschwindigkeit der Erde (aequatorial, in Einheiten von c)
    --   RA,DEC:  Rektaszension und Deklination
    ---------------------------------------------------------------------------
    procedure APPARENT (PNMAT   :        REAL33;
                        V       :        VECTOR;
                        RA, DEC : in out REAL) is
      K: VECTOR;
      R: REAL;
    begin
      K := CART (1.0, DEC, RA); -- kartesische Sternkoordinaten
      PRECART (PNMAT, K);       -- multiplizieren mit Praez.-Nut.-Matrix
      K := K + V;               -- Aberration
      POLAR (K, R, DEC, RA);    -- neue Polarkoordinaten RA und DEC
    end APPARENT;

    -----------------------------------------------------------------
    -- ECLEQU: Umwandlung ekliptikaler Koordinaten in aequatoriale
    --         (T: Aequinoktium in julian.Jahrhunderten seit J2000)
    -----------------------------------------------------------------
    procedure ECLEQU (T    :        REAL;
                      X,Y,Z: in out REAL) is
      pragma Unreferenced (X);
      EPS,C,S,V: REAL;
    begin
      EPS:=23.43929111-(46.8150+(0.00059-0.001813*T)*T)*T/3600.0;
      C:=CS(EPS);  S:=SN(EPS);
      V:=+C*Y-S*Z;  Z:=+S*Y+C*Z;  Y:=V;
    end ECLEQU;

    ----------------------------------------------------------------
    -- EQUECL: Umwandlung aequatorialer Koordinaten in ekliptikale
    --         (T: Aequinoktium in julian.Jahrhunderten seit J2000)
    ----------------------------------------------------------------
    procedure EQUECL (T    :        REAL;
                      X,Y,Z: in out REAL) is
      pragma Unreferenced (X);
      EPS,C,S,V: REAL;
    begin
      EPS:=23.43929111-(46.8150+(0.00059-0.001813*T)*T)*T/3600.0;
      C:=CS(EPS);  S:=SN(EPS);
      V:=+C*Y+S*Z;  Z:=-S*Y+C*Z;  Y:=V;
    end EQUECL;

    --------------------------------------------------------------------
    -- EQUHOR: Umwandlung aequatorialer Koordinaten ins Horizontsystem
    --   DEC  : Deklination (-90 Grad .. +90 Grad)
    --   TAU  : Stundenwinkel (0 Grad .. 360 Grad)
    --   PHI  : Geographische Breite (in Grad)
    --   H    : Hoehe des Gestirns in Grad
    --   AZ   : Azimut (0 Grad .. 360 Grad, Zaehlung S->W->N->O->S)
    --------------------------------------------------------------------
    procedure EQUHOR (DEC,TAU,PHI:     REAL;
                      H,AZ       : out REAL) is
      CS_PHI,SN_PHI, CS_DEC,SN_DEC, CS_TAU, X,Y,Z, DUMMY: REAL;
    begin
      CS_PHI:=CS(PHI); SN_PHI:=SN(PHI);
      CS_DEC:=CS(DEC); SN_DEC:=SN(DEC); CS_TAU:=CS(TAU);
      X:=CS_DEC*SN_PHI*CS_TAU - SN_DEC*CS_PHI;
      Y:=CS_DEC*SN(TAU);
      Z:=CS_DEC*CS_PHI*CS_TAU + SN_DEC*SN_PHI;
      POLAR (X,Y,Z, DUMMY,H,AZ);
    end EQUHOR;

    ----------------------------------------------------------------------------
    -- HOREQU: Umwandlung von Koordinaten im Horizontsystem in aequatoriale K.
    --   H,AZ : Hoehe bzw. Azimut des Gestirns in Grad
    --   PHI  : Geographische Breite (in Grad)
    --   DEC  : Deklination (-90 Grad .. +90 Grad)
    --   TAU  : Stundenwinkel (0 Grad .. 360 Grad)
    ----------------------------------------------------------------------------
    procedure HOREQU (H,AZ,PHI:     REAL;
                      DEC,TAU : out REAL) is
      CS_PHI,SN_PHI, CS_H,SN_H, CS_AZ, X,Y,Z, DUMMY: REAL;
    begin
      CS_PHI:=CS(PHI); SN_PHI:=SN(PHI);
      CS_H  :=CS(H);   SN_H  :=SN(H); CS_AZ:=CS(AZ);
      X := CS_H*SN_PHI*CS_AZ+SN_H*CS_PHI;
      Y := CS_H*SN(AZ);
      Z := SN_H*SN_PHI-CS_H*CS_PHI*CS_AZ;
      POLAR (X,Y,Z, DUMMY,DEC,TAU);
    end HOREQU;

    ----------------------------------------------------------------------------
    -- EQUSTD: Transformation aequatorialer Koordinaten in Standardkoordinaten
    --   RA0,DEC0: Rektaszension und Deklination der optischen Achse (in Grad)
    --   RA,DEC:   Rektaszension und Deklination (in Grad)
    --   XX,YY:    Standardkoordinaten
    ----------------------------------------------------------------------------
    procedure EQUSTD (RA0,DEC0,RA,DEC:     REAL;
                      XX,YY          : out REAL) is
      C: REAL;
    begin
      C  := CS(DEC0)*CS(DEC)*CS(RA-RA0)+SN(DEC0)*SN(DEC);
      XX := - ( CS(DEC)*SN(RA-RA0) ) / C;
      YY := - ( SN(DEC0)*CS(DEC)*CS(RA-RA0)-CS(DEC0)*SN(DEC) ) / C;
    end EQUSTD;

    ------------------------------------------------------------------
    -- GAUSVEC: Berechnung der Gauss'schen Vektoren  (P,Q,R) aus
    --          ekliptikalen Bahnelementen:
    --          LAN = Knotenlaenge (longitude of ascending node)
    --          INC = Bahnneigung (inclination)
    --          AOP = Argument des Perihels (argument of perihelion)
    ------------------------------------------------------------------
    procedure GAUSVEC (LAN,INC,AOP :     REAL;
                       PQR         : out REAL33) is
      C1,S1,C2,S2,C3,S3: REAL;
    begin
      C1:=CS(AOP);  C2:=CS(INC);  C3:=CS(LAN);
      S1:=SN(AOP);  S2:=SN(INC);  S3:=SN(LAN);
      PQR(1,1):=+C1*C3-S1*C2*S3; PQR(1,2):=-S1*C3-C1*C2*S3; PQR(1,3):=+S2*S3;
      PQR(2,1):=+C1*S3+S1*C2*C3; PQR(2,2):=-S1*S3+C1*C2*C3; PQR(2,3):=-S2*C3;
      PQR(3,1):=+S1*S2;          PQR(3,2):=+C1*S2;          PQR(3,3):=+C2;
    end GAUSVEC;

    -------------------------------------------------------------------
    -- ORBECL: Transformation von Koordinaten im System der Bahnebene
    --         in ekliptikale Koordinaten
    -------------------------------------------------------------------
    procedure ORBECL (XX,YY:     REAL;
                      PQR:       REAL33;
                      X,Y,Z: out REAL) is
    begin
      X:=PQR(1,1)*XX+PQR(1,2)*YY;
      Y:=PQR(2,1)*XX+PQR(2,2)*YY;
      Z:=PQR(3,1)*XX+PQR(3,2)*YY;
    end ORBECL;

    ----------------------------------------------------------------------------
    -- SITE:  berechnet geozentrische aus geographischen Beobachterkoordinaten
    --        RCPHI:  r * cos(phi') (geozentrisch; in Erdradien)
    --        RSPHI:  r * sin(phi') (geozentrisch; in Erdradien)
    --        PHI:    geographische Breite (in Grad)
    ----------------------------------------------------------------------------
    procedure SITE (PHI:             REAL;
                    RCPHI,RSPHI: out REAL) is
      E2 : constant := 0.006694; -- e**2=f(2-f) mit Erdabplattung f=1/298.257
      N,SNPHI: REAL;
    begin
      SNPHI := SN(PHI);    N := 1.0/SQRT(1.0-E2*SNPHI*SNPHI);
      RCPHI := N*CS(PHI);  RSPHI := (1.0-E2)*N*SNPHI;
    end SITE;

    -------------------------------------------------------------------------------
    -- STDEQU: Transformation von Standardkoordinaten in aequatoriale Koordinaten
    --   RA0,DEC0: Rektaszension und Deklination der optischen Achse (in Grad)
    --   XX,YY:    Standardkoordinaten
    --   RA,DEC:   Rektaszension und Deklination (in Grad)
    -------------------------------------------------------------------------------
    procedure STDEQU (RA0,DEC0,XX,YY:     REAL;
                      RA,DEC        : out REAL) is
    begin
      RA  := RA0 + ATN ( -XX / (CS(DEC0)-YY*SN(DEC0)) );
      DEC := ASN ( (SN(DEC0)+YY*CS(DEC0))/SQRT(1.0+XX*XX+YY*YY));
    end STDEQU;

  end SPHLIB;


  --========================
  -- Unit MOOLIB: Mondbahn
  --========================

  package body MOOLIB is

    ----------------------------------------------------------------------------
    -- MINI_MOON: Mondkoordinaten geringer Genauigkeit (ca.5'/1')
    --            T  : Zeit in jul.Jahrh. seit J2000  ( T=(JD-2451545)/36525 )
    --            RA : Rektaszension (in h)
    --            DEC: Deklination (in Grad)
    --            (Aequinoktium des Datums)
    ----------------------------------------------------------------------------
    procedure MINI_MOON (T     :     REAL;
                         RA,DEC: out REAL) is
      ARC    : constant := 206264.8062;
      COSEPS : constant := 0.91748; -- cos(Ekliptikschiefe)
      SINEPS : constant := 0.39778; -- sin(Ekliptikschiefe)
      L0,L,LS,F,D,H,S,N,DL,CB    : REAL;
      L_MOON,B_MOON,V,W,X,Y,Z,RHO: REAL;
    begin
      -- mittlere Elemente der Mondbahn
      L0:=   FRAC(0.606433+1336.855225*T); -- mittl. Laenge des Mondes (in r)
      L :=P2*FRAC(0.374897+1325.552410*T); -- mittl. Anomalie des Mondes
      LS:=P2*FRAC(0.993133+  99.997361*T); -- mittl. Anomalie Sonne
      D :=P2*FRAC(0.827361+1236.853086*T); -- Diff. Laenge Mond-Sonne
      F :=P2*FRAC(0.259086+1342.227825*T); -- Knotenabstand
      DL := +22640.0*SIN(L) - 4586.0*SIN(L-2.0*D) + 2370.0*SIN(2.0*D) +  769.0*SIN(2.0*L)
            -668.0*SIN(LS) - 412.0*SIN(2.0*F) - 212.0*SIN(2.0*L-2.0*D) - 206.0*SIN(L+LS-2.0*D)
            +192.0*SIN(L+2.0*D) - 165.0*SIN(LS-2.0*D) - 125.0*SIN(D) - 110.0*SIN(L+LS)
            +148.0*SIN(L-LS) - 55.0*SIN(2.0*F-2.0*D);
      S := F + (DL+412.0*SIN(2.0*F)+541.0*SIN(LS)) / ARC;
      H := F-2.0*D;
      N := -526.0*SIN(H) + 44.0*SIN(L+H) - 31.0*SIN(-L+H) - 23.0*SIN(LS+H)
           + 11.0*SIN(-LS+H) -25.0*SIN(-2.0*L+F) + 21.0*SIN(-L+F);
      L_MOON := P2 * FRAC ( L0 + DL/1296.0E3 ); -- in rad
      B_MOON := ( 18520.0*SIN(S) + N ) / ARC; -- in rad
      -- aequatoriale Koordinaten
      CB:=COS(B_MOON);
      X:=CB*COS(L_MOON); V:=CB*SIN(L_MOON); W:=SIN(B_MOON);
      Y:=COSEPS*V-SINEPS*W; Z:=SINEPS*V+COSEPS*W; RHO:=SQRT(1.0-Z*Z);
      DEC := (360.0/P2)*ARCTAN(Z/RHO);
      RA  := ( 48.0/P2)*ARCTAN(Y/(X+RHO));
      if RA<0.0 then
        RA:=RA+24.0;
      end if;
    end MINI_MOON;

    ----------------------------------------------------------------------------
    --
    --MOON: analytische Mondtheorie nach E.W.Brown (Improved Lunar Ephemeris)
    --      mit einer Genauigkeit von ca. 1"
    --
    --      T: Zeit in julianischen Jahrhunderten seit J2000 (Ephemeridenzeit)
    --         (T=(JD-2451545.0)/36525.0)
    --      LAMBDA: geozentrische ekliptikale Laenge (Aequinoktium des Datums)
    --      BETA:   geozentrische ekliptikale Breite (Aequinoktium des Datums)
    --      R:      geozentrische Entfernung (in Erdradien)
    --
    ----------------------------------------------------------------------------
    procedure MOON (T            :     REAL;
                    LAMBDA,BETA,R: out REAL) is

      ARC : constant := 206264.81; -- 3600*180/pi = Bogensekunden pro radian

      DGAM,FACL          : REAL;
      DLAM,N,GAM1C,SINPI : REAL;
      L0, L, LS, F, D ,S : REAL;
      DL0,DL,DLS,DF,DD,DS: REAL;
      CO,SI: array(-6..6,1..4) of REAL;

      -- berechne c=cos(a1+a2) und s=sin(a1+a2) aus den Additionstheo-
      -- remen fuer c1=cos(a1), s1=sin(a1), c2=cos(a2) und s2=sin(a2)
      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S        : out REAL) is
      begin
        C:=C1*C2-S1*S2;
        S:=S1*C2+C1*S2;
      end ADDTHE;

      -- berechne sin(phi); phi in Einheiten von 1r=360 Grad
      function SINUS (PHI:REAL) return REAL is
      begin
        return SIN(P2*FRAC(PHI));
      end SINUS;

      -- berechne die langperiodischen Aenderungen der mittleren Elemente *)
      -- l,l',F,D und L0 sowie dgamma                                     *)
      procedure LONG_PERIODIC (T:                               REAL;
                               DL0L,DLL,DLSL,DFL,DDL,DGAML: out REAL) is
        S1,S2,S3,S4,S5,S6,S7: REAL;
      begin
        S1:=SINUS(0.19833+0.05611*T); S2:=SINUS(0.27869+0.04508*T);
        S3:=SINUS(0.16827-0.36903*T); S4:=SINUS(0.34734-5.37261*T);
        S5:=SINUS(0.10498-5.37899*T); S6:=SINUS(0.42681-0.41855*T);
        S7:=SINUS(0.14943-5.37511*T);
        DL0L:= 0.84*S1+0.31*S2+14.27*S3+ 7.26*S4+ 0.28*S5+0.24*S6;
        DLL := 2.94*S1+0.31*S2+14.27*S3+ 9.34*S4+ 1.12*S5+0.83*S6;
        DLSL:=-6.40*S1                                   -1.89*S6;
        DFL := 0.21*S1+0.31*S2+14.27*S3-88.70*S4-15.30*S5+0.24*S6-1.86*S7;
        DDL := DL0L-DLSL;
        DGAML  := -3332.0E-9 * SINUS(0.59734-5.37261*T)
                  -539.0E-9 * SINUS(0.35498-5.37899*T)
                   -64.0E-9 * SINUS(0.39943-5.37511*T);
      end LONG_PERIODIC;

      -- INIT: berechne die mittleren Elemente und deren sin und cos
      --   l Anomalie des Mondes            l' Anomalie der Sonne
      --   F Abstand des Mondes vom Knoten  D  Elongation des Mondes
      procedure INIT is
        MAX       : Integer;
        T2,ARG,FAC: REAL;
      begin
        T2:=T*T;
        DLAM :=0.0; DS:=0.0; GAM1C:=0.0; SINPI:=3422.7000;
        LONG_PERIODIC ( T, DL0,DL,DLS,DF,DD,DGAM );
        L0 := P2*FRAC(0.60643382+1336.85522467*T-0.00000313*T2) + DL0/ARC;
        L  := P2*FRAC(0.37489701+1325.55240982*T+0.00002565*T2) + DL /ARC;
        LS := P2*FRAC(0.99312619+  99.99735956*T-0.00000044*T2) + DLS/ARC;
        F  := P2*FRAC(0.25909118+1342.22782980*T-0.00000892*T2) + DF /ARC;
        D  := P2*FRAC(0.82736186+1236.85308708*T-0.00000397*T2) + DD /ARC;
        for I in 1 .. 4 loop
          case I is
          when 1 => ARG:=L;  MAX:=4; FAC:=1.000002208;
          when 2 => ARG:=LS; MAX:=3; FAC:=0.997504612-0.002495388*T;
          when 3 => ARG:=F;  MAX:=4; FAC:=1.000002708+139.978*DGAM;
          when 4 => ARG:=D;  MAX:=6; FAC:=1.0;
          end case;
          CO(0,I):=1.0; CO(1,I):=COS(ARG)*FAC;
          SI(0,I):=0.0; SI(1,I):=SIN(ARG)*FAC;
          for J in 2 .. MAX loop
            ADDTHE(CO(J-1,I),SI(J-1,I),CO(1,I),SI(1,I),CO(J,I),SI(J,I));
          end loop;
          for J in 1 .. MAX loop
            CO(-J,I):=CO(J,I); SI(-J,I):=-SI(J,I);
          end loop;
        end loop;
      end INIT;

      -- TERM berechne X=cos(p*arg1+q*arg2+r*arg3+s*arg4) und
      -- Y=sin(p*arg1+q*arg2+r*arg3+s*arg4)
      procedure TERM(P,Q,R,S:    Integer;
                     X,Y:    out REAL) is
        I: array (1..4) of Integer;
      begin
        I(1):=P; I(2):=Q; I(3):=R; I(4):=S;  X:=1.0; Y:=0.0;
        for K in 1 .. 4 loop
          if I(K)/=0 then
            ADDTHE(X,Y,CO(I(K),K),SI(I(K),K),X,Y);
          end if;
        end loop;
      end TERM;

      procedure ADDSOL(COEFFL,COEFFS,COEFFG,COEFFP: REAL;
                       P,Q,R,S:                     Integer) is
        X,Y: REAL;
      begin
        TERM(P,Q,R,S,X,Y);
        DLAM :=DLAM +COEFFL*Y; DS   :=DS   +COEFFS*Y;
        GAM1C:=GAM1C+COEFFG*X; SINPI:=SINPI+COEFFP*X;
      end ADDSOL;

      procedure SOLAR1 is
      begin
        ADDSOL(   13.902,   14.06,-0.001,   0.2607,0, 0, 0, 4);
        ADDSOL(    0.403,   -4.01,+0.394,   0.0023,0, 0, 0, 3);
        ADDSOL( 2369.912, 2373.36,+0.601,  28.2333,0, 0, 0, 2);
        ADDSOL( -125.154, -112.79,-0.725,  -0.9781,0, 0, 0, 1);
        ADDSOL(    1.979,    6.98,-0.445,   0.0433,1, 0, 0, 4);
        ADDSOL(  191.953,  192.72,+0.029,   3.0861,1, 0, 0, 2);
        ADDSOL(   -8.466,  -13.51,+0.455,  -0.1093,1, 0, 0, 1);
        ADDSOL(22639.500,22609.07,+0.079, 186.5398,1, 0, 0, 0);
        ADDSOL(   18.609,    3.59,-0.094,   0.0118,1, 0, 0,-1);
        ADDSOL(-4586.465,-4578.13,-0.077,  34.3117,1, 0, 0,-2);
        ADDSOL(   +3.215,    5.44,+0.192,  -0.0386,1, 0, 0,-3);
        ADDSOL(  -38.428,  -38.64,+0.001,   0.6008,1, 0, 0,-4);
        ADDSOL(   -0.393,   -1.43,-0.092,   0.0086,1, 0, 0,-6);
        ADDSOL(   -0.289,   -1.59,+0.123,  -0.0053,0, 1, 0, 4);
        ADDSOL(  -24.420,  -25.10,+0.040,  -0.3000,0, 1, 0, 2);
        ADDSOL(   18.023,   17.93,+0.007,   0.1494,0, 1, 0, 1);
        ADDSOL( -668.146, -126.98,-1.302,  -0.3997,0, 1, 0, 0);
        ADDSOL(    0.560,    0.32,-0.001,  -0.0037,0, 1, 0,-1);
        ADDSOL( -165.145, -165.06,+0.054,   1.9178,0, 1, 0,-2);
        ADDSOL(   -1.877,   -6.46,-0.416,   0.0339,0, 1, 0,-4);
        ADDSOL(    0.213,    1.02,-0.074,   0.0054,2, 0, 0, 4);
        ADDSOL(   14.387,   14.78,-0.017,   0.2833,2, 0, 0, 2);
        ADDSOL(   -0.586,   -1.20,+0.054,  -0.0100,2, 0, 0, 1);
        ADDSOL(  769.016,  767.96,+0.107,  10.1657,2, 0, 0, 0);
        ADDSOL(   +1.750,    2.01,-0.018,   0.0155,2, 0, 0,-1);
        ADDSOL( -211.656, -152.53,+5.679,  -0.3039,2, 0, 0,-2);
        ADDSOL(   +1.225,    0.91,-0.030,  -0.0088,2, 0, 0,-3);
        ADDSOL(  -30.773,  -34.07,-0.308,   0.3722,2, 0, 0,-4);
        ADDSOL(   -0.570,   -1.40,-0.074,   0.0109,2, 0, 0,-6);
        ADDSOL(   -2.921,  -11.75,+0.787,  -0.0484,1, 1, 0, 2);
        ADDSOL(   +1.267,    1.52,-0.022,   0.0164,1, 1, 0, 1);
        ADDSOL( -109.673, -115.18,+0.461,  -0.9490,1, 1, 0, 0);
        ADDSOL( -205.962, -182.36,+2.056,  +1.4437,1, 1, 0,-2);
        ADDSOL(    0.233,    0.36, 0.012,  -0.0025,1, 1, 0,-3);
        ADDSOL(   -4.391,   -9.66,-0.471,   0.0673,1, 1, 0,-4);
      end SOLAR1;

      procedure SOLAR2 is
      begin
        ADDSOL(    0.283,    1.53,-0.111,  +0.0060,1,-1, 0,+4);
        ADDSOL(   14.577,   31.70,-1.540,  +0.2302,1,-1, 0, 2);
        ADDSOL(  147.687,  138.76,+0.679,  +1.1528,1,-1, 0, 0);
        ADDSOL(   -1.089,    0.55,+0.021,   0.0   ,1,-1, 0,-1);
        ADDSOL(   28.475,   23.59,-0.443,  -0.2257,1,-1, 0,-2);
        ADDSOL(   -0.276,   -0.38,-0.006,  -0.0036,1,-1, 0,-3);
        ADDSOL(    0.636,    2.27,+0.146,  -0.0102,1,-1, 0,-4);
        ADDSOL(   -0.189,   -1.68,+0.131,  -0.0028,0, 2, 0, 2);
        ADDSOL(   -7.486,   -0.66,-0.037,  -0.0086,0, 2, 0, 0);
        ADDSOL(   -8.096,  -16.35,-0.740,   0.0918,0, 2, 0,-2);
        ADDSOL(   -5.741,   -0.04, 0.0  ,  -0.0009,0, 0, 2, 2);
        ADDSOL(    0.255,    0.0 , 0.0  ,   0.0   ,0, 0, 2, 1);
        ADDSOL( -411.608,   -0.20, 0.0  ,  -0.0124,0, 0, 2, 0);
        ADDSOL(    0.584,    0.84, 0.0  ,  +0.0071,0, 0, 2,-1);
        ADDSOL(  -55.173,  -52.14, 0.0  ,  -0.1052,0, 0, 2,-2);
        ADDSOL(    0.254,    0.25, 0.0  ,  -0.0017,0, 0, 2,-3);
        ADDSOL(   +0.025,   -1.67, 0.0  ,  +0.0031,0, 0, 2,-4);
        ADDSOL(    1.060,    2.96,-0.166,   0.0243,3, 0, 0,+2);
        ADDSOL(   36.124,   50.64,-1.300,   0.6215,3, 0, 0, 0);
        ADDSOL(  -13.193,  -16.40,+0.258,  -0.1187,3, 0, 0,-2);
        ADDSOL(   -1.187,   -0.74,+0.042,   0.0074,3, 0, 0,-4);
        ADDSOL(   -0.293,   -0.31,-0.002,   0.0046,3, 0, 0,-6);
        ADDSOL(   -0.290,   -1.45,+0.116,  -0.0051,2, 1, 0, 2);
        ADDSOL(   -7.649,  -10.56,+0.259,  -0.1038,2, 1, 0, 0);
        ADDSOL(   -8.627,   -7.59,+0.078,  -0.0192,2, 1, 0,-2);
        ADDSOL(   -2.740,   -2.54,+0.022,   0.0324,2, 1, 0,-4);
        ADDSOL(    1.181,    3.32,-0.212,   0.0213,2,-1, 0,+2);
        ADDSOL(    9.703,   11.67,-0.151,   0.1268,2,-1, 0, 0);
        ADDSOL(   -0.352,   -0.37,+0.001,  -0.0028,2,-1, 0,-1);
        ADDSOL(   -2.494,   -1.17,-0.003,  -0.0017,2,-1, 0,-2);
        ADDSOL(    0.360,    0.20,-0.012,  -0.0043,2,-1, 0,-4);
        ADDSOL(   -1.167,   -1.25,+0.008,  -0.0106,1, 2, 0, 0);
        ADDSOL(   -7.412,   -6.12,+0.117,   0.0484,1, 2, 0,-2);
        ADDSOL(   -0.311,   -0.65,-0.032,   0.0044,1, 2, 0,-4);
        ADDSOL(   +0.757,    1.82,-0.105,   0.0112,1,-2, 0, 2);
        ADDSOL(   +2.580,    2.32,+0.027,   0.0196,1,-2, 0, 0);
        ADDSOL(   +2.533,    2.40,-0.014,  -0.0212,1,-2, 0,-2);
        ADDSOL(   -0.344,   -0.57,-0.025,  +0.0036,0, 3, 0,-2);
        ADDSOL(   -0.992,   -0.02, 0.0  ,   0.0   ,1, 0, 2, 2);
        ADDSOL(  -45.099,   -0.02, 0.0  ,  -0.0010,1, 0, 2, 0);
        ADDSOL(   -0.179,   -9.52, 0.0  ,  -0.0833,1, 0, 2,-2);
        ADDSOL(   -0.301,   -0.33, 0.0  ,   0.0014,1, 0, 2,-4);
        ADDSOL(   -6.382,   -3.37, 0.0  ,  -0.0481,1, 0,-2, 2);
        ADDSOL(   39.528,   85.13, 0.0  ,  -0.7136,1, 0,-2, 0);
        ADDSOL(    9.366,    0.71, 0.0  ,  -0.0112,1, 0,-2,-2);
        ADDSOL(    0.202,    0.02, 0.0  ,   0.0   ,1, 0,-2,-4);
      end SOLAR2;

      procedure SOLAR3 is
      begin
        ADDSOL(    0.415,    0.10, 0.0  ,  0.0013,0, 1, 2, 0);
        ADDSOL(   -2.152,   -2.26, 0.0  , -0.0066,0, 1, 2,-2);
        ADDSOL(   -1.440,   -1.30, 0.0  , +0.0014,0, 1,-2, 2);
        ADDSOL(    0.384,   -0.04, 0.0  ,  0.0   ,0, 1,-2,-2);
        ADDSOL(   +1.938,   +3.60,-0.145, +0.0401,4, 0, 0, 0);
        ADDSOL(   -0.952,   -1.58,+0.052, -0.0130,4, 0, 0,-2);
        ADDSOL(   -0.551,   -0.94,+0.032, -0.0097,3, 1, 0, 0);
        ADDSOL(   -0.482,   -0.57,+0.005, -0.0045,3, 1, 0,-2);
        ADDSOL(    0.681,    0.96,-0.026,  0.0115,3,-1, 0, 0);
        ADDSOL(   -0.297,   -0.27, 0.002, -0.0009,2, 2, 0,-2);
        ADDSOL(    0.254,   +0.21,-0.003,  0.0   ,2,-2, 0,-2);
        ADDSOL(   -0.250,   -0.22, 0.004,  0.0014,1, 3, 0,-2);
        ADDSOL(   -3.996,    0.0 , 0.0  , +0.0004,2, 0, 2, 0);
        ADDSOL(    0.557,   -0.75, 0.0  , -0.0090,2, 0, 2,-2);
        ADDSOL(   -0.459,   -0.38, 0.0  , -0.0053,2, 0,-2, 2);
        ADDSOL(   -1.298,    0.74, 0.0  , +0.0004,2, 0,-2, 0);
        ADDSOL(    0.538,    1.14, 0.0  , -0.0141,2, 0,-2,-2);
        ADDSOL(    0.263,    0.02, 0.0  ,  0.0   ,1, 1, 2, 0);
        ADDSOL(    0.426,   +0.07, 0.0  , -0.0006,1, 1,-2,-2);
        ADDSOL(   -0.304,   +0.03, 0.0  , +0.0003,1,-1, 2, 0);
        ADDSOL(   -0.372,   -0.19, 0.0  , -0.0027,1,-1,-2, 2);
        ADDSOL(   +0.418,    0.0 , 0.0  ,  0.0   ,0, 0, 4, 0);
        ADDSOL(   -0.330,   -0.04, 0.0  ,  0.0   ,3, 0, 2, 0);
      end SOLAR3;

      -- Stoerungsanteil N der ekliptikalen Breite
      procedure SOLARN(N: out REAL) is
        X,Y: REAL;
        procedure ADDN(COEFFN:REAL;P,Q,R,S:Integer) is
        begin
          TERM(P,Q,R,S,X,Y); N:=N+COEFFN*Y;
        end ADDN;
      begin -- SOLARN
        N := 0.0;
        ADDN(-526.069, 0, 0,1,-2); ADDN(  -3.352, 0, 0,1,-4);
        ADDN( +44.297,+1, 0,1,-2); ADDN(  -6.000,+1, 0,1,-4);
        ADDN( +20.599,-1, 0,1, 0); ADDN( -30.598,-1, 0,1,-2);
        ADDN( -24.649,-2, 0,1, 0); ADDN(  -2.000,-2, 0,1,-2);
        ADDN( -22.571, 0,+1,1,-2); ADDN( +10.985, 0,-1,1,-2);
      end SOLARN;

      -- Stoerungen der ekliptikalen Laenge durch Venus und Jupiter
      procedure PLANETARY(DLAML: in out REAL) is
      begin
        DLAML  := DLAML
          +0.82*SINUS(0.7736  -62.5512*T)+0.31*SINUS(0.0466 -125.1025*T)
          +0.35*SINUS(0.5785  -25.1042*T)+0.66*SINUS(0.4591+1335.8075*T)
          +0.64*SINUS(0.3130  -91.5680*T)+1.14*SINUS(0.1480+1331.2898*T)
          +0.21*SINUS(0.5918+1056.5859*T)+0.44*SINUS(0.5784+1322.8595*T)
          +0.24*SINUS(0.2275   -5.7374*T)+0.28*SINUS(0.2965   +2.6929*T)
          +0.33*SINUS(0.3132   +6.3368*T);
      end PLANETARY;

    begin -- MOON
      INIT;  SOLAR1; SOLAR2; SOLAR3; SOLARN(N);  PLANETARY(DLAM);
      LAMBDA := 360.0*FRAC( (L0+DLAM/ARC) / P2 );
      S    := F + DS/ARC;
      FACL  := 1.000002708+139.978*DGAM;
      BETA := ( FACL*(18518.511+1.189+GAM1C)*SIN(S)-6.24*SIN(3.0*S)+N ) / 3600.0;
      SINPI := SINPI * 0.999953253;
      R     := ARC / SINPI;
    end MOON;

    -----------------------------------------------------------------------------
    -- MOONEQU: aequatoriale Mondkoordinaten
    --          (Rektaszension RA und Deklination DEC in Grad, R in Erdradien)
    --          T in julian.Jahrhndt. seit J2000 ( T:= (JD - 2451545.0)/36525 )
    --          Die Koord. beziehen sich auf das wahre Aequinoktium des Datums.
    -----------------------------------------------------------------------------
    procedure MOONEQU(T:            REAL;
                      RA,DEC,R: out REAL) is
      L,B,X,Y,Z: REAL;
    begin
      MOON(T,L,B,R);         -- ekliptikale Moondkoordinaten
      CART(R,B,L,X,Y,Z);     -- (mittleres Aequinoktium des Datums)
      ECLEQU(T,X,Y,Z);       -- Umwandlung in aequatoriale Koordinaten
      NUTEQU(T,X,Y,Z);       -- Nutation
      POLAR(X,Y,Z,R,DEC,RA);
    end MOONEQU;

    ------------------------------------------------------------------------------
    -- T_FIT_MOON: Berechnet die Tschebyscheff-Entwicklung der
    --             Koordinaten des Mondes (Reihen fuer RA,DEC und Radius).
    --
    --       TA      : Beginn des Entwicklungsintervalls (jul.Jahrh. seit J2000)
    --       TB      : Ende des Entwicklungsintervalls ( TB < TA + 1 Monat )
    --       N       : Ordnung der Entwicklung
    --       RA_POLY,DE_POLY,R_POLY: Tschebyscheff Polynome fuer RA,DEC,R
    ------------------------------------------------------------------------------
    procedure T_FIT_MOON (TA,TB:                      REAL;
                          N:                          Integer;
                          RA_POLY,DE_POLY,R_POLY: out TPOLYNOM) is
    begin
      T_FIT_LBR (MOONEQU'access,TA,TB,N,RA_POLY,DE_POLY,R_POLY);
    end T_FIT_MOON;

  end MOOLIB;


  --==========================
  -- Unit SUNLIB: Sonnenbahn
  --==========================

  package body SUNLIB is

    ---------------------------------------------------------------------------
    -- MINI_SUN: Sonnenkoordinaten geringer Genauigkeit (ca.1')
    --           T  : Zeit in jul.Jahrh. seit J2000  ( T=(JD-2451545)/36525 )
    --           RA : Rektaszension (in h)
    --           DEC: Deklination (in Grad)
    --           (Aequinoktium des Datums)
    ---------------------------------------------------------------------------
    procedure MINI_SUN (T     :     REAL;
                        RA,DEC: out REAL) is
      COSEPS: constant := 0.91748;
      SINEPS: constant := 0.39778;
      L,M,DL,SL,X,Y,Z,RHO: REAL;
    begin
      M  := P2*FRAC(0.993133+99.997361*T);
      DL := 6893.0*SIN(M)+72.0*SIN(2.0*M);
      L  := P2*FRAC(0.7859453 + M/P2 + (6191.2*T+DL)/1296.0E3);
      SL := SIN(L);
      X:=COS(L); Y:=COSEPS*SL; Z:=SINEPS*SL; RHO:=SQRT(1.0-Z*Z);
      DEC := (360.0/P2)*ARCTAN(Z/RHO);
      RA  := ( 48.0/P2)*ARCTAN(Y/(X+RHO));
      if (RA<0.0) then
        RA:=RA+24.0;
      end if;
    end MINI_SUN;

    ------------------------------------------------------------------
    -- SUN200: Sonne; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    ------------------------------------------------------------------
    procedure SUN200 (T    :     REAL;
                      L,B,R: out REAL) is

      C3,S3:          array (-1..7) of REAL;
      C,S:            array (-8..0) of REAL;
      M2,M3,M4,M5,M6: REAL;
      D,A,UU:         REAL;
      U,V,DL,DR,DB:   REAL;

      procedure ADDTHE (C1,S1,C2,S2:     REAL;
                        C,S        : out REAL) is
      begin
        C:=C1*C2-S1*S2;
        S:=S1*C2+C1*S2;
      end ADDTHE;

      procedure TERM (I1,I,IT                : Integer;
                      DLC,DLS,DRC,DRS,DBC,DBS: REAL) is
      begin
        if IT=0 then
          ADDTHE(C3(I1),S3(I1),C(I),S(I),U,V);
        else
          U:=U*T; V:=V*T;
        end if;
        DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
      end TERM;


      procedure PERTVEN is -- Keplerterme und Stoerungen durch Venus
      begin
        C(0):=1.0; S(0):=0.0; C(-1):=COS(M2); S(-1):=-SIN(M2);
        for I in reverse -5 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(1, 0,0,-0.22,6892.76,-16707.37, -0.54, 0.00, 0.00);
        TERM(1, 0,1,-0.06, -17.35,    42.04, -0.15, 0.00, 0.00);
        TERM(1, 0,2,-0.01,  -0.05,     0.13, -0.02, 0.00, 0.00);
        TERM(2, 0,0, 0.00,  71.98,  -139.57,  0.00, 0.00, 0.00);
        TERM(2, 0,1, 0.00,  -0.36,     0.70,  0.00, 0.00, 0.00);
        TERM(3, 0,0, 0.00,   1.04,    -1.75,  0.00, 0.00, 0.00);
        TERM(0,-1,0, 0.03,  -0.07,    -0.16, -0.07, 0.02,-0.02);
        TERM(1,-1,0, 2.35,  -4.23,    -4.75, -2.64, 0.00, 0.00);
        TERM(1,-2,0,-0.10,   0.06,     0.12,  0.20, 0.02, 0.00);
        TERM(2,-1,0,-0.06,  -0.03,     0.20, -0.01, 0.01,-0.09);
        TERM(2,-2,0,-4.70,   2.90,     8.28, 13.42, 0.01,-0.01);
        TERM(3,-2,0, 1.80,  -1.74,    -1.44, -1.57, 0.04,-0.06);
        TERM(3,-3,0,-0.67,   0.03,     0.11,  2.43, 0.01, 0.00);
        TERM(4,-2,0, 0.03,  -0.03,     0.10,  0.09, 0.01,-0.01);
        TERM(4,-3,0, 1.51,  -0.40,    -0.88, -3.36, 0.18,-0.10);
        TERM(4,-4,0,-0.19,  -0.09,    -0.38,  0.77, 0.00, 0.00);
        TERM(5,-3,0, 0.76,  -0.68,     0.30,  0.37, 0.01, 0.00);
        TERM(5,-4,0,-0.14,  -0.04,    -0.11,  0.43,-0.03, 0.00);
        TERM(5,-5,0,-0.05,  -0.07,    -0.31,  0.21, 0.00, 0.00);
        TERM(6,-4,0, 0.15,  -0.04,    -0.06, -0.21, 0.01, 0.00);
        TERM(6,-5,0,-0.03,  -0.03,    -0.09,  0.09,-0.01, 0.00);
        TERM(6,-6,0, 0.00,  -0.04,    -0.18,  0.02, 0.00, 0.00);
        TERM(7,-5,0,-0.12,  -0.03,    -0.08,  0.31,-0.02,-0.01);
      end PERTVEN;

      procedure PERTMAR is -- Stoerungen durch Mars
      begin
        C(-1):=COS(M4);
        S(-1):=-SIN(M4);
        for I in reverse -7 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(1,-1,0,-0.22,   0.17,    -0.21, -0.27, 0.00, 0.00);
        TERM(1,-2,0,-1.66,   0.62,     0.16,  0.28, 0.00, 0.00);
        TERM(2,-2,0, 1.96,   0.57,    -1.32,  4.55, 0.00, 0.01);
        TERM(2,-3,0, 0.40,   0.15,    -0.17,  0.46, 0.00, 0.00);
        TERM(2,-4,0, 0.53,   0.26,     0.09, -0.22, 0.00, 0.00);
        TERM(3,-3,0, 0.05,   0.12,    -0.35,  0.15, 0.00, 0.00);
        TERM(3,-4,0,-0.13,  -0.48,     1.06, -0.29, 0.01, 0.00);
        TERM(3,-5,0,-0.04,  -0.20,     0.20, -0.04, 0.00, 0.00);
        TERM(4,-4,0, 0.00,  -0.03,     0.10,  0.04, 0.00, 0.00);
        TERM(4,-5,0, 0.05,  -0.07,     0.20,  0.14, 0.00, 0.00);
        TERM(4,-6,0,-0.10,   0.11,    -0.23, -0.22, 0.00, 0.00);
        TERM(5,-7,0,-0.05,   0.00,     0.01, -0.14, 0.00, 0.00);
        TERM(5,-8,0, 0.05,   0.01,    -0.02,  0.10, 0.00, 0.00);
      end PERTMAR;

      procedure PERTJUP is -- Stoerungen durch Jupiter
      begin
        C(-1):=COS(M5);
        S(-1):=-SIN(M5);
        for I in reverse -3 .. -1 loop
          ADDTHE(C(I),S(I),C(-1),S(-1),C(I-1),S(I-1));
        end loop;
        TERM(-1,-1,0,0.01,   0.07,     0.18, -0.02, 0.00,-0.02);
        TERM(0,-1,0,-0.31,   2.58,     0.52,  0.34, 0.02, 0.00);
        TERM(1,-1,0,-7.21,  -0.06,     0.13,-16.27, 0.00,-0.02);
        TERM(1,-2,0,-0.54,  -1.52,     3.09, -1.12, 0.01,-0.17);
        TERM(1,-3,0,-0.03,  -0.21,     0.38, -0.06, 0.00,-0.02);
        TERM(2,-1,0,-0.16,   0.05,    -0.18, -0.31, 0.01, 0.00);
        TERM(2,-2,0, 0.14,  -2.73,     9.23,  0.48, 0.00, 0.00);
        TERM(2,-3,0, 0.07,  -0.55,     1.83,  0.25, 0.01, 0.00);
        TERM(2,-4,0, 0.02,  -0.08,     0.25,  0.06, 0.00, 0.00);
        TERM(3,-2,0, 0.01,  -0.07,     0.16,  0.04, 0.00, 0.00);
        TERM(3,-3,0,-0.16,  -0.03,     0.08, -0.64, 0.00, 0.00);
        TERM(3,-4,0,-0.04,  -0.01,     0.03, -0.17, 0.00, 0.00);
      end PERTJUP;

      procedure PERTSAT is -- Stoerungen durch Saturn
      begin
        C(-1):=COS(M6);
        S(-1):=-SIN(M6);
        ADDTHE(C(-1),S(-1),C(-1),S(-1),C(-2),S(-2));
        TERM(0,-1,0, 0.00,   0.32,     0.01,  0.00, 0.00, 0.00);
        TERM(1,-1,0,-0.08,  -0.41,     0.97, -0.18, 0.00,-0.01);
        TERM(1,-2,0, 0.04,   0.10,    -0.23,  0.10, 0.00, 0.00);
        TERM(2,-2,0, 0.04,   0.10,    -0.35,  0.13, 0.00, 0.00);
      end PERTSAT;

      procedure PERTMOO is -- Differenz Erde-Mond-Schwerpunkt zu Erdmittelpunkt
      begin
        DL := DL +  6.45*SIN(D) - 0.42*SIN(D-A) + 0.18*SIN(D+A)
                                + 0.17*SIN(D-M3) - 0.06*SIN(D+M3);
        DR := DR + 30.76*COS(D) - 3.06*COS(D-A)+ 0.85*COS(D+A)
                                - 0.58*COS(D+M3) + 0.57*COS(D-M3);
        DB := DB + 0.576*SIN(UU);
      end PERTMOO;

    begin -- SUN200
      DL:=0.0; DR:=0.0; DB:=0.0;
      M2:=P2*FRAC(0.1387306+162.5485917*T); M3:=P2*FRAC(0.9931266+99.9973604*T);
      M4:=P2*FRAC(0.0543250+ 53.1666028*T); M5:=P2*FRAC(0.0551750+ 8.4293972*T);
      M6:=P2*FRAC(0.8816500+  3.3938722*T); D :=P2*FRAC(0.8274+1236.8531*T);
      A :=P2*FRAC(0.3749+1325.5524*T);      UU:=P2*FRAC(0.2591+1342.2278*T);
      C3(0):=1.0;     S3(0):=0.0;
      C3(1):=COS(M3); S3(1):=SIN(M3);  C3(-1):=C3(1); S3(-1):=-S3(1);
      for I in 2 .. 7 loop
        ADDTHE(C3(I-1),S3(I-1),C3(1),S3(1),C3(I),S3(I));
      end loop;
      PERTVEN; PERTMAR; PERTJUP; PERTSAT; PERTMOO;
      DL:=DL + 6.40*SIN(P2*(0.6983+0.0561*T)) + 1.87*SIN(P2*(0.5764+0.4174*T))
             + 0.27*SIN(P2*(0.4189+0.3306*T)) + 0.20*SIN(P2*(0.3581+2.4814*T));
      L:= 360.0*FRAC(0.7859453 + M3/P2 + ((6191.2+1.1*T)*T+DL)/1296.0E3 );
      R:= 1.0001398 - 0.0000007*T  +  DR*1.0E-6;
      B:= DB/3600.0;
    end SUN200;

    ----------------------------------------------------------------------------
    -- SUNEQU: aequatoriale Sonnenkoordinaten
    --         (Rektaszension RA und Deklination DEC in Grad, R in AE)
    --         T in julian.Jahrhndt. seit J2000 ( T:= (JD - 2451545.0)/36525 )
    --         Die Koord. beziehen sich auf das wahre Aequinoktium des Datums.
    ----------------------------------------------------------------------------
    procedure SUNEQU(T       :     REAL;
                     RA,DEC,R: out REAL) is
      DT,L,B,X,Y,Z: REAL;
    begin
      DT := (8.32/1440.0)/36525.0; -- Retardierung um 8.32 Minuten
      SUN200(T-DT,L,B,R);          -- geozentrische ekliptikale
      CART(R,B,L,X,Y,Z);           -- Sonnenkoordinaten
      ECLEQU(T,X,Y,Z);             -- aequatoriale Koordinaten
      NUTEQU(T,X,Y,Z);             -- Nutationskorrektur
      POLAR(X,Y,Z,R,DEC,RA);
    end SUNEQU;

    ------------------------------------------------------------------------------
    -- T_FIT_SUN: Berechnet die Tschebyscheff-Entwicklung der
    --            Koordinaten der Sonne (Reihen fuer RA,DEC und Radius).
    --
    --       TA      : Beginn des Entwicklungsintervalls (jul.Jahrh. seit J2000)
    --       TB      : Ende des Entwicklungsintervalls ( TB < TA + 1 Monat )
    --       N       : Ordnung der Entwicklung
    --       RA_POLY,DE_POLY,R_POLY: Tschebyscheff Polynome fuer RA,DEC,R
    ------------------------------------------------------------------------------
    procedure T_FIT_SUN (TA,TB:                      REAL;
                         N:                          Integer;
                         RA_POLY,DE_POLY,R_POLY: out TPOLYNOM) is
    begin
      T_FIT_LBR (SUNEQU'access,TA,TB,N,RA_POLY,DE_POLY,R_POLY);
    end T_FIT_SUN;

  end SUNLIB;


  --=====================================
  -- TIMLIB: Zeit- und Kalenderrechnung
  --=====================================
  package body TIMLIB is

    -----------------------------------------------------------
    -- CALDAT: Bestimmung des Kalenderdatums
    --         aus dem Modifizierten Julianischen Datum (MJD)
    -----------------------------------------------------------
    procedure CALDAT (MJDL           :     REAL;
                      DAY,MONTH,YEAR: out Natural;
                      HOUR          : out HOURS) is
     B,D,F     : Integer;
     JD,JD0,C,E: REAL;
    begin
      JD  := MJDL + 2400000.5;
      JD0 := REAL(TRUNC(JD+0.5));
      if (JD0<2299161.0) then -- Kalender:
        C:=JD0+1524.0;        -- -> julianisch
      else                    -- -> gregorianisch
        B:=TRUNC((JD0-1867216.25)/36524.25);
        C:=JD0+REAL((B-B/4))+1525.0;
      end if;
      D    := TRUNC((C-122.1)/365.25);                E     := REAL(365*D+(D/4));
      F    := TRUNC((C-E)/30.6001);
      DAY  := TRUNC(C-E+0.5)-TRUNC(30.6001*REAL(F));  MONTH := F-1-12*(F/14);
      YEAR := D-4715-((7+MONTH)/10);                  HOUR  := HOURS(24.0*(JD+0.5-JD0));
    end CALDAT;

    ----------------------------------------------------------------------------
    -- ETMINUT: Differenz Ephemeridenzeit - Weltzeit
    --          (Polynomdarstellung; gueltig von 2000-2050)
    --          T:     Zeit in jul.Jahrh. seit J2000 (=(JD-2451545.0)/36525.0)
    --          DTSEC: DT=ET-UT in sec (nur fuer VALID=TRUE)
    --          VALID: TRUE fuer Zeiten zwischen 2000 und 2050, sonst FALSE
    ----------------------------------------------------------------------------
    procedure ETMINUT (T    :     REAL;
                       DTSEC: out REAL;
                       VALID: out Boolean) is

    begin
      VALID := ( (0.0<=T) and (T<=0.5) );
      if (VALID) then
        DTSEC := 62.92 + T * (32.217 + T * 55.89);
      end if;
    end ETMINUT;

    ------------------------------------------------------------
    -- LMST: mittlere Ortssternzeit (local mean sidereal time)
    ------------------------------------------------------------
    function LMST (MJDL,LAMBDA:REAL) return HOURS is
      MJD0,T,UT,GMST: REAL;
    begin
      MJD0:=REAL(TRUNC(MJDL));
      UT:=(MJDL-MJD0)*24.0; T:=(MJD0-51544.5)/36525.0;
      GMST:=6.697374558 + 1.0027379093*UT
              +(8640184.812866+(0.093104-6.2E-6*T)*T)*T/3600.0;
      return HOURS(24.0*FRAC((GMST-LAMBDA/15.0)/24.0));
    end LMST;

    ----------------------------------------------------------
    -- MJD: Modifiziertes Julianisches Datum
    --      gueltig fuer jedes Datum seit 4713 v.Chr.
    --      julianischer Kalender bis zum 4. Oktober 1582
    --      gregorianischer Kalender ab dem 15. Oktober 1582
    ----------------------------------------------------------
    function MJD (DAY,MONTH,YEAR:Natural;
                  HOUR          :HOURS) return REAL is
      A: Integer;
      B: Integer;
      M: Natural := MONTH;
      Y: Natural := YEAR;
    begin
      A:=Y*10000+M*100+DAY;
      if M<=2 then
        M:=M+12; Y:=Y-1;
      end if;
      if A<=15821004 then
        B:=-2+((Y+4716)/4)-1179;
      else
        B:=(Y/400)-(Y/100)+(Y/4);
      end if;
      A:=Y*365-679004;
      return REAL(A+B+TRUNC(REAL(M+1)*30.6001)+DAY)+REAL(HOUR/24.0);
    end MJD;

  end TIMLIB;

end Astro;
