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

package Astro is

  type REAL is new Long_Float;

  Earth_Equatorial_Radius : constant REAL := 6378.135; -- kilometers (WGS '72)

  -- Vektoren und Matrizen

  type INDEX  is (X,Y,Z);
  type VECTOR is array (INDEX) of REAL;
  type MAT3X  is array (1..3)  of VECTOR;

  type REAL3  is array (1..3)  of REAL;
  type REAL33 is array (1..3, 1..3) of REAL;

  function "+" (Left, Right : VECTOR) return VECTOR;

  function "-" (Left, Right : VECTOR) return VECTOR;

  -- Tschebyscheff-Polynom

  MAX_TP_DEG : constant := 13; -- Entwicklungsgrad der T-Polynome

  type TKOEFF is array (0..MAX_TP_DEG) of REAL; -- Tschebyscheff-Koeffizienten

  type TPOLYNOM is record -- Tschebyscheff-Polynom
    M  : Integer;         -- Grad
    A,B: REAL;            -- Intervall
    C  : TKOEFF;          -- Koeffizienten *)
  end record;

  ----------------------
  -- SQRT: Square Root
  ----------------------
  function SQRT (X : REAL) return REAL;


  --=========
  -- MATLIB
  --=========
  package MATLIB is

    -- Ausgleichsmatrix und -vektor
    -- Minimale Dimensionierung fuer Programm FOTO; bei Verwendung fuer
    -- andere Programme koennen die Grenzen beliebig erhoeht werden.

    type LSQVEC is array (1..3) of REAL;
    type LSQMAT is array (1..30,1..5) of REAL;

    -------------------------------------------
    -- ACS: Arcus-Cosinus-Funktion (Gradmass)
    -------------------------------------------
    function ACS (X:REAL) return REAL;

    -----------------------------------------
    -- ASN: Arcus-Sinus-Funktion (Gradmass)
    -----------------------------------------
    function ASN (X:REAL) return REAL;

    -------------------------------------------
    -- ATN: Arcus-Tangens-Funktion (Gradmass)
    -------------------------------------------
    function ATN (X:REAL) return REAL;

    -----------------------------------------------------------------
    -- ATN2: Arcus-Tangens von y/x fuer zwei Argumente
    --       (quadrantenrichtig mit -180 Grad <= ATN2 <= +180 Grad)
    -----------------------------------------------------------------
    function ATN2 (Y,X:REAL) return REAL;

    -----------------------------------------------------------------------
    -- CART: Umwandlung von Polarkoordinaten (r,theta,phi)
    --       in kartesische Koordinaten (x,y,z)
    --       ( theta = [-90 Grad,+90 Grad]; phi = [-360 Grad,+360 Grad] )
    -----------------------------------------------------------------------
    procedure CART (R,THETA,PHI:     REAL;
                    X,Y,Z      : out REAL);

    function CART (R,THETA,PHI: REAL) return VECTOR;

    ----------------------------------------
    -- CROSS: Kreuzprodukt zweier Vektoren
    ----------------------------------------
    procedure CROSS (A,B:     VECTOR;
                     C  : out VECTOR);

    ------------------------------------
    -- CS: Cosinus-Funktion (Gradmass)
    ------------------------------------
    function CS (X:REAL) return REAL;

    ------------------------
    -- CUBR: dritte Wurzel
    ------------------------
    function CUBR (X:REAL) return REAL;

    ------------------------------------------------------------------------------
    -- DDD: Umwandlung von Grad, Minuten und Sekunden in Bruchteile eines Grades
    ------------------------------------------------------------------------------
    procedure DDD (D,M:     Integer;
                   S  :     REAL;
                   DD : out REAL);

    ----------------------------------------------------------------------------
    -- DMS: Umwandlung von Bruchteilen eines Grades in Grad, Minuten, Sekunden
    ----------------------------------------------------------------------------
    procedure DMS (DD :     REAL;
                   D,M: out Integer;
                   S  : out REAL);

    ----------------------------------------
    -- DOT: Skalarprodukt zweier Vektoren
    ----------------------------------------
    function DOT (A,B:VECTOR) return REAL;

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
                      S   : out LSQVEC );

    -------------------------------
    -- NORM: Betrag eines Vektors
    -------------------------------
    function NORM (A:VECTOR) return REAL;

    --------------------------------------------------------------------
    -- POLAR: Umwandlung von kartesischen Koordinaten (x,y,z)
    --        in Polarkoordinaten (r,theta,phi)
    --        ( theta = [-90 Grad,+90 Grad]; phi = [0 Grad,+360 Grad])
    --------------------------------------------------------------------
    procedure POLAR (X,Y,Z      :     REAL;
                     R,THETA,PHI: out REAL);

    procedure POLAR (V          :     VECTOR;
                     R,THETA,PHI: out REAL);

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
                    NZ                : out Integer);

    ----------------------------------
    -- SN: Sinus-Funktion (Gradmass)
    ----------------------------------
    function SN (X:REAL) return REAL;

    ------------------------------------------------------------------------------
    -- T_EVAL: Berechnet Funktionswerte fuer eine im Intervall [F.A,F.B] durch
    --         eine Tschebyscheff-Entwickl. vom Grad F.M approximierte Funktion.
    --   F: Tschebyscheff-Polynom
    --   X: Argument
    ------------------------------------------------------------------------------
    function T_EVAL (F: TPOLYNOM; X: REAL) return REAL;

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
                         L_POLY,B_POLY,R_POLY: out TPOLYNOM);

    ------------------------------------
    -- TN: Tangens-Funktion (Gradmass)
    ------------------------------------
    function TN (X:REAL) return REAL;

    ---------------------------------------
    -- CTN: Cotangens-Funktion (Gradmass)
    ---------------------------------------
    function CTN (X:REAL) return REAL;

  end MATLIB;


  --============================================
  -- Unit PLALIB: S
  --============================================
  package PLALIB is

    type PLANET is (Sun, Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto);

    type MODUS is (Geometric, Astrometric, Apparent);

    use MATLIB;

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
                      XP,YP,ZP, XS,YS,ZS, X,Y,Z,DELTA0: out REAL);

    -------------------------------------------------------------------
    -- MER200: Merkur; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure MER200 (T:         REAL;
                      L,B,R: out REAL);

    ------------------------------------------------------------------
    -- VEN200: Venus; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    ------------------------------------------------------------------
    procedure VEN200 (T:         REAL;
                      L,B,R: out REAL);

    -----------------------------------------------------------------
    -- MAR200: Mars; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -----------------------------------------------------------------
    procedure MAR200(T:         REAL;
                     L,B,R: out REAL);

    --------------------------------------------------------------------
    -- JUP200: Jupiter; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    --------------------------------------------------------------------
    procedure JUP200 (T:         REAL;
                      L,B,R: out REAL);

    -------------------------------------------------------------------
    -- SAT200: Saturn; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure SAT200 (T:         REAL;
                      L,B,R: out REAL);

    -------------------------------------------------------------------
    -- URA200: Uranus; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure URA200 (T:         REAL;
                      L,B,R: out REAL);

    -------------------------------------------------------------------
    -- NEP200: Neptun; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    -------------------------------------------------------------------
    procedure NEP200 (T:         REAL;
                      L,B,R: out REAL);

    ----------------------------------------------------------------------
    -- PLU200: Pluto; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums; nur gueltig von ca. 1890-2100 !!
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    ----------------------------------------------------------------------
    procedure PLU200 (T:         REAL;
                      L,B,R: out REAL);

   end PLALIB;


  --========================================
  -- Unit PNULIB: Praezession und Nutation
  --========================================
  package PNULIB is

    use MATLIB;

    --------------------------------------------------------------
    -- NUTEQU: Transformation von mittleren in wahre aequatoriale
    --         Koordinaten (Terme >0.1" nach IAU 1980)
    --         T = (JD-2451545.0)/36525.0
    --------------------------------------------------------------
    procedure NUTEQU (T    :        REAL;
                      X,Y,Z: in out REAL);

    -----------------------------------------------------------------
    -- PMATECL: Berechnung der Praezessionsmatrix A[i,j] fuer
    --          ekliptikale Koordinaten vom Aequinoktium T1 nach T2
    --          ( T=(JD-2451545.0)/36525 )
    -----------------------------------------------------------------
    procedure PMATECL (T1,T2:     REAL;
                       A    : out REAL33);

    ------------------------------------------------------------------
    -- PMATEQU: Berechnung der Praezessionsmatrix A[i,j] fuer
    --          aequatoriale Koordinaten vom Aequinoktium T1 nach T2
    --          ( T=(JD-2451545.0)/36525 )
    ------------------------------------------------------------------
    procedure PMATEQU (T1,T2:     REAL;
                       A    : out REAL33);

    ----------------------------------------------------------------------------
    -- PN_MATRIX: Matrix fuer Praezession und Nutation vom mittleren
    --            Aequinoktium T0 ins wahre Aequinoktium T.
    -- T0  Ausgangsepoche in julian. Jahrh. seit J2000; T0=(JD0-2451545)/36525
    -- T   Endepoche in julianischen Jahrh. seit J2000; T=(JD-2451545)/36525
    ----------------------------------------------------------------------------
    procedure PN_MATRIX (T0,T:     REAL;
                         A   : out REAL33 );

    ----------------------------------------------------------------------
    -- PRECART: Praezessions-Transformation  bei bekannter Matrix A[i,j]
    --          fuer ekliptikale und aequatoriale Koordinaten
    --          ( zu verwenden in Verbindung mit PMATECL und PMATEQU )
    ----------------------------------------------------------------------
    procedure PRECART (A    :        REAL33;
                       X,Y,Z: in out REAL);

    procedure PRECART (A:        REAL33;
                       V: in out VECTOR);

  end PNULIB;


  ----------------------------------------
  -- Unit SPHLIB: sphaerische Astronomie
  ----------------------------------------
  package SPHLIB is

    use MATLIB, PNULIB;

    ------------------------------------------------------------------
    -- ABERRAT: Geschwindigkeitsvektor der Erde
    --          (aequatorial, in Einheiten der Lichtgeschwindigkeit)
    ------------------------------------------------------------------
    procedure ABERRAT (T:     REAL;
                       V: out VECTOR);

    ---------------------------------------------------------------------------
    -- APPARENT:  scheinbare Sternkoordinaten
    --   PNMAT:   Matrix fuer Praezession und Nutation
    --   V:       Geschwindigkeit der Erde (aequatorial, in Einheiten von c)
    --   RA,DEC:  Rektaszension und Deklination
    ---------------------------------------------------------------------------
    procedure APPARENT (PNMAT   :        REAL33;
                        V       :        VECTOR;
                        RA, DEC : in out REAL);

    -----------------------------------------------------------------
    -- ECLEQU: Umwandlung ekliptikaler Koordinaten in aequatoriale
    --         (T: Aequinoktium in julian.Jahrhunderten seit J2000)
    -----------------------------------------------------------------
    procedure ECLEQU (T    :        REAL;
                      X,Y,Z: in out REAL);

    ----------------------------------------------------------------
    -- EQUECL: Umwandlung aequatorialer Koordinaten in ekliptikale
    --         (T: Aequinoktium in julian.Jahrhunderten seit J2000)
    ----------------------------------------------------------------
    procedure EQUECL (T    :        REAL;
                      X,Y,Z: in out REAL);

    --------------------------------------------------------------------
    -- EQUHOR: Umwandlung aequatorialer Koordinaten ins Horizontsystem
    --   DEC  : Deklination (-90 Grad .. +90 Grad)
    --   TAU  : Stundenwinkel (0 Grad .. 360 Grad)
    --   PHI  : Geographische Breite (in Grad)
    --   H    : Hoehe des Gestirns in Grad
    --   AZ   : Azimut (0 Grad .. 360 Grad, Zaehlung S->W->N->O->S)
    --------------------------------------------------------------------
    procedure EQUHOR (DEC,TAU,PHI:     REAL;
                      H,AZ       : out REAL);

    ----------------------------------------------------------------------------
    -- HOREQU: Umwandlung von Koordinaten im Horizontsystem in aequatoriale K.
    --   H,AZ : Hoehe bzw. Azimut des Gestirns in Grad
    --   PHI  : Geographische Breite (in Grad)
    --   DEC  : Deklination (-90 Grad .. +90 Grad)
    --   TAU  : Stundenwinkel (0 Grad .. 360 Grad)
    ----------------------------------------------------------------------------
    procedure HOREQU (H,AZ,PHI:     REAL;
                      DEC,TAU : out REAL);

    ----------------------------------------------------------------------------
    -- EQUSTD: Transformation aequatorialer Koordinaten in Standardkoordinaten
    --   RA0,DEC0: Rektaszension und Deklination der optischen Achse (in Grad)
    --   RA,DEC:   Rektaszension und Deklination (in Grad)
    --   XX,YY:    Standardkoordinaten
    ----------------------------------------------------------------------------
    procedure EQUSTD (RA0,DEC0,RA,DEC:     REAL;
                      XX,YY          : out REAL);

    ------------------------------------------------------------------
    -- GAUSVEC: Berechnung der Gauss'schen Vektoren  (P,Q,R) aus
    --          ekliptikalen Bahnelementen:
    --          LAN = Knotenlaenge (longitude of ascending node)
    --          INC = Bahnneigung (inclination)
    --          AOP = Argument des Perihels (argument of perihelion)
    ------------------------------------------------------------------
    procedure GAUSVEC (LAN,INC,AOP :     REAL;
                       PQR         : out REAL33);

    -------------------------------------------------------------------
    -- ORBECL: Transformation von Koordinaten im System der Bahnebene
    --         in ekliptikale Koordinaten
    -------------------------------------------------------------------
    procedure ORBECL (XX,YY:     REAL;
                      PQR:       REAL33;
                      X,Y,Z: out REAL);

    ----------------------------------------------------------------------------
    -- SITE:  berechnet geozentrische aus geographischen Beobachterkoordinaten
    --        RCPHI:  r * cos(phi') (geozentrisch; in Erdradien)
    --        RSPHI:  r * sin(phi') (geozentrisch; in Erdradien)
    --        PHI:    geographische Breite (in Grad)
    ----------------------------------------------------------------------------
    procedure SITE (PHI:             REAL;
                    RCPHI,RSPHI: out REAL);

    -------------------------------------------------------------------------------
    -- STDEQU: Transformation von Standardkoordinaten in aequatoriale Koordinaten
    --   RA0,DEC0: Rektaszension und Deklination der optischen Achse (in Grad)
    --   XX,YY:    Standardkoordinaten
    --   RA,DEC:   Rektaszension und Deklination (in Grad)
    -------------------------------------------------------------------------------
    procedure STDEQU (RA0,DEC0,XX,YY:     REAL;
                      RA,DEC        : out REAL);

  end SPHLIB;


  --========================
  -- Unit MOOLIB: Mondbahn
  --========================

  package MOOLIB is

    use MATLIB, PNULIB, SPHLIB;

    ----------------------------------------------------------------------------
    -- MINI_MOON: Mondkoordinaten geringer Genauigkeit (ca.5'/1')
    --            T  : Zeit in jul.Jahrh. seit J2000  ( T=(JD-2451545)/36525 )
    --            RA : Rektaszension (in h)
    --            DEC: Deklination (in Grad)
    --            (Aequinoktium des Datums)
    ----------------------------------------------------------------------------
    procedure MINI_MOON (T     :     REAL;
                         RA,DEC: out REAL);

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
                    LAMBDA,BETA,R: out REAL);

    -----------------------------------------------------------------------------
    -- MOONEQU: aequatoriale Mondkoordinaten
    --          (Rektaszension RA und Deklination DEC in Grad, R in Erdradien)
    --          T in julian.Jahrhndt. seit J2000 ( T:= (JD - 2451545.0)/36525 )
    --          Die Koord. beziehen sich auf das wahre Aequinoktium des Datums.
    -----------------------------------------------------------------------------
    procedure MOONEQU(T:            REAL;
                      RA,DEC,R: out REAL);

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
                          RA_POLY,DE_POLY,R_POLY: out TPOLYNOM);

  end MOOLIB;


  --==========================
  -- Unit SUNLIB: Sonnenbahn
  --==========================

  package SUNLIB is

    use MATLIB, PNULIB, SPHLIB;

    ---------------------------------------------------------------------------
    -- MINI_SUN: Sonnenkoordinaten geringer Genauigkeit (ca.1')
    --           T  : Zeit in jul.Jahrh. seit J2000  ( T=(JD-2451545)/36525 )
    --           RA : Rektaszension (in h)
    --           DEC: Deklination (in Grad)
    --           (Aequinoktium des Datums)
    ---------------------------------------------------------------------------
    procedure MINI_SUN (T     :     REAL;
                        RA,DEC: out REAL);


    ------------------------------------------------------------------
    -- SUN200: Sonne; ekliptikale Koordinaten L,B,R (in Grad und AE)
    --         Aequinoktium des Datums
    --         (T: Zeit in julianischen Jahrhunderten seit J2000)
    --         (   = (JED-2451545.0)/36525                      )
    ------------------------------------------------------------------
    procedure SUN200 (T    :     REAL;
                      L,B,R: out REAL);


    ----------------------------------------------------------------------------
    -- SUNEQU: aequatoriale Sonnenkoordinaten
    --         (Rektaszension RA und Deklination DEC in Grad, R in AE)
    --         T in julian.Jahrhndt. seit J2000 ( T:= (JD - 2451545.0)/36525 )
    --         Die Koord. beziehen sich auf das wahre Aequinoktium des Datums.
    ----------------------------------------------------------------------------
    procedure SUNEQU(T       :     REAL;
                     RA,DEC,R: out REAL);

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
                         RA_POLY,DE_POLY,R_POLY: out TPOLYNOM);

  end SUNLIB;


  ---------------------------------------
  -- TIMLIB: Zeit- und Kalenderrechnung
  ---------------------------------------
  package TIMLIB is

    type HOURS is new REAL;

    -----------------------------------------------------------
    -- CALDAT: Bestimmung des Kalenderdatums
    --         aus dem Modifizierten Julianischen Datum (MJD)
    -----------------------------------------------------------
    procedure CALDAT (MJDL          :     REAL;
                      DAY,MONTH,YEAR: out Natural;
                      HOUR          : out HOURS);

    ----------------------------------------------------------------------------
    -- ETMINUT: Differenz Ephemeridenzeit - Weltzeit
    --          (Polynomdarstellung; gueltig von 2000-2050)
    --          T:     Zeit in jul.Jahrh. seit J2000 (=(JD-2451545.0)/36525.0)
    --          DTSEC: DT=ET-UT in sec (nur fuer VALID=TRUE)
    --          VALID: TRUE fuer Zeiten zwischen 2000 und 2050, sonst FALSE
    ----------------------------------------------------------------------------
    procedure ETMINUT (T    :     REAL;
                       DTSEC: out REAL;
                       VALID: out Boolean);

    ------------------------------------------------------------
    -- LMST: mittlere Ortssternzeit (local mean sidereal time)
    --       LAMBDA positive nach Westen !
    ------------------------------------------------------------
    function LMST (MJDL,LAMBDA:REAL) return HOURS;

    ----------------------------------------------------------
    -- MJD: Modifiziertes Julianisches Datum
    --      gueltig fuer jedes Datum seit 4713 v.Chr.
    --      julianischer Kalender bis zum 4. Oktober 1582
    --      gregorianischer Kalender ab dem 15. Oktober 1582
    ----------------------------------------------------------
    function MJD (DAY,MONTH,YEAR:Natural;
                  HOUR          :HOURS) return REAL;

  end TIMLIB;

end Astro;
