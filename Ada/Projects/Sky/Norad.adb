-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Error;
with Traces;

package body Norad is

  package Log is new Traces ("Norad");

  subtype Double is Astro.REAL;

  function Trunc (R : Double) return Double is
  begin
    return Double'truncation(R);
  end Trunc;

  use type Double;

  package Math is

    package Numeric is new Ada.Numerics.Generic_Elementary_Functions (Double);

    Pi : constant := Ada.Numerics.Pi;

    Two_Pi : constant := 2.0 * Pi;

    function Sqr (Arg : Double) return Double;

    function Cube (Arg : Double) return Double;

    function Sqrt (Arg : Double) return Double renames Numeric.Sqrt;

    function EXP (Arg : Double) return Double renames Numeric.Exp;

    function LN (Arg : Double) return Double renames Numeric.Log;

    function Cos (Arg : Double) return Double renames Numeric.Cos;

    function Sin (Arg : Double) return Double renames Numeric.Sin;

    function Power (Arg, Pwr : Double) return Double;

    function Radians (Arg : Double) return Double;

    function Fmod2p (Arg : Double) return Double;

    function Actan (Sinx, Cosx : Double) return Double;

  end Math;


  package body Math is

    function Sqr (Arg : Double) return Double is
    begin
      return Arg * Arg;
    end Sqr;


    function Cube (Arg : Double) return Double is
    begin
      return Arg * Arg * Arg;
    end Cube;


    function Arctan (Arg : Double) return Double is
    begin
      return Numeric.Arctan (Arg);
    end Arctan;


    function Power (Arg, Pwr : Double) return Double is
    begin
      if Arg > 0.0 then
        return EXP (Pwr * LN (Arg));
      else
        Log.Write ("Invalid argument in function Power!");
        raise Program_Error;
      end if;
    end Power;


    function Radians (Arg : Double) return Double is
    begin
      return Arg * Pi / 180.0;
    end Radians;


    function Modulus (Arg1, Arg2 : Double) return Double is
      Modu : Double;
    begin
      Modu := Arg1 - Trunc (Arg1 / Arg2) * Arg2;
      if Modu >= 0.0 then
        return Modu;
      else
        return Modu + Arg2;
      end if;
    end Modulus;


    function Fmod2p (Arg : Double) return Double is
    begin
      return Modulus (Arg, Two_Pi);
    end Fmod2p;


    function Actan (Sinx, Cosx : Double) return Double is
    begin
      if Cosx = 0.0 then
        if Sinx > 0.0 then
          return Pi / 2.0;
        else
          return 3.0 * Pi / 2.0;
        end if;
      elsif Cosx > 0.0 then
        return Arctan (Sinx / Cosx);
      else
        return Pi + Arctan (Sinx / Cosx);
      end if;
    end Actan;

  end Math;

  use Math;

  Ae     : constant Double := 1.0;
  Tothrd : constant Double := 2.0 / 3.0;
  Xkmper : constant Double := Astro.Earth_Equatorial_Radius;
  Ge     : constant Double := 398600.8;     -- Earth gravitational constant (WGS '72)
  J2     : constant Double := 1.0826158E-3; -- J2 harmonic (WGS '72)
  J3     : constant Double := -2.53881E-6;  -- J3 harmonic (WGS '72)
  J4     : constant Double := -1.65597E-6;  -- J4 harmonic (WGS '72)
  Ck2    : constant Double := J2 / 2.0;
  Ck4    : constant Double := -3.0 * J4 / 8.0;
  Xj3    : constant Double := J3;
  Qo     : constant Double := Ae + 120.0 / Xkmper;
  S      : constant Double := Ae + 78.0 / Xkmper;
  E6a    : constant Double := 1.0E-6;
  Xke    : constant Double := Sqrt (3600.0 * Ge / Cube (Xkmper)); --Sqrt(ge) ER^3/min^2}
  Qoms2t : constant Double := Sqr (Sqr (Qo -S ));                  --(qo-s)^4 ER^4}

  Xmnpda : constant := 1440.0; -- Minutes per day


  function Is_Deep_Space (Eo, Xincl, Xno: Double) return Boolean is
    A1, Ao, Del1, Delo, Xnodp, Temp : Double;
  begin
    A1 := Power (Xke / Xno, Tothrd);
    Temp := 1.5 * Ck2 * (3.0 * Sqr (Cos (Xincl)) - 1.0) / Power(1.0 - Eo * Eo, 1.5);
    Del1 := Temp / (A1 * A1);
    Ao := A1 * (1.0 - Del1 * (0.5 * Tothrd + Del1 * (1.0 + 134.0 / 81.0 * Del1)));
    Delo := Temp / (Ao * Ao);
    Xnodp := Xno / (1.0 + Delo);
    return Two_Pi / Xnodp >= 225.0;
  end Is_Deep_Space;


  function Is_In_Deep_Space (Data : Two_Line) return Boolean is
  begin
    return Is_Deep_Space (Eo    => Double'value(Data(2)(27..33)) * 1.0E-7,
                          Xincl => Double'value(Data(2)(9..16)),
                          Xno   => Double'value(Data(2)(53..63)) * Two_Pi / Xmnpda);
  end Is_In_Deep_Space;


  procedure SGP (Data         :     Two_Line;
                 Ut           :     Time.Ut;
                 The_Position : out Vector;
                 The_Velocity : out Vector) is

    Julian_Epoch : Double;
    Xndt2o       : Double;
    Xndd6o       : Double;
    Bstar        : Double;
    Xincl        : Double;
    Xnodeo       : Double;
    Eo           : Double;
    Omegao       : Double;
    Xmo          : Double;
    Xno          : Double;

    procedure Convert_Satellite_Data (The_Data : Two_Line) is

    --Cat_Nr     : Natural;
      Epoch_Year : Natural;
      Epoch_Day  : Double;

      function Julian_Date_Of_Epoch return Double is
        A, B : Integer;
      begin
        if Epoch_Year >= 57 then
          Error.Raise_With ("NORAD - Epoch year" & Natural'image(Epoch_Year + 1900) & " not supported!");
        end if;
        -- Astronomical Formulae for Calculators, Jean Meeus, pages 23-25
        -- Calculate Julian Date of 0.0 Jan year
        Epoch_Year := Epoch_Year + 1999;
        A := Epoch_Year / 100;
        B := 2 - A + (A / 4);
        return Double(Trunc (365.25 * Double(Epoch_Year)) + Trunc (30.6001 * 14.0)) + 1720994.5 + Double(B) -- year
               + Epoch_Day;
      end Julian_Date_Of_Epoch;

      Iexp, Ibexp : Integer;

    begin -- Convert_Satellite_Data
      -- Decode Line 1
    --Cat_Nr := Natural'value(The_Data(1)(3..7));
      Epoch_Year := Natural'value(The_Data(1)(19..20));
      Epoch_Day := Double'value(The_Data(1)(21..32));
      Julian_Epoch := Julian_Date_Of_Epoch;
      Xndt2o := Double'value(The_Data(1)(34..43));
      Xndd6o := Double'value(The_Data(1)(45..50)) * 1.0E-5;
      Iexp := Integer'value(The_Data(1)(51..52));
      Bstar := Double'value(The_Data(1)(54..59)) * 1.0E-5;
      Ibexp := Integer'value(The_Data(1)(60..61));

      -- Decode Line 2
      Xincl := Double'value(The_Data(2)(9..16));
      Xnodeo := Double'value(The_Data(2)(18..25));
      Eo := Double'value(The_Data(2)(27..33)) * 1.0E-7;
      Omegao := Double'value(The_Data(2)(35..42));
      Xmo := Double'value(The_Data(2)(44..51));
      Xno := Double'value(The_Data(2)(53..63)); -- period := 1.0 / xno;

      -- Convert to proper units
      Xndd6o := Xndd6o * 10.0 ** Iexp;
      Bstar := (Bstar * 10.0 ** Ibexp) / Ae;
      Xnodeo := Radians (Xnodeo);
      Omegao := Radians (Omegao);
      Xmo := Radians (Xmo);
      Xincl := Radians (Xincl);
      Xno := Xno * Two_Pi / Xmnpda;
      Xndt2o := Xndt2o * Two_Pi / Sqr (Xmnpda);
      Xndd6o := Xndd6o * Two_Pi / Cube (Xmnpda);
    end Convert_Satellite_Data;


    procedure Convert_Sat_State (With_Position : in out Vector;
                                 And_Velocity  : in out Vector) is
    begin
      for Index in Astro.AXIS loop
        With_Position(Index) := With_Position(Index) * Xkmper;      -- kilometers
        And_Velocity(Index) := And_Velocity(Index) * Xkmper / 60.0; -- kilometers / second
      end loop;
    end Convert_Sat_State;


    procedure SGP4 (Tsince       :     Double;
                    New_Position : out Vector;
                    New_Velocity : out Vector) is

      A1     : Double := 0.0;  A3ovk2 : Double := 0.0;  Ao     : Double  := 0.0;
      Aodp   : Double := 0.0;  Aycof  : Double := 0.0;  Betao  : Double  := 0.0;
      Betao2 : Double := 0.0;  C1     : Double := 0.0;  C1sq   : Double  := 0.0;
      C2     : Double := 0.0;  C3     : Double := 0.0;  C4     : Double  := 0.0;
      C5     : Double := 0.0;  Coef   : Double := 0.0;  Coef1  : Double  := 0.0;
      Cosio  : Double := 0.0;  D2     : Double := 0.0;  D3     : Double  := 0.0;
      D4     : Double := 0.0;  Del1   : Double := 0.0;  Delmo  : Double  := 0.0;
      Delo   : Double := 0.0;  Eeta   : Double := 0.0;  Eosq   : Double  := 0.0;
      Eta    : Double := 0.0;  Etasq  : Double := 0.0;  Isimp  : Integer := 0;
      Omgcof : Double := 0.0;  Omgdot : Double := 0.0;  Perige : Double  := 0.0;
      Pinvsq : Double := 0.0;  Psisq  : Double := 0.0;  Qoms24 : Double  := 0.0;
      S4     : Double := 0.0;  Sinio  : Double := 0.0;  Sinmo  : Double  := 0.0;
      T2cof  : Double := 0.0;  T3cof  : Double := 0.0;  T4cof  : Double  := 0.0;
      T5cof  : Double := 0.0;  Temp   : Double := 0.0;  Temp1  : Double  := 0.0;
      Temp2  : Double := 0.0;  Temp3  : Double := 0.0;  Theta2 : Double  := 0.0;
      Theta4 : Double := 0.0;  Tsi    : Double := 0.0;  X1m5th : Double  := 0.0;
      X1mth2 : Double := 0.0;  X3thm1 : Double := 0.0;  X7thm1 : Double  := 0.0;
      Xhdot1 : Double := 0.0;  Xlcof  : Double := 0.0;  Xmcof  : Double  := 0.0;
      Xmdot  : Double := 0.0;  Xnodcf : Double := 0.0;  Xnodot : Double  := 0.0;
      Xnodp  : Double := 0.0;

      Cosuk, Sinuk, Rfdotk, Vx, Vy, Vz, Ux, Uy, Uz, Xmy, Xmx,
      Cosnok, Sinnok, Cosik, Sinik, Rdotk, Xinck, Xnodek, Uk, Rk,
      Cos2u, Sin2u, U, Sinu, Cosu,  Betal, Rfdot, Rdot,  R, Pl, Elsq,
      Esine, Ecose, Epw, Temp6, Temp5, Temp4, Cosepw, Sinepw,
      Capu, Ayn, Xlt, Aynl, Xll, Axn, Xn, Beta, Xl, E, A, Tfour,
      Tcube, Delm, Delomg, Templ, Tempe, Tempa, Xnode, Tsq, Xmp,
      Omega, Xnoddf, Omgadf, Xmdf, X, Y, Z, Xdot, Ydot, Zdot     : Double;

    begin
      -- Recover original mean motion (xnodp) and semimajor axis (aodp)
      -- from input elements.
      A1 := Power (Xke / Xno, Tothrd);
      Cosio := Cos (Xincl);
      Theta2 := Cosio * Cosio;
      X3thm1 := 3.0 * Theta2 - 1.0;
      Eosq := Eo * Eo;
      Betao2 := 1.0 - Eosq;
      Betao := Sqrt (Betao2);
      Del1 := 1.5 * Ck2 * X3thm1 / (A1 * A1 * Betao * Betao2);
      Ao := A1 * (1.0 - Del1 * (0.5 * Tothrd + Del1 * (1.0 + 134.0 / 81.0 * Del1)));
      Delo := 1.5 * Ck2 * X3thm1 / (Ao * Ao * Betao * Betao2);
      Xnodp := Xno / (1.0 + Delo);
      Aodp := Ao / (1.0 - Delo);

      -- Initialization
      -- for perigee less than 220 kilometers, the isimp flag is set and
      -- the equations are truncated to linear variation in sqrt a and
      -- quadratic variation in mean anomaly.  Also, the c3 term, the
      -- delta omega term, and the delta m term are dropped.
      Isimp := 0;
      if (Aodp * (1.0 - Eo) / Ae) < (220.0 / Xkmper + Ae) then
        Isimp := 1;
      end if;

      -- for perigee below 156 km, the values of s and qoms2t are altered.
      S4 := S;
      Qoms24 := Qoms2t;
      Perige := (Aodp * (1.0 - Eo) - Ae) * Xkmper;
      if Perige < 156.0 then
        S4 := Perige - 78.0;
        if Perige <= 98.0 then
          S4 := 20.0;
        end if;
        Qoms24 := Power ((120.0 - S4) * Ae / Xkmper, 4.0);
        S4 := S4 / Xkmper + Ae;
      end if;
      Pinvsq := 1.0 / (Aodp * Aodp * Betao2 * Betao2);
      Tsi := 1.0 / (Aodp - S4);
      Eta := Aodp * Eo * Tsi;
      Etasq := Eta * Eta;
      Eeta := Eo * Eta;
      Psisq := abs (1.0 - Etasq);
      Coef := Qoms24 * Power (Tsi, 4.0);
      Coef1 := Coef / Power (Psisq, 3.5);
      C2 := Coef1 * Xnodp * (Aodp * (1.0 + 1.5 * Etasq + Eeta * (4.0 + Etasq))
          + 0.75 * Ck2 * Tsi / Psisq * X3thm1 * (8.0 + 3.0 * Etasq * (8.0 + Etasq)));
      C1 := Bstar * C2;
      Sinio := Sin (Xincl);
      A3ovk2 := - Xj3 / Ck2 * Power (Ae, 3.0);
      C3 := Coef * Tsi * A3ovk2 * Xnodp * Ae * Sinio / Eo;
      X1mth2 := 1.0 - Theta2;
      C4 := 2.0 * Xnodp * Coef1 * Aodp * Betao2 * (Eta * (2.0 + 0.5 * Etasq)
          + Eo * (0.5 + 2.0 * Etasq) - 2.0 * Ck2 * Tsi / (Aodp * Psisq)
          * (-3.0 * X3thm1 * (1.0 - 2.0 * Eeta + Etasq * (1.5 - 0.5 * Eeta))
          + 0.75 * X1mth2 * (2.0 * Etasq - Eeta * (1.0 + Etasq)) * Cos (2.0 * Omegao)));
      C5 := 2.0 * Coef1 * Aodp * Betao2 * (1.0 + 2.75 * (Etasq + Eeta) + Eeta * Etasq);
      Theta4 := Theta2 * Theta2;
      Temp1 := 3.0 * Ck2 * Pinvsq * Xnodp;
      Temp2 := Temp1 * Ck2 * Pinvsq;
      Temp3 := 1.25 * Ck4 * Pinvsq * Pinvsq * Xnodp;
      Xmdot := Xnodp + 0.5 * Temp1 * Betao * X3thm1
             + 0.0625 * Temp2 * Betao * (13.0 - 78.0 * Theta2 + 137.0 * Theta4);
      X1m5th := 1.0 - 5.0 * Theta2;
      Omgdot := -0.5 * Temp1 * X1m5th + 0.0625 * Temp2 * (7.0 - 114.0 * Theta2 + 395.0 * Theta4)
              + Temp3 * (3.0 - 36.0 * Theta2 + 49.0 * Theta4);
      Xhdot1 := -Temp1 * Cosio;
      Xnodot := Xhdot1 + (0.5 * Temp2 * (4.0 - 19.0 * Theta2)
              + 2.0 * Temp3 * (3.0 - 7.0 * Theta2)) * Cosio;
      Omgcof := Bstar * C3 * Cos (Omegao);
      Xmcof := -Tothrd * Coef * Bstar *Ae / Eeta;
      Xnodcf := 3.5 * Betao2 * Xhdot1 *C1;
      T2cof := 1.5 * C1;
      Xlcof := 0.125 * A3ovk2 * Sinio * (3.0 + 5.0 * Cosio) / (1.0 + Cosio);
      Aycof := 0.25 * A3ovk2 * Sinio;
      Delmo := Power (1.0 + Eta * Cos (Xmo), 3.0);
      Sinmo := Sin (Xmo);
      X7thm1 := 7.0 * Theta2 - 1.0;
      if Isimp /= 1 then
        C1sq := C1 * C1;
        D2 := 4.0 * Aodp * Tsi * C1sq;
        Temp := D2 * Tsi *C1 / 3.0;
        D3 := (17.0 * Aodp + S4) * Temp;
        D4 := 0.5 * Temp * Aodp * Tsi * (221.0 * Aodp + 31.0 * S4) * C1;
        T3cof := D2 + 2.0 * C1sq;
        T4cof := 0.25 * (3.0 * D3 + C1 * (12.0 * D2 + 10.0 * C1sq));
        T5cof := 0.2 * (3.0 * D4 + 12.0 * C1 * D3 + 6.0 * D2 * D2 + 15.0 * C1sq * (2.0 * D2 + C1sq));
      end if;

      -- Update for secular gravity and atmospheric drag
      Xmdf := Xmo + Xmdot * Tsince;
      Omgadf := Omegao + Omgdot * Tsince;
      Xnoddf := Xnodeo + Xnodot * Tsince;
      Omega := Omgadf;
      Xmp := Xmdf;
      Tsq := Tsince * Tsince;
      Xnode := Xnoddf + Xnodcf*Tsq;
      Tempa := 1.0 - C1 * Tsince;
      Tempe := Bstar * C4 * Tsince;
      Templ := T2cof * Tsq;
      if Isimp /= 1 then
        Delomg := Omgcof * Tsince;
        Delm := Xmcof * (Power (1.0 + Eta * Cos (Xmdf), 3.0) - Delmo);
        Temp := Delomg + Delm;
        Xmp := Xmdf + Temp;
        Omega := Omgadf - Temp;
        Tcube := Tsq * Tsince;
        Tfour := Tsince * Tcube;
        Tempa := Tempa - D2 * Tsq - D3 * Tcube - D4 * Tfour;
        Tempe := Tempe + Bstar * C5 * (Sin(Xmp) - Sinmo);
        Templ := Templ + T3cof * Tcube + Tfour * (T4cof + Tsince * T5cof);
      end if;
      A := Aodp * Sqr (Tempa);
      E := Eo - Tempe;
      Xl := Xmp + Omega + Xnode + Xnodp * Templ;
      Beta := Sqrt (1.0 - E * E);
      Xn := Xke / Power(A, 1.5);

      --Long period periodics
      Axn := E * Cos (Omega);
      Temp := 1.0 / (A * Beta * Beta);
      Xll := Temp * Xlcof * Axn;
      Aynl := Temp * Aycof;
      Xlt := Xl + Xll;
      Ayn := E * Sin (Omega) + Aynl;

      --Solve Kepler's Equation
      Capu := Fmod2p (Xlt - Xnode);
      Temp2 := Capu;
      for Index in 1 .. 10 loop
        pragma Unreferenced (Index);
        Sinepw := Sin (Temp2);
        Cosepw := Cos (Temp2);
        Temp3 := Axn * Sinepw;
        Temp4 := Ayn * Cosepw;
        Temp5 := Axn * Cosepw;
        Temp6 := Ayn * Sinepw;
        Epw := (Capu - Temp4 + Temp3 - Temp2) / (1.0 - Temp5 - Temp6) + Temp2;
        exit when abs (Epw - Temp2) <= E6a;
        Temp2 := Epw;
      end loop;

      -- Short period preliminary quantities
      Ecose := Temp5 + Temp6;
      Esine := Temp3 - Temp4;
      Elsq := Axn * Axn + Ayn * Ayn;
      Temp := 1.0 - Elsq;
      Pl := A * Temp;
      R := A * (1.0 - Ecose);
      Temp1 := 1.0 / R;
      Rdot := Xke * Sqrt (A) * Esine * Temp1;
      Rfdot := Xke * Sqrt (Pl) * Temp1;
      Temp2 := A * Temp1;
      Betal := Sqrt (Temp);
      Temp3 := 1.0 / (1.0 + Betal);
      Cosu := Temp2 * (Cosepw - Axn + Ayn * Esine * Temp3);
      Sinu := Temp2 * (Sinepw - Ayn - Axn * Esine * Temp3);
      U := Actan (Sinu,Cosu);
      Sin2u := 2.0 * Sinu * Cosu;
      Cos2u := 2.0 * Cosu * Cosu - 1.0;
      Temp := 1.0 / Pl;
      Temp1 := Ck2 * Temp;
      Temp2 := Temp1 * Temp;

      -- Update for short periodics
      Rk := R * (1.0 - 1.5 * Temp2 * Betal * X3thm1) + 0.5 * Temp1 * X1mth2 * Cos2u;
      Uk := U - 0.25 * Temp2 * X7thm1 * Sin2u;
      Xnodek := Xnode + 1.5 * Temp2 * Cosio * Sin2u;
      Xinck := Xincl + 1.5 * Temp2 * Cosio * Sinio * Cos2u;
      Rdotk := Rdot - Xn * Temp1 * X1mth2 * Sin2u;
      Rfdotk := Rfdot + Xn * Temp1 * (X1mth2 * Cos2u + 1.5 * X3thm1);

      -- Orientation vectors
      Sinuk := Sin (Uk);
      Cosuk := Cos (Uk);
      Sinik := Sin (Xinck);
      Cosik := Cos (Xinck);
      Sinnok := Sin (Xnodek);
      Cosnok := Cos (Xnodek);
      Xmx := -Sinnok * Cosik;
      Xmy := Cosnok * Cosik;
      Ux := Xmx * Sinuk + Cosnok * Cosuk;
      Uy := Xmy * Sinuk + Sinnok * Cosuk;
      Uz := Sinik * Sinuk;
      Vx := Xmx * Cosuk - Cosnok * Sinuk;
      Vy := Xmy * Cosuk - Sinnok * Sinuk;
      Vz := Sinik * Cosuk;

      --Position and velocity
      X := Rk * Ux;  New_Position(Astro.X) := X;
      Y := Rk * Uy;  New_Position(Astro.Y) := Y;
      Z := Rk * Uz;  New_Position(Astro.Z) := Z;
      Xdot := Rdotk * Ux + Rfdotk*Vx;  New_Velocity(Astro.X) := Xdot;
      Ydot := Rdotk * Uy + Rfdotk*Vy;  New_Velocity(Astro.Y) := Ydot;
      Zdot := Rdotk * Uz + Rfdotk*Vz;  New_Velocity(Astro.Z) := Zdot;
    end SGP4;

    Tsince : Double;

  begin -- SGP
    Convert_Satellite_Data (Data);
    Tsince := (Double(Time.Julian_Date_Of (Ut)) - Julian_Epoch) * Xmnpda;
    if Is_Deep_Space (Eo => Eo, Xincl => Xincl, Xno => Xno) then
      Error.Raise_With ("NORAD - Data for deep space not supported !");
      --SDP4 (Tsince, The_Position, The_Velocity);
    else
      SGP4 (Tsince, The_Position, The_Velocity);
    end if;

    Convert_Sat_State (The_Position, The_Velocity);
  exception
  when Error.Occurred =>
    raise;
  when others =>
    raise Bad_Data;
  end SGP;

end Norad;
