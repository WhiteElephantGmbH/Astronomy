pragma Style_Astronomy;

pragma Build (Description => "Test GNAT 15.2",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNAT\15.2");

with Ada.Text_IO;
with Ada.Exceptions;
with Astr;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Test is

  procedure Test_Local is

    P2 : constant := 2.0 * Ada.Numerics.Pi;

    type REAL is new Long_Float;

    package Numeric is new Ada.Numerics.Generic_Elementary_Functions (REAL);

    function SIN (X : REAL) return REAL renames Numeric.Sin;

    function COS (X : REAL) return REAL renames Numeric.Cos;

    function FRAC (X:REAL) return REAL is
      VX : REAL := X-REAL'truncation(X);
    begin
      if VX<0.0 then
        VX:=VX+1.0;
      end if;
      return VX;
    end FRAC;

    package PLALIB is

      procedure VEN200 (T:         REAL;
                        L,B,R: out REAL);
    end PLALIB;

    package body PLALIB is

      procedure VEN200 (T:         REAL;
                        L,B,R: out REAL) is

        C2,S2:             array ( 0..8) of REAL;
        C,S:               array (-8..0) of REAL;
        M1,M2,M3,M4,M5,M6: REAL;
        U,V:               REAL;
        DL,DR,DB:          REAL;

        procedure ADDTHE (C1,S1,C2L,S2L:   REAL;
                          C,S:         out REAL) is
        begin
          C:=C1*C2L-S1*S2L; S:=S1*C2L+C1*S2L;
        end ADDTHE;

        procedure TERM (I1,I,IT                 :        Integer;
                        DLC,DLS,DRC,DRS,DBC,DBS :        REAL) is
        begin
          if IT=0 then
            ADDTHE(C2(I1),S2(I1),C(I),S(I),U,V);
          else
            U:=U*T; V:=V*T;
          end if;
          DL :=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
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
        M1:=P2*FRAC(0.4861431+415.2018375*T);
        M2:=P2*FRAC(0.1400197+162.5494552*T);
        M3:=P2*FRAC(0.9944153+ 99.9982208*T);
        M4:=P2*FRAC(0.0556297+ 53.1674631*T);
        M5:=P2*FRAC(0.0567028+  8.4305083*T);
        M6:=P2*FRAC(0.8830539+  3.3947206*T);
        C2(0):=1.0; S2(0):=0.0; C2(1):=COS(M2); S2(1):=SIN(M2);
        for I in 2 .. 8 loop
          ADDTHE(C2(I-1),S2(I-1),C2(1),S2(1),C2(I),S2(I));
        end loop;
        PERTMER; PERTEAR; PERTMAR; PERTJUP; PERTSAT;
        Ada.Text_IO.Put_Line ("1. DL:" & DL'image);
        DL:=DL + 2.74*SIN(P2*(0.0764+0.4174*T)) + 0.27*SIN(P2*(0.9201+0.3307*T));
        DL:=DL + (1.9+1.8*T);
        Ada.Text_IO.Put_Line ("2. DL:" & DL'image);
        L:= 360.0*FRAC(0.3654783 + M2/P2 + ((5071.2+1.1*T)*T+DL)/1296.0E3 );
        R:= 0.7233482 - 0.0000002*T  +  DR*1.0E-6;
        B:= ( -67.70 + ( 0.04 + 0.01*T) * T  +  DB ) / 3600.0;
      end VEN200;

    end PLALIB;

    LL, BL, RL : REAL;

  begin
    PLALIB.VEN200 (0.264725661675974, LL, BL, RL);
    Ada.Text_IO.Put_Line ("VEN200 -> L:=" & LL'image & " -> B:=" & BL'image & " -> R:=" & RL'image);
  end Test_Local;

   LA, BA, RA : Astr.REAL;
begin
   Test_Local;

   Astr.PLALIB.VEN200 (0.264725661675974, LA, BA, RA);
   Ada.Text_IO.Put_Line ("VEN200 -> L:=" & LA'image & " -> B:=" & BA'image & " -> R:=" & RA'image);

exception
when Item: others =>
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Item));
end Test;
