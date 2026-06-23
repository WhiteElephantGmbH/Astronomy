-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

pragma Build (Description => "Test GNAT 15.2",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNAT\15.2");

with Ada.Text_IO;
with Ada.Exceptions;

procedure Test is

  procedure Work is

    type REAL is new Long_Float;

  -- R with Volatile or Optimisation -O0 or using GNATPRO 23.0 fixes the problem

    R : REAL;
  --R : REAL with Volatile;

    N : constant := 3;

    C2, S2 : array ( 0..N) of REAL;
    C, S   : array (-N..0) of REAL;

    U, V : REAL;


    procedure ADD (C1, S1, C2L, S2L :     REAL;
                   CO, SO           : out REAL) is
    begin
      CO := C1*C2L - S1*S2L;
      SO := S1*C2L + C1*S2L;
    end ADD;


    procedure TERM (I1, I    : Integer;
                    DLC, DLS : REAL) is
    begin
      ADD (C2(I1), S2(I1), C(I), S(I), U, V);
      R := R + DLC*U + DLS*V;
    end TERM;


    procedure PERT_1 is
      COS_M : constant REAL := -0.810937203096633;
      SIN_M : constant REAL := 0.585133192216790;
    begin
      C(-1) := COS_M;
      S(-1) := -SIN_M;
      ADD (C(-1), S(-1), C(-1), S(-1), C(-2), S(-2));
      TERM (1, -1, 0.00,  0.00);
      TERM (2, -1, 0.25, -0.09);
    end PERT_1;


    procedure PERT_2 is
      COS_M : constant REAL := -0.977631621135276;
      SIN_M : constant REAL := 0.210324542924530;
    begin
      C(-1) := COS_M;
      S(-1) := -SIN_M;
      for I in reverse -(N-1) .. -1 loop
        ADD (C(I), S(I), C(-1), S(-1), C(I-1), S(I-1));
      end loop;
      TERM (1, 0, 2.37, 2793.23);
    end PERT_2;


    procedure PERT_3 is
      COS_M : constant REAL := 0.682917440967473;
      SIN_M : constant REAL := 0.730495563862258;
    begin
      C(-1) := COS_M;
      S(-1) := -SIN_M;
      for I in reverse -2 .. -1 loop
        ADD (C(I), S(I), C(-1), S(-1), C(I-1), S(I-1));
      end loop;
      TERM (1, -3, -0.65, 1.02);
      TERM (2, -2, -0.05, 0.04);
      TERM (2, -3, -0.50, 0.45);
    end PERT_3;


    procedure PERT_4 is
      COS_M : constant REAL := -0.238503621267387;
      SIN_M : constant REAL := 0.971141607924582;
    begin
      C(-1) := COS_M;
      S(-1) := -SIN_M;
      for I in reverse -2 .. -1 loop
        ADD (C(I), S(I), C(-1), S(-1), C(I-1), S(I-1));
      end loop;
      TERM (0, -1, -0.05,  1.56);
      TERM (1, -1, -2.62,  1.40);
      TERM (1, -2, -0.47, -0.08);
      TERM (2, -2, -0.73, -0.51);
      TERM (2, -3, -0.14, -0.10);
      TERM (3, -3, -0.01,  0.04);
    end PERT_4;

    COS_M : constant REAL := 0.476541619435702;
    SIN_M : constant REAL := 0.879151912325508;

  begin -- VEN200
    R := 0.0;
    C(0) := 1.0;
    S(0) := 0.0;
    C2(0) := 1.0;
    S2(0) := 0.0;
    C2(1) := COS_M;
    S2(1) := SIN_M;
    for I in 2 .. N loop
      ADD (C2(I-1), S2(I-1), C2(1), S2(1), C2(I), S2(I));
    end loop;
    PERT_1;
    PERT_2;
    PERT_3;
    PERT_4;
    Ada.Text_IO.Put_Line ("R:" & R'image);
    if Integer(R) = 2452 then
      Ada.Text_IO.Put_Line ("Test Ok");
    else
      Ada.Text_IO.Put_Line ("Test Failed");
    end if;
  end Work;

begin
  Work;
exception
when Item: others =>
  Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Item));
end Test;
