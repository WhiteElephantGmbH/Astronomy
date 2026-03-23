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

pragma Build (Description => "Focus test",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWS64", "COLL64"),
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Exceptions;
with Astro;

procedure Focus_Test is

  package IO renames Ada.Text_IO;

  use Astro;

  type Distance is new Natural;

  type Vektor is array (Positive range <>) of REAL;


  function Model (X  : REAL;
                  X0 : REAL;
                  A  : REAL;
                  B  : REAL) return REAL is
  begin
    return SQRT (A * (X - X0) * (X - X0) + B);
  end Model;


  function Error (Start : Distance;
                  Step  : Distance;
                  Hfd   : Vektor;
                  X0    : REAL;
                  A     : REAL;
                  B     : REAL) return REAL is
    E : REAL := 0.0;
    P : Distance := Start;
  begin
    for I in Hfd'range loop
      declare
         W : constant REAL := 1.0 / (Hfd(I) * Hfd(I));
         M : constant REAL := Model (REAL(P), X0, A, B);
         D : constant REAL := Hfd(I) - M;
      begin
         E := E + W * D * D;
      end;
      P := P + Step;
    end loop;
    return E;
  end Error;


  function Best_Focus (Start : Distance;
                       Step  : Distance;
                       Hfd   : Vektor) return REAL is
    X0 : REAL := REAL(Start);
    A  : constant REAL := 0.001;
    B  : constant REAL := Hfd(Hfd'first) ** 2;

    Inc    : REAL := 50.0;
    Best_E : REAL := Error (Start, Step, Hfd, X0, A, B);

  begin
    for Unused in 1 .. 200 loop
      declare
        Try_X0 : constant REAL := X0 + Inc;
        E2     : constant REAL := Error (Start, Step, Hfd, Try_X0, A, B);
      begin
        if E2 < Best_E then
          X0 := Try_X0;
          Best_E := E2;
        else
          Inc := -Inc * 0.5;
        end if;
      end;
    end loop;
    return X0;
  end Best_Focus;

  Hfd : constant Vektor := [302.0, 190.0, 80.0, 90.0, 200.0];

  Focus : REAL;

begin
  Focus := Best_Focus (18200, 200, Hfd);
  IO.Put_Line ("Best Focus:" & Natural(Focus)'image);
exception
when Item: others =>
  IO.Put_Line ("Exception: " & Exceptions.Information_Of (Item));
end Focus_Test;
