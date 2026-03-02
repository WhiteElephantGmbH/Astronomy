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
pragma Style_White_Elephant;

with Astro;

package body Focus.Evaluation is

  use Astro;

  function Model (X  : REAL;
                  X0 : REAL;
                  A  : REAL;
                  B  : REAL) return REAL is
  begin
    return SQRT (A * (X - X0) * (X - X0) + B);
  end Model;


  function Error (Start_Position : Distance;
                  Position_Step  : Step;
                  HFD_Array      : Vektor;
                  X0             : REAL;
                  A              : REAL;
                  B              : REAL) return REAL is
    E : REAL     := 0.0;
    P : Distance := Start_Position;
  begin
    for I in HFD_Array'range loop
      declare
         W : constant REAL := 1.0 / (REAL(HFD_Array(I)) * REAL(HFD_Array(I)));
         M : constant REAL := Model (REAL(P), X0, A, B);
         D : constant REAL := REAL(HFD_Array(I)) - M;
      begin
         E := E + W * D * D;
      end;
      P := P + Position_Step;
    end loop;
    return E;
  end Error;


  function Best_For (Start_Position : Distance;
                     Position_Step  : Step;
                     HFD_Array      : Vektor) return Distance is

    X0 : REAL := REAL(Start_Position);

    A  : constant REAL := 0.001;
    B  : constant REAL := REAL(HFD_Array(HFD_Array'first)) ** 2;

    Inc    : REAL := 50.0;
    Best_E : REAL := Error (Start_Position, Position_Step, HFD_Array, X0, A, B);

  begin
    Log.Write ("Samples: " & HFD_Array'image);
    for Unused in 1 .. 200 loop
      declare
        Try_X0 : constant REAL := X0 + Inc;
        E2     : constant REAL := Error (Start_Position, Position_Step, HFD_Array, Try_X0, A, B);
      begin
        if E2 < Best_E then
          X0 := Try_X0;
          Best_E := E2;
        else
          Inc := -Inc * 0.5;
        end if;
      end;
    end loop;
    return Distance(X0);
  end Best_For;

end Focus.Evaluation;
