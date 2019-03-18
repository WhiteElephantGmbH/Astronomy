-- *********************************************************************************************************************
-- *                       (c) 2016 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Parameter;
with Traces;

package body Alignment.Calculations is

  use Astro;
  use MATLIB;
  use SPHLIB;

  package Log is new Traces ("Alignment.Calculations");

  use type Angle.Value;

  Quadrant    : constant Angle.Degrees := +Angle.Quadrant;
  Semi_Circle : constant Angle.Degrees := +Angle.Semi_Circle;
  Full_Circle : constant Angle.Degrees := Semi_Circle * 2.0;


  procedure Normalize (The_Value : in out REAL) is
  begin
    if The_Value > Semi_Circle then
      The_Value := The_Value - Full_Circle;
    elsif The_Value <= -Semi_Circle then
      The_Value := The_Value + Full_Circle;
    end if;
  end Normalize;


  procedure Prepare (Nominal_Alt    : REAL;
                     Nominal_Az     : REAL;
                     Actual_Alt     : REAL;
                     Actual_Az      : REAL;
                     Phi            : REAL;
                     The_Nominal_Az : out REAL;
                     The_Actual_Az  : out REAL;
                     The_Actual_Dec : out REAL;
                     The_Actual_Tau : out REAL;
                     The_Delta_Dec  : out REAL;
                     The_Delta_Tau  : out REAL) is
    The_Nominal_Dec : REAL;
    The_Nominal_Tau : REAL;
  begin
    The_Nominal_Az := Nominal_Az - Semi_Circle;
    The_Actual_Az := Actual_Az - Semi_Circle;
    HOREQU (H   => Nominal_Alt,
            AZ  => The_Nominal_Az,
            PHI => Phi,
            DEC => The_Nominal_Dec,
            TAU => The_Nominal_Tau);
    HOREQU (H   => Actual_Alt,
            AZ  => The_Actual_Az,
            PHI => Phi,
            DEC => The_Actual_Dec,
            TAU => The_Actual_Tau);
    The_Delta_Dec := The_Nominal_Dec - The_Actual_Dec;
    The_Delta_Tau := The_Nominal_Tau - The_Actual_Tau;
    Normalize (The_Delta_Dec);
    Normalize (The_Delta_Tau);
  end Prepare;


  procedure Evaluate_Tree_Star_Rotations (Nominal_Alt_1          :     REAL;
                                          Nominal_Az_1           :     REAL;
                                          Nominal_Alt_2          :     REAL;
                                          Nominal_Az_2           :     REAL;
                                          Nominal_Alt_3          :     REAL;
                                          Nominal_Az_3           :     REAL;
                                          Actual_Alt_1           :     REAL;
                                          Actual_Az_1            :     REAL;
                                          Actual_Alt_2           :     REAL;
                                          Actual_Az_2            :     REAL;
                                          Actual_Alt_3           :     REAL;
                                          Actual_Az_3            :     REAL;
                                          The_Pole_Height_Offset : out REAL;
                                          The_Pole_Az_Offset     : out REAL;
                                          The_Ra_Rotation        : out REAL;
                                          The_Dec_Rotation       : out REAL;
                                          The_System_Error       : out Astro.REAL) is

    Phi : constant REAL := +Parameter.Pole_Height;


    procedure Evaluate_Two_Star_Pole_Rotations (Star_1_Nominal_Alt :     REAL;
                                                Star_1_Nominal_Az  :     REAL;
                                                Star_2_Nominal_Alt :     REAL;
                                                Star_2_Nominal_Az  :     REAL;
                                                Star_1_Actual_Alt  :     REAL;
                                                Star_1_Actual_Az   :     REAL;
                                                Star_2_Actual_Alt  :     REAL;
                                                Star_2_Actual_Az   :     REAL;
                                                The_Height_Offset  : out REAL;
                                                The_Az_Offset      : out REAL;
                                                The_Rotation       : out REAL) is
      A1, H1, A2, H2, R1, D1, R2, D2     : REAL;
      U1, V1, W1, U2, V2, W2, U3, V3, W3 : REAL;
      M1, M2, M3, M4, M5, M6, Divisor    : REAL;
      X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3 : REAL;

    begin
      HOREQU (H   => Star_1_Nominal_Alt,
              AZ  => Star_1_Nominal_Az,
              PHI => Phi,
              DEC => D1,
              TAU => R1);
      D1 := Quadrant - D1;
      HOREQU (H   => Star_2_Nominal_Alt,
              AZ  => Star_2_Nominal_Az,
              PHI => Phi,
              DEC => D2,
              TAU => R2);
      D2 := Quadrant - D2;

      A1 := Star_1_Actual_Az;
      H1 := Quadrant - Star_1_Actual_Alt;
      A2 := Star_2_Actual_Az;
      H2 := Quadrant - Star_2_Actual_Alt;

      --Log.Write ("R1 =" & R1'img);
      --Log.Write ("D1 =" & D1'img);
      --Log.Write ("R2 =" & R2'img);
      --Log.Write ("D2 =" & D2'img);
      --
      --Log.Write ("A1 =" & A1'img);
      --Log.Write ("H1 =" & H1'img);
      --Log.Write ("A2 =" & A2'img);
      --Log.Write ("H2 =" & H2'img);

      U1 := SN(D1) * CS(R1);
      V1 := SN(D1) * SN(R1);
      W1 := CS(D1);
      U2 := SN(D2) * CS(R2);
      V2 := SN(D2) * SN(R2);
      W2 := CS(D2);
      U3 := V1 * W2 - W1 * V2;
      V3 := W1 * U2 - U1 * W2;
      W3 := U1 * V2 - V1 * U2;

      --Log.Write ("U1 =" & U1'img);
      --Log.Write ("V1 =" & V1'img);
      --Log.Write ("W1 =" & W1'img);
      --
      --Log.Write ("U2 =" & U2'img);
      --Log.Write ("V2 =" & V2'img);
      --Log.Write ("W2 =" & W2'img);
      --
      --Log.Write ("U3 =" & U3'img);
      --Log.Write ("V3 =" & V3'img);
      --Log.Write ("W3 =" & W3'img);

      Divisor := U3 * U3 + V3 * V3 + W3 * W3;
      M1 := (U2 * V3 - V2 * U3) / Divisor;
      M2 := (V1 * U3 - U1 * V3) / Divisor;
      M3 := (U1 * V2 - V1 * U2) / Divisor;

      --Log.Write ("M1 =" & M1'img);
      --Log.Write ("M2 =" & M2'img);
      --Log.Write ("M3 =" & M3'img);

      X1 := SN(H1) * CS(A1);
      Y1 := SN(H1) * SN(A1);
      Z1 := CS(H1);
      X2 := SN(H2) * CS(A2);
      Y2 := SN(H2) * SN(A2);
      Z2 := CS(H2);
      X3 := Y1 * Z2 - Z1 * Y2;
      Y3 := Z1 * X2 - X1 * Z2;
      Z3 := X1 * Y2 - Y1 * X2;

      --Log.Write ("X1 =" & X1'img);
      --Log.Write ("Y1 =" & Y1'img);
      --Log.Write ("Z1 =" & Z1'img);
      --
      --Log.Write ("X2 =" & X2'img);
      --Log.Write ("Y2 =" & Y2'img);
      --Log.Write ("Z2 =" & Z2'img);
      --
      --Log.Write ("X3 =" & X3'img);
      --Log.Write ("Y3 =" & Y3'img);
      --Log.Write ("Z3 =" & Z3'img);

      Divisor := X3 ** 2 + Y3 ** 2 + Z3 ** 2;
      M4 := (X2 * Y3 - Y2 * X3) / Divisor;
      M5 := (Y1 * X3 - X1 * Y3) / Divisor;
      M6 := (X1 * Y2 - Y1 * X2) / Divisor;

      --Log.Write ("M4 =" & M4'img);
      --Log.Write ("M5 =" & M5'img);
      --Log.Write ("M6 =" & M6'img);

      The_Height_Offset := Phi - Quadrant + ACS(M1 * Z1 + M2 * Z2 + M3 * Z3);
      The_Az_Offset := Semi_Circle - ATN2(M1 * Y1 + M2 * Y2 + M3 * Y3, M1 * X1 + M2 * X2 + M3 * X3);
      The_Rotation := ATN2(M4 * V1 + M5 * V2 + M6 * V3, M4 * U1 + M5 * U2 + M6 * U3);

      Normalize (The_Height_Offset);
      Normalize (The_Az_Offset);
      Normalize (The_Rotation);

      --Log.Write ("The_Height_Offset:" & The_Height_Offset'img);
      --Log.Write ("The_Az_Offset    :" & The_Az_Offset'img);
      --Log.Write ("The_Rotation     :" & The_Rotation'img);
    end Evaluate_Two_Star_Pole_Rotations;


    The_Nominal_Az_1 : REAL;
    The_Nominal_Az_2 : REAL;
    The_Nominal_Az_3 : REAL;

    The_Actual_Alt_1 : REAL;
    The_Actual_Az_1  : REAL;
    The_Actual_Alt_2 : REAL;
    The_Actual_Az_2  : REAL;
    The_Actual_Alt_3 : REAL;
    The_Actual_Az_3  : REAL;

    The_Actual_Dec_1 : REAL;
    The_Actual_Tau_1 : REAL;
    The_Actual_Dec_2 : REAL;
    The_Actual_Tau_2 : REAL;
    The_Actual_Dec_3 : REAL;
    The_Actual_Tau_3 : REAL;

    Pole_Height_Offset_1_2 : REAL;
    Pole_Az_Offset_1_2     : REAL;
    Pole_Rotation_1_2      : REAL;
    Pole_Height_Offset_2_3 : REAL;
    Pole_Az_Offset_2_3     : REAL;
    Pole_Rotation_2_3      : REAL;
    Pole_Height_Offset_1_3 : REAL;
    Pole_Az_Offset_1_3     : REAL;
    Pole_Rotation_1_3      : REAL;

    The_Delta_Dec_1       : REAL;
    The_Delta_Dec_2       : REAL;
    The_Delta_Dec_3       : REAL;
    The_Delta_Tau_1       : REAL;
    The_Delta_Tau_2       : REAL;
    The_Delta_Tau_3       : REAL;
    The_Best_Dec_Rotation : REAL;
    The_Tau_Rotation      : REAL;

    Actual_Height_Error   : REAL;
    Actual_Az_Error       : REAL;
    Actual_Rotation_Error : REAL;

    Next_Height_Error   : REAL;
    Next_Az_Error       : REAL;
    Next_Rotation_Error : REAL;
    Next_Error          : REAL;

    Max_Error : constant Angle.Degrees := 5.0;
    Increment : constant Angle.Degrees := 0.0001;

  begin -- Evaluate_Tree_Star_Rotations
    The_System_Error := REAL'last;
    Prepare (Nominal_Alt    => Nominal_Alt_1,
             Nominal_Az     => Nominal_Az_1,
             Actual_Alt     => Actual_Alt_1,
             Actual_Az      => Actual_Az_1,
             Phi            => Phi,
             The_Nominal_Az => The_Nominal_Az_1,
             The_Actual_Az  => The_Actual_Az_1,
             The_Actual_Dec => The_Actual_Dec_1,
             The_Actual_Tau => The_Actual_Tau_1,
             The_Delta_Dec  => The_Delta_Dec_1,
             The_Delta_Tau  => The_Delta_Tau_1);
    Prepare (Nominal_Alt    => Nominal_Alt_2,
             Nominal_Az     => Nominal_Az_2,
             Actual_Alt     => Actual_Alt_2,
             Actual_Az      => Actual_Az_2,
             Phi            => Phi,
             The_Nominal_Az => The_Nominal_Az_2,
             The_Actual_Az  => The_Actual_Az_2,
             The_Actual_Dec => The_Actual_Dec_2,
             The_Actual_Tau => The_Actual_Tau_2,
             The_Delta_Dec  => The_Delta_Dec_2,
             The_Delta_Tau  => The_Delta_Tau_2);
    Prepare (Nominal_Alt    => Nominal_Alt_3,
             Nominal_Az     => Nominal_Az_3,
             Actual_Alt     => Actual_Alt_3,
             Actual_Az      => Actual_Az_3,
             Phi            => Phi,
             The_Nominal_Az => The_Nominal_Az_3,
             The_Actual_Az  => The_Actual_Az_3,
             The_Actual_Dec => The_Actual_Dec_3,
             The_Actual_Tau => The_Actual_Tau_3,
             The_Delta_Dec  => The_Delta_Dec_3,
             The_Delta_Tau  => The_Delta_Tau_3);
    The_Dec_Rotation := (The_Delta_Dec_1 + The_Delta_Dec_2 + The_Delta_Dec_3) / 3.0;
    The_Tau_Rotation := (The_Delta_Tau_1 + The_Delta_Tau_2 + The_Delta_Tau_3) / 3.0;
    Log.Write ("Dec_Rotation:" & The_Dec_Rotation'img);
    Log.Write ("Tau_Rotation:" & The_Tau_Rotation'img);
    The_Dec_Rotation := The_Dec_Rotation - Max_Error;
    while The_Dec_Rotation <= Max_Error loop
      The_Dec_Rotation := The_Dec_Rotation + Increment;
      EQUHOR (DEC => The_Actual_Dec_1 + The_Dec_Rotation,
              TAU => The_Actual_Tau_1 + The_Tau_Rotation,
              PHI => Phi,
              H   => The_Actual_Alt_1,
              AZ  => The_Actual_Az_1);
      EQUHOR (DEC => The_Actual_Dec_2 + The_Dec_Rotation,
              TAU => The_Actual_Tau_2 + The_Tau_Rotation,
              PHI => Phi,
              H   => The_Actual_Alt_2,
              AZ  => The_Actual_Az_2);
      EQUHOR (DEC => The_Actual_Dec_3 + The_Dec_Rotation,
              TAU => The_Actual_Tau_3 + The_Tau_Rotation,
              PHI => Phi,
              H   => The_Actual_Alt_3,
              AZ  => The_Actual_Az_3);
      Evaluate_Two_Star_Pole_Rotations (Star_1_Nominal_Alt => Nominal_Alt_1,
                                        Star_1_Nominal_Az  => The_Nominal_Az_1,
                                        Star_2_Nominal_Alt => Nominal_Alt_2,
                                        Star_2_Nominal_Az  => The_Nominal_Az_2,
                                        Star_1_Actual_Alt  => The_Actual_Alt_1,
                                        Star_1_Actual_Az   => The_Actual_Az_1,
                                        Star_2_Actual_Alt  => The_Actual_Alt_2,
                                        Star_2_Actual_Az   => The_Actual_Az_2,
                                        The_Height_Offset  => Pole_Height_Offset_1_2,
                                        The_Az_Offset      => Pole_Az_Offset_1_2,
                                        The_Rotation       => Pole_Rotation_1_2);
      Evaluate_Two_Star_Pole_Rotations (Star_1_Nominal_Alt => Nominal_Alt_1,
                                        Star_1_Nominal_Az  => The_Nominal_Az_1,
                                        Star_2_Nominal_Alt => Nominal_Alt_3,
                                        Star_2_Nominal_Az  => The_Nominal_Az_3,
                                        Star_1_Actual_Alt  => The_Actual_Alt_1,
                                        Star_1_Actual_Az   => The_Actual_Az_1,
                                        Star_2_Actual_Alt  => The_Actual_Alt_3,
                                        Star_2_Actual_Az   => The_Actual_Az_3,
                                        The_Height_Offset  => Pole_Height_Offset_1_3,
                                        The_Az_Offset      => Pole_Az_Offset_1_3,
                                        The_Rotation       => Pole_Rotation_1_3);
      Evaluate_Two_Star_Pole_Rotations (Star_1_Nominal_Alt => Nominal_Alt_2,
                                        Star_1_Nominal_Az  => The_Nominal_Az_2,
                                        Star_2_Nominal_Alt => Nominal_Alt_3,
                                        Star_2_Nominal_Az  => The_Nominal_Az_3,
                                        Star_1_Actual_Alt  => The_Actual_Alt_2,
                                        Star_1_Actual_Az   => The_Actual_Az_2,
                                        Star_2_Actual_Alt  => The_Actual_Alt_3,
                                        Star_2_Actual_Az   => The_Actual_Az_3,
                                        The_Height_Offset  => Pole_Height_Offset_2_3,
                                        The_Az_Offset      => Pole_Az_Offset_2_3,
                                        The_Rotation       => Pole_Rotation_2_3);
      Next_Az_Error := (abs (Pole_Az_Offset_1_2 - Pole_Az_Offset_1_3) +
                        abs (Pole_Az_Offset_1_2 - Pole_Az_Offset_2_3) +
                        abs (Pole_Az_Offset_1_3 - Pole_Az_Offset_2_3)) / 3.0;
      Next_Height_Error := (abs (Pole_Height_Offset_1_2 - Pole_Height_Offset_1_3) +
                            abs (Pole_Height_Offset_1_2 - Pole_Height_Offset_2_3) +
                            abs (Pole_Height_Offset_1_3 - Pole_Height_Offset_2_3)) / 3.0;
      Next_Rotation_Error := (abs (Pole_Rotation_1_2 - Pole_Rotation_1_3) +
                              abs (Pole_Rotation_1_2 - Pole_Rotation_2_3) +
                              abs (Pole_Rotation_1_3 - Pole_Rotation_2_3)) / 3.0;
      Next_Error := (Next_Az_Error + Next_Height_Error + Next_Rotation_Error) / 3.0;
      if Next_Error < The_System_Error then
        The_System_Error := Next_Error;
        Actual_Az_Error := Next_Az_Error;
        Actual_Height_Error := Next_Height_Error;
        Actual_Rotation_Error := Next_Rotation_Error;
        The_Best_Dec_Rotation := The_Dec_Rotation;
        The_Pole_Height_Offset := Pole_Height_Offset_2_3;
        The_Pole_Az_Offset := Pole_Az_Offset_2_3;
        The_Ra_Rotation := Pole_Rotation_2_3;
      end if;
    end loop;
    The_Dec_Rotation := The_Best_Dec_Rotation;
    Log.Write ("System_Error  :" & The_System_Error'img);
    Log.Write ("Az_Error      :" & Actual_Az_Error'img);
    Log.Write ("Height_Error  :" & Actual_Height_Error'img);
    Log.Write ("Rotation_Error:" & Actual_Rotation_Error'img);

    Log.Write ("Dec_Rotation  :" & The_Dec_Rotation'img);
    Log.Write ("Tau_Rotation  :" & The_Tau_Rotation'img);
    The_Ra_Rotation := The_Ra_Rotation + The_Tau_Rotation;
    Normalize (The_Pole_Height_Offset);
    Normalize (The_Pole_Az_Offset);
    Normalize (The_Ra_Rotation);
  exception
  when Item: others =>
    Log.Termination (Item);
    raise Failed;
  end Evaluate_Tree_Star_Rotations;


  procedure Evaluate_Tree_Star_Corrections (Nominal_Alt        :     Astro.REAL;
                                            Nominal_Az         :     Astro.REAL;
                                            Actual_Alt         :     Astro.REAL;
                                            Actual_Az          :     Astro.REAL;
                                            The_Ra_Correction  : out Astro.REAL;
                                            The_Dec_Correction : out Astro.REAL) is

    Phi : constant REAL := +Parameter.Pole_Height;

    The_Nominal_Az : REAL;
    The_Actual_Az  : REAL;
    The_Actual_Dec : REAL;
    The_Actual_Tau : REAL;

  begin -- Evaluate_Tree_Star_Corrections
    Prepare (Nominal_Alt    => Nominal_Alt,
             Nominal_Az     => Nominal_Az,
             Actual_Alt     => Actual_Alt,
             Actual_Az      => Actual_Az,
             Phi            => Phi,
             The_Nominal_Az => The_Nominal_Az,
             The_Actual_Az  => The_Actual_Az,
             The_Actual_Dec => The_Actual_Dec,
             The_Actual_Tau => The_Actual_Tau,
             The_Delta_Dec  => The_Dec_Correction,
             The_Delta_Tau  => The_Ra_Correction);
  end Evaluate_Tree_Star_Corrections;

end Alignment.Calculations;
