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

with Astro;

private package Alignment.Calculations is

  Failed : exception;

  procedure Evaluate_Tree_Star_Rotations (Nominal_Alt_1          :     Astro.REAL;
                                          Nominal_Az_1           :     Astro.REAL;
                                          Nominal_Alt_2          :     Astro.REAL;
                                          Nominal_Az_2           :     Astro.REAL;
                                          Nominal_Alt_3          :     Astro.REAL;
                                          Nominal_Az_3           :     Astro.REAL;
                                          Actual_Alt_1           :     Astro.REAL;
                                          Actual_Az_1            :     Astro.REAL;
                                          Actual_Alt_2           :     Astro.REAL;
                                          Actual_Az_2            :     Astro.REAL;
                                          Actual_Alt_3           :     Astro.REAL;
                                          Actual_Az_3            :     Astro.REAL;
                                          The_Pole_Height_Offset : out Astro.REAL;
                                          The_Pole_Az_Offset     : out Astro.REAL;
                                          The_Ra_Rotation        : out Astro.REAL;
                                          The_Dec_Rotation       : out Astro.REAL;
                                          The_System_Error       : out Astro.REAL);

  procedure Evaluate_Tree_Star_Corrections (Nominal_Alt        :     Astro.REAL;
                                            Nominal_Az         :     Astro.REAL;
                                            Actual_Alt         :     Astro.REAL;
                                            Actual_Az          :     Astro.REAL;
                                            The_Ra_Correction  : out Astro.REAL;
                                            The_Dec_Correction : out Astro.REAL);
end Alignment.Calculations;
