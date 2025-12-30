-- *********************************************************************************************************************
-- *                           (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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

with Ada.Numerics.Generic_Real_Arrays;
with Angle;
with Text;

package Astap is

  package Numeric is new Ada.Numerics.Generic_Real_Arrays (Angle.Degrees);

  Ra  : constant := Text.First_Index;
  Dec : constant := Ra + 1;

  subtype Index is Integer range Ra .. Dec;

  subtype Location is Numeric.Real_Vector (Index);

  subtype Matrix is Numeric.Real_Matrix (Index, Index);

  type Pixels is array (Index) of Positive;

  Not_Solved : exception;

  procedure Define (Executable : String);

  procedure Solve (Filename : String;
                   Height   : Angle.Degrees;
                   Start    : Location);
  -- exception: Not_Solved

  function Solved (The_Ra  : out Angle.Degrees;
                   The_Dec : out Angle.Degrees) return Boolean;
  -- exception: Not_Solved if no sulution found

  function Solved (CRVAL : out Location;
                   CD    : out Matrix;
                   Size  : out Pixels) return Boolean;
  -- exception: Not_Solved if no sulution found

  procedure Stop;

end Astap;
