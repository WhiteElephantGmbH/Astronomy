-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                                                                                                                   *
-- *    A big part of this software was created by                                                                     *
-- *    Dr Stephen J. Sangwine sangwine@users.sourceforge.net                                                          *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Png.Adam7 is

  -- This package is implemented using table-driven code, because the Adam7
  -- scheme is quite complex, but at the same time systematic and capable of
  -- coding using tables.

  -- The next table is the fundamental definition of the Adam7 scheme.
  -- It represents an 8 by 8 block of pixels within the image and it indicates
  -- for each pixel in the block which of the seven passes the pixel falls within.
  -- This is documented in Section 8.2 of the ISO standard.

  subtype Index is Natural range 0 .. 7;

  Pass_Table : constant array(Index, Index) of Pass_Number :=

                   --   0  1  2  3  4  5  6  7

                (0 => ( 1, 6, 4, 6, 2, 6, 4, 6 ),
                 1 => ( 7, 7, 7, 7, 7, 7, 7, 7 ),
                 2 => ( 5, 6, 5, 6, 5, 6, 5, 6 ),
                 3 => ( 7, 7, 7, 7, 7, 7, 7, 7 ),
                 4 => ( 3, 6, 4, 6, 3, 6, 4, 6 ),
                 5 => ( 7, 7, 7, 7, 7, 7, 7, 7 ),
                 6 => ( 5, 6, 5, 6, 5, 6, 5, 6 ),
                 7 => ( 7, 7, 7, 7, 7, 7, 7, 7 ));

  -- The next two tables give the number of scan lines and columns within an 8 by 8 block
  -- which are accessed on each pass. Pass 7 for example accesses all 8 columns of the
  -- block, but only 4 rows.

  N_Lines   : constant array(Pass_Number) of Positive range 1 .. 4 := (1, 1, 1, 2, 2, 4, 4);
  N_Columns : constant array(Pass_Number) of Positive range 1 .. 8 := (1, 1, 2, 2, 4, 4, 8);


  function Pass (R, C : Coordinate) return Pass_Number is
  begin
    return (Pass_Table(R mod 8, C mod 8));
  end Pass;


  function Sub_Image_Width (W : Dimension; P : Pass_Number) return Natural is

    -- The width of a sub-image in a given pass is determined by the number
    -- of columns in each 8 by 8 block which contain pixels in that pass,
    -- times the number of 8 by 8 blocks across the image, plus an addition
    -- for the odd few columns (if any) past the last complete 8. The latter
    -- is what is tabulated below.

    Extra_Columns : constant array(Index, Pass_Number) of Index :=
                (0 => (0, 0, 0, 0, 0, 0, 0),
                 1 => (1, 0, 1, 0, 1, 0, 1),
                 2 => (1, 0, 1, 0, 1, 1, 2),
                 3 => (1, 0, 1, 1, 2, 1, 3),
                 4 => (1, 0, 1, 1, 2, 2, 4),
                 5 => (1, 1, 2, 1, 3, 2, 5),
                 6 => (1, 1, 2, 1, 3, 3, 6),
                 7 => (1, 1, 2, 2, 4, 3, 7));
  begin
    return (W/8) * N_Columns(P) + Extra_Columns(W mod 8, P);
  end Sub_Image_Width;


  function Sub_Image_Height (H : Dimension; P : Pass_Number) return Natural is
    -- See comment in previous function.
    Extra_Lines : constant array(Index, Pass_Number) of Index :=
                (0 => (0, 0, 0, 0, 0, 0, 0),
                 1 => (1, 1, 0, 1, 0, 1, 0),
                 2 => (1, 1, 0, 1, 0, 1, 1),
                 3 => (1, 1, 0, 1, 1, 2, 1),
                 4 => (1, 1, 0, 1, 1, 2, 2),
                 5 => (1, 1, 1, 2, 1, 3, 2),
                 6 => (1, 1, 1, 2, 1, 3, 3),
                 7 => (1, 1, 1, 2, 2, 4, 3));
  begin
    return (H/8) * N_Lines(P) + Extra_Lines(H mod 8, P);
  end Sub_Image_Height;


  X : constant := -1; -- Indicates a don't care value.

  subtype Image_Offset is Integer range X .. Index'last;


  function Sub_Image_Row (R, C : Coordinate) return Coordinate is
    P : constant Pass_Number := Pass(R, C);
    T : constant array(Index, Pass_Number) of Image_Offset :=
                   --  1  2  3  4  5  6  7
                (0 => (0, 0, X, 0, X, 0, X),
                 1 => (X, X, X, X, X, X, 0),
                 2 => (X, X, X, X, 0, 1, X),
                 3 => (X, X, X, X, X, X, 1),
                 4 => (X, X, 0, 1, X, 2, X),
                 5 => (X, X, X, X, X, X, 2),
                 6 => (X, X, X, X, 1, 3, X),
                 7 => (X, X, X, X, X, X, 3));
    O : constant Image_Offset := T(R mod 8, P);
  begin
    if O = X then
      raise Program_Error;
    end if;
    return (R/8) * N_Lines(P) + O;
  end Sub_Image_Row;


  function Sub_Image_Col (R, C : Coordinate) return Coordinate is
    P : constant Pass_Number := Pass(R, C);
    T : constant array(Pass_Number, Index) of Image_Offset :=
                   --  0  1  2  3  4  5  6  7
                (1 => (0, X, X, X, X, X, X, X),
                 2 => (X, X, X, X, 0, X, X, X),
                 3 => (0, X, X, X, 1, X, X, X),
                 4 => (X, X, 0, X, X, X, 1, X),
                 5 => (0, X, 1, X, 2, X, 3, X),
                 6 => (X, 0, X, 1, X, 2, X, 3),
                 7 => (0, 1, 2, 3, 4, 5, 6, 7));
    O : constant Image_Offset := T(P, C mod 8);
  begin
    if O = X then
      raise Program_Error;
    end if;
    return (C/8) * N_Columns(P) + O;
  end Sub_Image_Col;


  -- The two following functions map from a coordinate within a sub-image to the
  -- corresponding coordinate in the whole image. There is no error checking here
  -- because the algorithm is valid for arbitrary sized images.

  function Image_Row (R : Coordinate; P : Pass_Number) return Coordinate is
    Scale  : constant array(Pass_Number) of Positive range 2 .. 8 := (8, 8, 8, 4, 4, 2, 2);
    Offset : constant array(Pass_Number) of Natural  range 0 .. 4 := (0, 0, 4, 0, 2, 0, 1);
  begin
    return R * Scale(P) + Offset(P);
  end Image_Row;


  function Image_Col (C : Coordinate; P : Pass_Number) return Coordinate is
    Scale  : constant array(Pass_Number) of Positive range 1 .. 8 := (8, 8, 4, 4, 2, 2, 1);
    Offset : constant array(Pass_Number) of Natural  range 0 .. 4 := (0, 4, 0, 2, 0, 1, 0);
  begin
    return C * Scale(P) + Offset(P);
  end Image_Col;

end Png.Adam7;
