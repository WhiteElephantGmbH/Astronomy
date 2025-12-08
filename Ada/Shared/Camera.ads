-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Exposure;
with Sensitivity;

package Camera is

  Max_With_Or_Height : constant := 5496; -- maximum for Canon 6D
  Min_With_Or_Height : constant := 3464; -- minimum for Canon 60D

  type Model is (Canon_Eos_6D, Canon_Eos_60D);

  type Status is (Idle, Connecting, Connected, Capturing, Captured, Downloading, Cropping, Cropped, Stopping, Error);

  type Information is record
    State  : Status;
    Camera : Model;
  end record;

  Pixel_Size : constant := 16; -- allowed 8 or 16

  type Pixel is new Natural range 0 .. 2 ** Pixel_Size - 1 with Size => Pixel_Size;

  type Square_Size is new Natural range 2 .. Min_With_Or_Height with Dynamic_Predicate => Square_Size mod 2 = 0;

  type Columns is range 1 .. Max_With_Or_Height;
  type Rows    is range 1 .. Max_With_Or_Height;

  type Green_Grid is array (Rows range <>, Columns range <>) of Pixel;

  procedure Start;

  function Actual_Information return Information;

  procedure Capture (Filename : String;
                     Time     : Exposure.Item := Exposure.From_Camera;
                     Iso      : Sensitivity.Item := Sensitivity.From_Camera);

  procedure Capture (Size : Square_Size;
                     Time : Exposure.Item := Exposure.From_Camera;
                     Iso  : Sensitivity.Item := Sensitivity.From_Camera);

  function Captured return Green_Grid;

  function Image_Height return Rows;

  function Image_Width return Columns;

  procedure Stop;

  function Error_Message return String;

  procedure Finish;

end Camera;
