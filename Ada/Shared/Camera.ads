-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package Camera is

  procedure Start;

  procedure Capture;

  procedure Stop;

  procedure Finish;

private
  Exposure_Delta : constant := 0.000_001;

  type Exposure_Time is delta Exposure_Delta range Exposure_Delta .. 4194.0 with Small => Exposure_Delta, Size  => 32,
    Static_Predicate => Exposure_Time in -- Seconds
    30.0 | 25.0 | 20.0 | 15.0 | 13.0 | 10.0 | 8.0 | 6.0 | 5.0 | 4.0 |
     3.2 |  2.5 |  2.0 |  1.6 |  1.3 |  1.0 | 0.8 | 0.6 | 0.5 | 0.4 |
     0.3 |
     1.0 / 4    | 1.0 / 5    | 1.0 / 6    | 1.0 / 8    |
     1.0 / 10   | 1.0 / 13   | 1.0 / 15   | 1.0 / 20   | 1.0 / 25   | 1.0 / 30   |
     1.0 / 40   | 1.0 / 50   | 1.0 / 60   | 1.0 / 80   |
     1.0 / 100  | 1.0 / 125  | 1.0 / 160  | 1.0 / 200  | 1.0 / 250  | 1.0 / 320  |
     1.0 / 400  | 1.0 / 500  | 1.0 / 640  | 1.0 / 800  |
     1.0 / 1000 | 1.0 / 1250 | 1.0 / 1600 | 1.0 / 2000 | 1.0 / 2500 | 1.0 / 3200 |
     1.0 / 4000;

  type Iso_Value is new Natural range 100 .. 25600 with
    Static_Predicate => Iso_Value in 100 | 200 | 400 | 800 | 1600 | 3200 | 6400 | 12800 | 25600;

  Id : constant String := "Camera";

  procedure Define_Picture (Filename : String);

  procedure Define_Exposure (Exposure : Exposure_Time);

  procedure Define_Iso (Iso : Iso_Value);

end Camera;
