-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

private package Camera.QHYCCD is

  procedure Start_Control;

  function Is_Available return Boolean;

  procedure Capture_Picture (Filename  : String;
                             Time      : Exposure.Item;     -- 0.000_001 .. 3600.000_000 seconds
                             Parameter : Sensitivity.Item); -- Gain 0 .. 140 and Offset 0 .. 255

  procedure Capture_Grid (Size      : Square_Size;
                          Time      : Exposure.Item;     -- 0.000_001 .. 3600.000_000 seconds
                          Parameter : Sensitivity.Item); -- Gain 0 .. 140 and Offset 0 .. 255

  function Grid return Raw_Grid;

  procedure Stop_Capture;

  procedure End_Control;

end Camera.QHYCCD;
