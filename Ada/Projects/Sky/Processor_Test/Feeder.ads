-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Space;
with Units;

package Feeder is

  procedure New_Darks (Id    : Units.Darks_Id;
                       Count : Units.Picture_Number);

  procedure Set (Focal_Length : Units.Focal_Length);

  procedure Set (Pixel_Size : Units.Pixel_Size);

  procedure New_Sequence (Target   : Space.Direction;
                          Darks_Id : Units.Darks_Id);

  procedure Next_Picture (Filename : String) with Inline;

  procedure Stop;

  Operation_Failed : exception;

end Feeder;
