-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;

package Space is

  type Direction is private;

  subtype Distance is Angle.Degrees;

  Unknown_Direction : constant Direction;

  North_Pole : constant Direction;

  Axis_Pole_Left  : constant Direction;
  Axis_Pole_Right : constant Direction;
  Axis_Pole_Top   : constant Direction;

  function Direction_Of (Dec : Angle.Value;
                         Ra  : Angle.Value) return Direction with Inline;

  function Direction_Of (Dec : Angle.Degrees;
                         Ra  : Angle.Degrees) return Direction with Inline;

  function Direction_Of (Dec_Image : String;
                         Ra_Image  : String) return Direction with Inline;

  function Dec_Of (The_Direction : Direction) return Angle.Value with Inline;

  function Ra_Of (The_Direction : Direction) return Angle.Value with Inline;

  function Dec_Image_Of (The_Direction : Direction) return String with Inline;

  function Ra_Image_Of (The_Direction : Direction) return String with Inline;

  function Image_Of (The_Direction : Direction) return String;

  function Dec_Offset_Image_Of (The_Direction : Direction) return String with Inline;

  function Ra_Offset_Image_Of (The_Direction : Direction) return String with Inline;

  function Direction_Is_Known (The_Direction : Direction) return Boolean with Inline;

  function "-" (Left, Right : Direction) return Direction;

  function "-" (Left, Right : Direction) return Distance;
  -- fast calculation for the maximum distance (RA or Dec) with a correction close to the Pole

private

  type Direction is record
    Dec      : Angle.Value := Angle.Zero;
    Ra       : Angle.Value := Angle.Zero;
    Is_Known : Boolean     := False;
  end record;

  Unknown_Direction : constant Direction := (others => <>);

  North_Pole : constant Direction := (Dec      => Angle.Quadrant,
                                      Ra       => Angle.Zero,
                                      Is_Known => True);

  Axis_Pole_Left : constant Direction := (Dec      => Angle.Zero,
                                          Ra       => Angle.Semi_Circle,
                                          Is_Known => True);

  Axis_Pole_Right : constant Direction := (Dec      => Angle.Zero,
                                           Ra       => Angle.Zero,
                                           Is_Known => True);

  Axis_Pole_Top : constant Direction := (Dec      => Angle.Zero,
                                         Ra       => Angle.Quadrant,
                                         Is_Known => True);

end Space;
