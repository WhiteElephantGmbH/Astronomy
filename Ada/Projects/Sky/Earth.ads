-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package Earth is

  type Direction is private;

  Unknown_Direction : constant Direction;

  Zero_Direction : constant Direction;

  function Direction_Of (Alt : Angle.Value;
                         Az  : Angle.Value;
                         Inv : Boolean := False) return Direction with Inline;

  function Alt_Of (The_Direction : Direction) return Angle.Value with Inline;

  function Az_Of (The_Direction : Direction) return Angle.Value with Inline;

  function Alt_Image_Of (The_Direction : Direction) return String with Inline;

  function Az_Image_Of (The_Direction : Direction) return String with Inline;

  function Alt_Offset_Image_Of (The_Direction : Direction) return String with Inline;

  function Az_Offset_Image_Of (The_Direction : Direction) return String with Inline;

  function Is_Below_Horizon (The_Direction : Direction) return Boolean with Inline;

  function Direction_Is_Known (The_Direction : Direction) return Boolean with Inline;

  function Direction_Is_Inverse (The_Direction : Direction) return Boolean with Inline;

  function "+" (Left, Right : Direction) return Direction with Inline; -- uses Inv from Left

  function "-" (Left, Right : Direction) return Direction with Inline; -- uses Inv from Left

  procedure Add_Az_To (The_Direction : in out Direction;
                       The_Offset    :        Angle.Degrees) with Inline;

  procedure Add_Alt_To (The_Direction : in out Direction;
                        The_Offset    :        Angle.Degrees) with Inline;

  procedure Add_To (The_Direction : in out Direction;
                    The_Offset    : in out Direction) with Inline; -- altitude limited and offset corrected

private

  type Direction is record
    Alt        : Angle.Value := Angle.Zero;
    Az         : Angle.Value := Angle.Zero;
    Is_Known   : Boolean     := False;
    Is_Inverse : Boolean     := False;
  end record;

  Unknown_Direction : constant Direction := (others => <>);

  Zero_Direction : constant Direction := (Is_Known => True, others => <>);

end Earth;
