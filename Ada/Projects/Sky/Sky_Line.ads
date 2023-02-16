-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Earth;
with Space;
with Time;

package Sky_Line is

  No_Top_Altitude : constant Angle.Value := Angle.Zero;

  procedure Read;

  procedure Create;

  procedure Append (Direction : Earth.Direction;
                    Top_Alt   : Angle.Value := No_Top_Altitude);

  procedure Close;

  procedure Clear;

  procedure Add (Direction : Earth.Direction);

  function Is_Defined return Boolean;

  function Is_Above (Direction : Earth.Direction;
                     Use_Upper : Boolean := False) return Boolean;

  function Is_Above (Direction : Space.Direction;
                     Lmst      : Time.Value;
                     Use_Upper : Boolean := False) return Boolean;
end Sky_Line;
