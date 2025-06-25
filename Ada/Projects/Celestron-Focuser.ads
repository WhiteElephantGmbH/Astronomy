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

package Celestron.Focuser is

  type Command is (Decrease_Rate, Increase_Rate, Move_In, Move_Out, Stop);

  procedure Start;

  subtype Distance is Natural range 0 .. 2**24 - 1;

  type Lash is new Distance range 0 .. 2**8 - 1;

  subtype Rate is Natural range 1 .. 4;

  function Exists return Boolean;

  function Moving return Boolean;

  function Backlash return Lash;

  function Position return Distance;

  function Speed return Rate;

  procedure Execute (Item : Command);

  procedure Move_To (Item : Distance);

  procedure Set (Item : Lash);

  procedure Close;

end Celestron.Focuser;
