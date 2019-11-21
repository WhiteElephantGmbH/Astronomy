-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Exceptions;

package Log is

  type Category is private;

  function "&" (Left, Right : Category) return Category;

  function Lookup (The_Category : String) return Category;

  function Debug return Category with Inline;

  function Is_Enabled (The_Category : Category) return Boolean with Inline;

  procedure Write (The_String : String) with Inline; -- category debug

  procedure Write (The_String   : String;
                   The_Category : Category) with Inline;

  procedure Write (Occurrence : Ada.Exceptions.Exception_Occurrence);

  procedure Write (Reason     : String;
                   Occurrence : Ada.Exceptions.Exception_Occurrence);

private

  Max_Number_Of_Categories : constant := 31;

  type Category is mod 2 ** (Max_Number_Of_Categories + 1);

end Log;
