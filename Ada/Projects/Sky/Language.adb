-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Strings;
with Traces;

package body Language is

  package Log is new Traces (Id);

  The_Language : Kind := English;

  procedure Define (The_Kind : Kind) is
  begin
    Log.Write (Id & ": " & Strings.Legible_Of(The_Kind'image));
    The_Language := The_Kind;
  end Define;

  function Actual return Kind is
  begin
    return The_Language;
  end Actual;

end Language;
