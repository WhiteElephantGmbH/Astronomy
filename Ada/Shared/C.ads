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

with Interfaces.C;

package C is

  subtype Bool               is Interfaces.C.C_bool;
  subtype Double             is Interfaces.C.double;
  subtype Long               is Interfaces.C.long;
  subtype Long_Long          is Interfaces.C.long_long;
  subtype Int                is Interfaces.C.int;
  subtype Uint8              is Interfaces.Unsigned_8;
  subtype Uint32             is Interfaces.Unsigned_32;
  subtype Unsigned_Short     is Interfaces.C.unsigned_short;
  subtype Unsigned_Long      is Interfaces.C.unsigned_long;
  subtype Unsigned_Long_Long is Interfaces.C.unsigned_long_long;

  type Name is new String with Pack, Convention => C;

end C;
