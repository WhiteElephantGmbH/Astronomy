-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Ada.Characters.Conversions;
with Npp.Plugin;

package body Npp.Buffer is

  function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);

  function Name return String is
    Path : aliased Wide_String(1..261);
    use type Win.LRESULT;
  begin
    if Win.Send_Message (Plugin.Handle, M_GETFULLCURRENTPATH, 0, Convert(Path'address)) = Win.OK then
      for The_Index in Path'range loop
        if Path(The_Index) = Wide_Character'val(0) then
          return Ada.Characters.Conversions.To_String (Path(1 .. The_Index - 1));
        end if;
      end loop;
    end if;
    return "";
  end Name;

end Npp.Buffer;