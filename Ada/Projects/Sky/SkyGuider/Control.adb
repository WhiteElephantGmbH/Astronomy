-- *********************************************************************************************************************
-- *                               (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Exif;

package body Control is

  procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

  procedure Start is
  begin
    case Ada.Command_Line.Argument_Count is
    when 0 =>
      Put_Line ("Picture filename expected");
    when 1 =>
      Exif.Read (Ada.Command_Line.Argument(1));
    when others =>
      Put_Line ("Too many arguments");
    end case;
  exception
  when Exif.File_Not_Found =>
    Put_Line ("File not found");
  when Item: others =>
    Put_Line (Exceptions.Information_Of(Item));
  end Start;

end Control;
