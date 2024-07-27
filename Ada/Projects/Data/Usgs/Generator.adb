-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Exceptions;
with Generator.Data;

package body Generator is

  procedure Execute is
  begin
    Data.Read;
    Data.Generate;
  exception
  when Error_Occured =>
    null;
  when Item: others =>
    Error (Exceptions.Information_Of (Item));
  end Execute;


  procedure Error (Text : String) is
  begin
    IO.Put_Line ("*** " & Text & (if The_Line_Number = 0 then "" else " at line" & The_Line_Number'image));
    raise Error_Occured;
  end Error;

end Generator;
