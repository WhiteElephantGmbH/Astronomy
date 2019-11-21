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

with GNAT.Traceback.Symbolic;
with Strings;

package body Exceptions is

  function Information_Of (Occurrence : Ada.Exceptions.Exception_Occurrence) return String is
  begin
    declare
      Traceback : constant String := GNAT.Traceback.Symbolic.Symbolic_Traceback (Occurrence);
    begin
      if Strings.Trimmed (Traceback) = "" then
        return Ada.Exceptions.Exception_Information (Occurrence);
      else
        return "Exception: " & Ada.Exceptions.Exception_Name (Occurrence) & Ascii.Lf & Traceback;
      end if;
    end;
  end Information_Of;

end Exceptions;