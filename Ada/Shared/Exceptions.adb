-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

  function Name_Of (Occurrence : Ada.Exceptions.Exception_Occurrence) return String is
    Message : constant String := Strings.Trimmed (Ada.Exceptions.Exception_Message (Occurrence));
    Name    : constant String := Ada.Exceptions.Exception_Name (Occurrence);
  begin
    if Message = "" then
      return Name;
    else
      return Name & " (" & Message & ')';
    end if;
  end Name_Of;


  function Information_Of (Occurrence : Ada.Exceptions.Exception_Occurrence) return String is
  begin
    declare
      Header    : constant String := "Exception: " & Name_Of (Occurrence) & Ascii.Lf;
      Traceback : constant String := GNAT.Traceback.Symbolic.Symbolic_Traceback (Occurrence);
    begin
      if Strings.Trimmed (Traceback) = "" then
        return Header & Ada.Exceptions.Exception_Information (Occurrence);
      else
        return Header & Traceback;
      end if;
    end;
  end Information_Of;

end Exceptions;