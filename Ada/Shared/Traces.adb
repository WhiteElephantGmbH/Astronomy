-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Log;

package body Traces is

  Category : constant Log.Category := Log.Lookup (Id);

  Category_Is_Enabled : constant Boolean := Log.Is_Enabled (Category);


  function Is_Enabled return Boolean is
  begin
    return Category_Is_Enabled;
  end Is_Enabled;


  procedure Write (Message : String) is
  begin
    Log.Write (Id & " - " & Message, Category);
  end Write;


  procedure Warning (Message : String) is
  begin
    Log.Write ("<W> " & Id & " - " & Message); -- category debug
  end Warning;


  procedure Error (Message : String) is
  begin
    Log.Write ("<E> " & Id & " - " & Message); -- category debug
  end Error;


  procedure Termination (Occurrence : Ada.Exceptions.Exception_Occurrence) is
  begin
    Log.Write ("<T> " & Id, Occurrence); -- category debug
  end Termination;

end Traces;
