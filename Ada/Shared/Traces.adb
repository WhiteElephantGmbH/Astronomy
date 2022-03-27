-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

  Category_Forced_Enabled : Boolean := False;


  function Is_Enabled return Boolean is
  begin
    return Category_Is_Enabled or Category_Forced_Enabled;
  end Is_Enabled;


  procedure Force_Enable is
  begin
    Category_Forced_Enabled := True;
    Log.Write ("<I> " & Id & " - forced enabled on");
  end Force_Enable;


  procedure Normal is
  begin
    if Category_Forced_Enabled then
      Category_Forced_Enabled := False;
      Log.Write ("<I> " & Id & " - forced enabled off");
    end if;
  end Normal;


  procedure Write (Message : String) is
  begin
    if Is_Enabled then
      Log.Write (Id & " - " & Message);
    end if;
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
