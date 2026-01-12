-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Text;

package body Error is

  The_Message : Text.String;

  procedure Set (Item : String) is
  begin
    The_Message := [Item];
  end Set;


  procedure Raise_With (Item       : String;
                        Clear_Rest : Boolean := False) is
    use type Text.String;
  begin
    if Text.Is_Null (The_Message) or Clear_Rest then
      The_Message := [Item];
    else
      The_Message := Item & " (" & The_Message & ")";
    end if;
    raise Occurred;
  end Raise_With;


  function Message return String is
    Image : constant String := The_Message.S;
  begin
    Text.Clear (The_Message);
    return Image;
  end Message;

end Error;
