-- *********************************************************************************************************************
-- *                               (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Protected_Storage is

  protected Storage is

    procedure Place (Item : Element);

    function Value return Element;

  private
    The_Element : Element;
  end Storage;


  procedure Set (Item : Element) is
  begin
    Storage.Place (Item);
  end Set;


  function Data return Element is
  begin
    return Storage.Value;
  end Data;


  protected body Storage is

    procedure Place (Item : Element) is
    begin
      The_Element := Item;
    end Place;

    function Value return Element is
    begin
      return The_Element;
    end Value;

  end Storage;

end Protected_Storage;
