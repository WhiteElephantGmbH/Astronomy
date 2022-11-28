-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

package body Doubly_Linked_Lists_Extension is

  function Elements_Of (The_List : Container.List) return Elements is
    The_Elements : Elements(1 .. Natural(The_List.Length));
    The_Last     : Natural := 0;
  begin
    for The_Element of The_List loop
      The_Last := @ + 1;
      The_Elements(The_Last) := The_Element;
    end loop;
    return The_Elements;
  end Elements_Of;


  function List_Of (The_Elements : Elements) return Container.List is
    The_List : Container.List;
  begin
    for The_Element of The_Elements loop
      The_List.Append (The_Element);
    end loop;
    return The_List;
  end List_Of;

end Doubly_Linked_Lists_Extension;
