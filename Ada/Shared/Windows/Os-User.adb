-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with System;
with Win32.Winbase;
with ShlObj;

package body Os.User is

  function Name return String is

    type Dword_Access is access all Win32.DWORD;

    Max_Username_Size : constant := 100;
    Username_Size     : aliased Win32.DWORD := Max_Username_Size;
    Name_Size_Access  : constant Dword_Access := Username_Size'access;
    The_Username      : aliased String (1..Max_Username_Size);

    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PSTR);
    function Convert is new Ada.Unchecked_Conversion (Dword_Access, Win32.LPDWORD);

    use type Win32.DWORD;
    use type Win32.BOOL;

  begin -- Name
    if Win32.Winbase.GetUserName (Convert(The_Username(The_Username'first)'address), Convert(Name_Size_Access))
       = Win32.TRUE
    then
      return The_Username (1..Natural(Username_Size) - 1);
    else
      return "";
    end if;
  end Name;


  function Desktop_Folder return String is
  begin
    return ShlObj.Folder_Path_For (ShlObj.Desktop'access);
  end Desktop_Folder;


  function Documents_Folder return String is
  begin
    return ShlObj.Folder_Path_For (ShlObj.Documents'access);
  end Documents_Folder;


  function Downloads_Folder return String is
  begin
    return ShlObj.Folder_Path_For (ShlObj.Downloads'access);
  end Downloads_Folder;

end Os.User;
