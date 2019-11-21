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

with Ada.Unchecked_Conversion;
with GNAT.Sockets;
with System;
with Win32.Winbase;
with Win32.Winuser;

package body Os is

  function Computer_Name return String is
  begin
    return GNAT.Sockets.Host_Name;
  end Computer_Name;


  function User_Name return String is
    type Dword_Access is access all Win32.DWORD;
    Max_Username_Size : constant := 100;
    Username_Size     : aliased Win32.DWORD := Max_Username_Size;
    Name_Size_Access  : constant Dword_Access := Username_Size'access;
    The_Username      : aliased String (1..Max_Username_Size);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PSTR);
    function Convert is new Ada.Unchecked_Conversion (Dword_Access, Win32.LPDWORD);
  use type Win32.DWORD;
  use type Win32.BOOL;
  begin
    if Win32.Winbase.GetUserName (Convert(The_Username(The_Username'first)'address), Convert(Name_Size_Access))
       = Win32.TRUE
    then
      return The_Username (1..Natural(Username_Size) - 1);
    else
      return "";
    end if;
  end User_Name;


  function Is_Shutting_Down return Boolean is
    use type Win32.INT;
  begin
    return Win32.Winuser.GetSystemMetrics (Win32.Winuser.SM_SHUTTINGDOWN) /= 0;
  end Is_Shutting_Down;


  function Family return Family_Name is
  begin
    return Windows;
  end Family;


  function Is_Linux return Boolean is
  begin
    return False;
  end Is_Linux;


  function Is_Osx return Boolean is
  begin
    return False;
  end Is_Osx;


  function Is_Windows return Boolean is
  begin
    return True;
  end Is_Windows;


end Os;
