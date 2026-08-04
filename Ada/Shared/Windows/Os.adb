-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with GNAT.Sockets;
with Win32.Winbase;
with Win32.Winuser;

package body Os is

  function Computer_Name return String is
  begin
    return GNAT.Sockets.Host_Name;
  end Computer_Name;


  function Hex_Digit_Of (The_Nibble : Win32.DWORD) return Character is
    use type Win32.DWORD;
  begin
    if The_Nibble < 10 then
      return Character'val (Character'pos('0') + The_Nibble);
    elsif  The_Nibble < 16 then
      return Character'val (Character'pos('A') + The_Nibble - 10);
    else
      return '?';
    end if;
  end Hex_Digit_Of;


  function Hex_Image_Of (The_Dword : Win32.DWORD) return String is
    The_Result : String (1..8) := (others => '0');
    The_Value  : Win32.DWORD := The_Dword;
    use type Win32.DWORD;
  begin
    for Character of reverse The_Result loop
      exit when The_Value = 0;
      Character := Hex_Digit_Of (The_Value mod 16);
      The_Value := The_Value / 16;
    end loop;
    return The_Result;
  end Hex_Image_Of;


  function Thread_Id return String is
  begin
    return Hex_Image_Of (Win32.Winbase.GetCurrentThreadId);
  end Thread_Id;


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
