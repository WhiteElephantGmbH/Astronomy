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

with Ada.Strings.UTF_Encoding.Wide_Strings;
with System;
with Win32.Winnt;
with Win32.Winerror;

package body Shlobj is

  type Wide_String_Access is access Wide_String(1..4096); -- big egnough to hold any folder path name

  function SH_Get_Known_Folder_Path (Id      : access constant GUID;
                                     Dwflags : Win32.DWORD;
                                     Htoken  : Win32.Winnt.HANDLE;
                                     Pszpath : out Wide_String_Access) return Win32.LRESULT
  with
    Import,
    Convention    => Stdcall,
    External_Name => "SHGetKnownFolderPath";


  function Folder_Path_For (Id : access constant GUID) return String is
    Result : Win32.LRESULT;
    Data   : aliased Wide_String_Access;
    use type Win32.LRESULT;
  begin
    Result := SH_Get_Known_Folder_Path (Id, 0, System.Null_Address, Data);
    if Result /= Win32.Winerror.S_OK then
      raise Constraint_Error;
    end if;
    for The_Index in Data'range loop
      if Data(The_Index) = Wide_Character'first then
        return Ada.Strings.UTF_Encoding.Wide_Strings.Encode (Data(Data'first .. The_Index - 1)) & '\';
      end if;
    end loop;
    raise Program_Error;
  end Folder_Path_For;

end Shlobj;
