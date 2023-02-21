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

with Win32;

package Shlobj is

  subtype UTF_8_String is String;

  type BYTES8 is array (1 .. 8) of Win32.BYTE with Pack;

  type GUID is record
    Data1 : Win32.UINT;
    Data2 : Win32.USHORT;
    Data3 : Win32.USHORT;
    Data4 : BYTES8;
  end record
  with Convention => C;

  Desktop : aliased constant GUID
    := (Data1 => 16#b4bfcc3a#,
        Data2 => 16#db2c#,
        Data3 => 16#424c#,
        Data4 => [16#b0#, 16#29#, 16#7f#, 16#e9#, 16#9a#, 16#87#, 16#c6#, 16#41#]);

  Documents : aliased constant GUID
    := (Data1 => 16#fdd39ad0#,
        Data2 => 16#238f#,
        Data3 => 16#46af#,
        Data4 => [16#ad#, 16#b4#, 16#6c#, 16#85#, 16#48#, 16#03#, 16#69#, 16#c7#]);

  Downloads : aliased constant GUID
    := (Data1 => 16#374de290#,
        Data2 => 16#123f#,
        Data3 => 16#4565#,
        Data4 => [16#91#, 16#64#, 16#39#, 16#c4#, 16#92#, 16#5e#, 16#46#, 16#7b#]);

  Program_Files : aliased constant GUID
    := (Data1 => 16#905e63b6#,
        Data2 => 16#c1bf#,
        Data3 => 16#494e#,
        Data4 => [16#b2#, 16#9c#, 16#65#, 16#b7#, 16#32#, 16#d3#, 16#d2#, 16#1a#]);

  Program_Files_X86 : aliased constant GUID
    := (Data1 => 16#7c5a40ef#,
        Data2 => 16#a0fb#,
        Data3 => 16#4bfc#,
        Data4 => [16#87#, 16#4a#, 16#c0#, 16#f2#, 16#e0#, 16#b9#, 16#fa#, 16#8e#]);

  function Folder_Path_For (Id : access constant GUID) return UTF_8_String;

end Shlobj;
