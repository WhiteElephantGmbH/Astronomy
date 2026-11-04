-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Log;
with System;
with System.Storage_Elements;
with Win32;
with Win32.Windef;

package body Os.Shell is

  pragma Linker_Options ("-lshell32");

  function Shell_Execute (Hwnd       : Win32.Windef.HWND;
                          Operation  : Win32.LPCSTR;
                          File       : Win32.LPCSTR;
                          Parameters : Win32.LPSTR;
                          Directory  : Win32.LPCSTR;
                          Show_Cmd   : Win32.INT) return Win32.Windef.HINSTANCE with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "ShellExecuteA";


  procedure Execute (File       : String;
                     Operation  : String       := "Open";
                     Directory  : String       := "";
                     Parameters : String       := "";
                     Show_Cmd   : Show_Command := Normal) is

    The_Operation      : constant String := Operation & Ascii.Nul;
    The_File           : constant String := File & Ascii.Nul;
    The_Parameters     : constant String := Parameters & Ascii.Nul;
    Parameters_Pointer : Win32.PCHAR;
    The_Directory      : constant String := Directory & Ascii.Nul;
    Directory_Pointer  : Win32.PCCH;
    Result             : Win32.Windef.HINSTANCE;
    Value              : System.Storage_Elements.Integer_Address;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PCHAR);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PCCH);
  begin
    if Parameters = "" then
      Parameters_Pointer := Convert (System.Null_Address);
    else
      Parameters_Pointer := Win32.Addr (The_Parameters);
    end if;
    if Directory = "" then
      Directory_Pointer := Convert (System.Null_Address);
    else
      Directory_Pointer := Win32.Addr (The_Directory);
    end if;
    Log.Write ("Start ShellExecute for " & File);
    Result := Shell_Execute (System.Null_Address,
                             Win32.Addr (The_Operation),
                             Win32.Addr (The_File),
                             Parameters_Pointer,
                             Directory_Pointer,
                             Show_Command'pos (Show_Cmd));
    Value := System.Storage_Elements.To_Integer (Result);
    Log.Write ("ShellExecute started, result is " & Value'img);
  end Execute;

end Os.Shell;
