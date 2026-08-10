-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
-- *                        Interface to the Standard C library  (items are implemented by need)                       *
-- *        Detailed descriptions can be found in man7.org>Linux>man-pages;  --> example: syscalls(2) > read(2)        *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Interfaces.C.Strings;
with System;

package Standard_C_Interface is

  package C renames Interfaces.C;

  type File_Descriptor is new C.int;

  Fd_Set_Size : constant File_Descriptor := 1024;

  subtype Fd_Number is File_Descriptor range 0 .. Fd_Set_Size;
  subtype Fd_Id     is File_Descriptor range 0 .. Fd_Set_Size - 1;

  type Fd_Set is array (Fd_Id) of Boolean
  with
    Pack,
    Size       => Fd_Set_Size,
    Convention => C;

  Standard_Input  : constant File_Descriptor := 0;
  Standard_Output : constant File_Descriptor := 1;
  Standard_Error  : constant File_Descriptor := 2;
  Not_Opened      : constant File_Descriptor := -1;


  type Open_Flags is new C.int;

  Read_Only  : constant Open_Flags := 0;
  Write_Only : constant Open_Flags := 1;
  Read_Write : constant Open_Flags := 2;
  No_CTTY    : constant Open_Flags := 16#100#;
  Non_Block  : constant Open_Flags := 16#800#;
  Cloexec    : constant Open_Flags := 16#80000#;


  type Return_Code is new C.int;

  Success : constant Return_Code := 0;
  function Failed return Return_Code is (-1);

  type Return_Count is new C.int;

  Timed_Out : constant Return_Count := 0;
  function Failed  return Return_Count is (-1);

  type Pipe_Fds is record
    Read_Fd  : File_Descriptor;
    Write_Fd : File_Descriptor;
  end record
  with
      Convention => C;

  subtype Tv is C.long;

  type Timeval is record
    Sec  : Tv;
    Usec : Tv;
  end record
  with
    Convention => C;

  ---------------------
  -- clock_getres(2) --
  ---------------------
  type Timespec is record
    Sec  : Tv;
    Nsec : Tv;
  end record
  with
    Convention => C;

  type Clock_Id is (Realtime, Monotonic) with Convention => C;
  for Clock_Id use (
    Realtime  => 0,
    Monotonic => 1);


  function Clock_Getres (Clock : Clock_Id;
                         Res   : access Timespec) return Return_Code
  with
    Import        => True,
    Convention    => C,
    External_Name => "clock_getres";

  function Clock_Gettime (Clock : Clock_Id;
                          Tp    : access Timespec) return Return_Code
  with
    Import        => True,
    Convention    => C,
    External_Name => "clock_gettime";

  function Clock_Settime (Clock : Clock_Id;
                          Tp    : access constant Timespec) return Return_Code
  with
    Import        => True,
    Convention    => C,
    External_Name => "clock_settime";


  ---------------------------
  -- syscalls(2) > open(2) --
  ---------------------------
  function Open  (Path  : C.Strings.chars_ptr;
                  Flags : Open_Flags;
                  Mode  : C.int) return File_Descriptor
  with
    Import        => True,
    Convention    => C,
    External_Name => "open";


  ----------------------------
  -- syscalls(2) > close(2) --
  ----------------------------
  function Close (Fd : File_Descriptor) return Return_Code
  with
    Import        => True,
    Convention    => C,
    External_Name => "close";


  ---------------------------
  -- syscalls(2) > read(2) --
  ---------------------------
  function Read (Fd    : File_Descriptor;
                 Buf   : System.Address;
                 Count : C.size_t) return Return_Count
  with
    Import        => True,
    Convention    => C,
    External_Name => "read";


  ----------------------------
  -- syscalls(2) > write(2) --
  ----------------------------
  function Write (Fd    : File_Descriptor;
                  Buf   : System.Address;
                  Count : C.size_t) return Return_Count
  with
    Import        => True,
    Convention    => C,
    External_Name => "write";


  -----------------------------
  -- syscalls(2) > select(2) --
  -----------------------------

  function Wait_Select (Nfds       : Fd_Number;
                        Read_Fds   : access Fd_Set;
                        Write_Fds  : access Fd_Set := null;
                        Except_Fds : access Fd_Set := null;
                        Timeout    : access Timeval := null) return Return_Count
  with
    Import        => True,
    Convention    => C,
    External_Name => "select";


  -----------------------
  -- pipe(2) > pipe(2) --
  -----------------------

  function Pipe (Fds : access Pipe_Fds) return Return_Code
  with
    Import        => True,
    Convention    => C,
    External_Name => "pipe";

end Standard_C_Interface;
