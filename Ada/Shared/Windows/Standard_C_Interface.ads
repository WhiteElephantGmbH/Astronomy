-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                              Interface to the Standard C library  (Linux simulation)                              *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Interfaces.C;

package Standard_C_Interface is

  package C renames Interfaces.C;

  type File_Descriptor is new C.int;

  GPIO_Edge    : constant := 0; -- simulation
  GPIO_Aborter : constant := 1; -- simulation

  Fd_Set_Size : constant File_Descriptor := 1024;

  subtype Fd_Number is File_Descriptor range 0 .. Fd_Set_Size;
  subtype Fd_Id     is File_Descriptor range 0 .. Fd_Set_Size - 1;

  type Fd_Set is array (Fd_Id) of Boolean
  with
    Pack,
    Size       => Fd_Set_Size,
    Convention => C;

  type Return_Code is new C.int;

  Success : constant Return_Code := 0;
  function Failed return Return_Code is (-1);

  type Return_Count is new C.int;

  subtype Tv is C.long;

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
                         Res   : access Timespec) return Return_Code;

  function Clock_Gettime (Clock : Clock_Id;
                          Tp    : access Timespec) return Return_Code;

  function Clock_Settime (Clock : Clock_Id;
                          Tp    : access constant Timespec) return Return_Code;


  Timed_Out : constant Return_Count := 0;
  function Failed  return Return_Count is (-1);

  type Timeval is record
    Sec  : C.long;
    Usec : C.long;
  end record
  with
    Convention => C;

  function Wait_Select (Nfds       : Fd_Number;
                        Read_Fds   : access Fd_Set;
                        Write_Fds  : access Fd_Set := null;
                        Except_Fds : access Fd_Set := null;
                        Timeout    : access Timeval := null) return Return_Count;
                        
  type Edge is (Rising, Falling);
  
  procedure Set (Expected : Edge);
  
  procedure Signal_Abort;

end Standard_C_Interface;
