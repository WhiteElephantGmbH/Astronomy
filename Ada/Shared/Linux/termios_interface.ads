-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                         Interface to the Termios Library  (items are implemented by need)                         *
-- *         Detailed descriptions can be found in man7.org>Linux>man-pages;  --> example: termios(3) > Tcgetattr      *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Interfaces.C;
with Standard_C_Interface;

package Termios_Interface is

  package C renames Interfaces.C;
  package I renames Standard_C_Interface;

  type Speed_T  is new C.unsigned;
  type Tcflag_T is new C.unsigned;
  type Cc_T     is new C.char;
  type Cc_Array is array (0 .. 31) of Cc_T;

  TCSANOW : constant := 0;

  -- Flags for 8N1
  CS8    : constant Tcflag_T := 16#00000030#;
  CREAD  : constant Tcflag_T := 16#00000800#;
  CLOCAL : constant Tcflag_T := 16#00008000#;

  -- Baud rates
  B19200 : constant Speed_T := 14;

  VMIN  : constant := 6; -- index for minimum number of read bytes
  VTIME : constant := 5; -- index for interbyte timeout in 1/10s

  No_Flags : constant Tcflag_T := 0;

  type Termios is record
    C_Iflag  : Tcflag_T;
    C_Oflag  : Tcflag_T;
    C_Cflag  : Tcflag_T;
    C_Lflag  : Tcflag_T;
    C_Line   : Cc_T;
    C_Cc     : Cc_Array;
    C_Ispeed : Speed_T;
    C_Ospeed : Speed_T;
  end record
    with Convention => C;


  ----------------------------
  -- termios(3) > Tcgetattr --
  ----------------------------
  function Tcgetattr (Fd        : I.File_Descriptor;
                      Termios_P : access Termios) return I.Return_Code
    with
      Import        => True,
      Convention    => C,
      External_Name => "tcgetattr";


  ----------------------------
  -- termios(3) > Tcgetattr --
  ----------------------------
  function Tcsetattr (Fd               : I.File_Descriptor;
                      Optional_Actions : C.int;
                      Termios_P        : access Termios) return I.Return_Code
    with
      Import        => True,
      Convention    => C,
      External_Name => "tcsetattr";


  ------------------------------
  -- termios(3) > cfsetispeed --
  ------------------------------
  function Cfsetispeed (Termios_P : access Termios;
                        Speed     : Speed_T) return I.Return_Code
    with
      Import        => True,
      Convention    => C,
      External_Name => "cfsetispeed";


  ------------------------------
  -- termios(3) > cfsetospeed --
  ------------------------------
  function Cfsetospeed (Termios_P : access Termios;
                        Speed     : Speed_T) return I.Return_Code
    with
      Import        => True,
      Convention    => C,
      External_Name => "cfsetospeed";


  -----------------------------
  -- termios(3) > cfsetspeed --
  -----------------------------
  function Cfsetspeed (Termios_P : access Termios;
                       Speed     : Speed_T) return I.Return_Code
    with
      Import        => True,
      Convention    => C,
      External_Name => "cfsetspeed";

end Termios_Interface;
