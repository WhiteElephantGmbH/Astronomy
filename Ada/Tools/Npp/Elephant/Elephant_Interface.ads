-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Interfaces.C;
with System;

package Elephant_Interface is

  Alignment : constant := Standard'address_size / System.Storage_Unit;

  type    WPARAM   is mod 2 ** Standard'address_size;
  type    LONG_PTR is range -(2 ** (Standard'address_size - 1)) .. (2 ** (Standard'address_size - 1) - 1);
  subtype LPARAM   is LONG_PTR;
  subtype LRESULT  is LONG_PTR;

  subtype BOOL is Interfaces.C.int;
  subtype INT  is Interfaces.C.int;
  subtype UINT is Interfaces.C.unsigned;

  type Info is record
    Npp_Handle              : System.Address;
    Scintilla_Main_Handle   : System.Address;
    Scintilla_Second_Handle : System.Address;
  end record
  with
    Alignment  => Alignment,
    Convention => C_Pass_By_Copy,
    Pack       => True;

  function Get_Name return System.Address
  with
    Export        => True,
    Convention    => C,
    External_Name => "getName";

  function Is_Unicode return BOOL
  with
    Export,
    Convention    => C,
    External_Name => "isUnicode";

  procedure Set_Info (Data : Info)
  with
    Export,
    Convention     => C,
    External_Name  => "setInfo";

  function Get_Funcs_Array (Count : access INT) return System.Address
  with
    Export,
    Convention    => C,
    External_Name => "getFuncsArray";

  procedure Be_Notified (Notify : System.Address)
  with
    Export,
    Convention    => C,
    External_Name => "beNotified";

  function Message_Proc (Unused_Message : UINT;
                         Unused_Wpara   : WPARAM;
                         Unused_Lpara   : LPARAM) return LRESULT
  with
    Export,
    Convention    => C,
    External_Name => "messageProc";

end Elephant_Interface;
