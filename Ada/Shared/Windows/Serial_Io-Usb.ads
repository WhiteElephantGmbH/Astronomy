-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

package Serial_Io.Usb is

  type Vendor_Id is new Unsigned.Word;

  type Product_Id is new Unsigned.Word;

  type Port_Count is range 0 .. Port'pos(Port'last) - Port'pos(Port'first) + 1;

  subtype Port_Index is Port_Count range 1 .. Port_Count'last;

  type Ports is array (Port_Index range <>) of Port;

  function Ports_For (Vid : Vendor_Id;
                      Pid : Product_Id) return Ports;

end Serial_Io.Usb;
