-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Finalization;
with Unsigned;

package Serial_Io is

  type Vendor_Id is new Unsigned.Word;

  type Product_Id is new Unsigned.Word;

  type Baudrate is (B110, B300, B600, B1200, B2400, B4800, B9600, B14400, B19200, B38400, B57600, B115200);

  type Parity is (None, Odd, Even, Mark, Space);

  type Stop_Bits is (One, One_And_A_Half, Two);

  type Flow_Control is (None, Cts, Dsr, Cts_And_Dsr);

  type Byte_Size is (Eight_Bit_Bytes, Seven_Bit_Bytes, Six_Bit_Bytes, Five_Bit_Bytes);

  Default_Baudrate      : constant Baudrate := B19200;
  Default_Byte_Size     : constant Byte_Size := Eight_Bit_Bytes;
  Default_Parity        : constant Parity := None;
  Default_Stop_Bits     : constant Stop_Bits := One;
  Default_Flow_Control  : constant Flow_Control := None;
  Default_Flush_Timeout : constant Duration := 0.1;

  Infinite : constant Duration := 0.0;

  type Device is tagged limited private;

  procedure Allocate (The_Device : Device;
                      Vendor     : Vendor_Id;
                      Product    : Product_Id);

  Device_Not_Found  : exception;
  Multiple_Devices  : exception;


  procedure Set (The_Device   : Device;
                 The_Baudrate : Baudrate);

  function Actual_Baudrate (The_Device : Device) return Baudrate;

  function Max_Baudrate (The_Device : Device) return Baudrate;


  procedure Set (The_Device : Device;
                 The_Parity : Parity);

  function Actual_Parity (The_Device : Device) return Parity;


  procedure Set (The_Device    : Device;
                 The_Stop_Bits : Stop_Bits);

  function Actual_Stop_Bits (The_Device : Device) return Stop_Bits;


  procedure Set (The_Device       : Device;
                 The_Flow_Control : Flow_Control);

  function Actual_Flow_Control (The_Device : Device) return Flow_Control;


  procedure Set (The_Device  : Device;
                 The_Timeout : Duration);

  procedure Set_For_Read (The_Device  : Device;
                          The_Timeout : Duration);

  procedure Set_For_Write (The_Device  : Device;
                           The_Timeout : Duration);

  function Read_Timeout (The_Device : Device) return Duration;

  function Write_Timeout (The_Device : Device) return Duration;


  procedure Set (The_Device    : Device;
                 The_Byte_Size : Byte_Size);

  function Actual_Byte_Size (The_Device : Device) return Byte_Size;


  procedure Send (The_Device : Device;
                  The_Item   : String);

  procedure Send (The_Device : Device;
                  The_Item   : Character);

  procedure Send (The_Device : Device;
                  The_Item   : Unsigned.Byte_String);

  procedure Send (The_Device : Device;
                  The_Item   : Unsigned.Byte);


  procedure Receive (The_Device :     Device;
                     The_Item   : out String);

  procedure Receive (The_Device :     Device;
                     The_Item   : out Character);

  procedure Receive (The_Device :     Device;
                     The_Item   : out Unsigned.Byte_String);

  procedure Receive (The_Device :     Device;
                     The_Item   : out Unsigned.Byte);


  function Next_Character (The_Device : Device) return Character;

  function Next_Byte (The_Device : Device) return Unsigned.Byte;


  procedure Flush (The_Device  : Device;
                   The_Timeout : Duration := Default_Flush_Timeout);


  procedure Free (The_Device : Device);

  procedure Close (The_Device : Device);

  Aborted           : exception;
  No_Access         : exception;
  Operation_Failed  : exception;
  Timeout           : exception;
  Illegal_Baudrate  : exception;
  Illegal_Byte_Size : exception;

private

  type Device_Data;

  type Data_Pointer is access Device_Data;

  type Device is new Ada.Finalization.Limited_Controlled with record
    Data : Data_Pointer;
  end record;

  overriding
  procedure Initialize (The_Device : in out Device);

  overriding
  procedure Finalize (The_Device : in out Device);

end Serial_Io;
