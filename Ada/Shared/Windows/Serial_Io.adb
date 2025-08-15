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
pragma Style_White_Elephant;

with Serial_Io.Usb;
with Serial_Io.Windows;
with Ada.Unchecked_Deallocation;

package body Serial_Io is

------------------------------------------------------------------------------------------------------------------------

  type Serial_Channel is access Serial_Io.Windows.Channel;

  type Device_Data is record
    Port    : Serial_Io.Windows.Port;
    Channel : Serial_Channel;
  end record;


  procedure Initialize (The_Device : in out Device) is
  begin
    The_Device.Data := new Device_Data;
  end Initialize;


  procedure Finalize (The_Device : in out Device) is
    procedure Dispose is new Ada.Unchecked_Deallocation (Device_Data, Data_Pointer);
  begin
    Free (The_Device);
    Dispose (The_Device.Data);
  end Finalize;


  procedure Allocate (The_Device : Device;
                      Vendor     : Vendor_Id;
                      Product    : Product_Id) is
    Ports : constant Serial_Io.Usb.Ports := Serial_Io.Usb.Ports_For (Vid => Vendor, Pid => Product);
  begin
    Free (The_Device);
    if Ports'length = 0 then
      raise Device_Not_Found;
    elsif Ports'length > 1 then
      raise Multiple_Devices;
    end if;
    The_Device.Data.Port := Ports(Ports'first);
    The_Device.Data.Channel := new Serial_Io.Windows.Channel(The_Device.Data.Port);
  end Allocate;


  procedure Set (The_Device   : Device;
                 The_Baudrate : Baudrate) is
  begin
    Windows.Set (The_Baudrate, On => The_Device.Data.Channel.all);
  end Set;


  function Actual_Baudrate (The_Device : Device) return Baudrate is
  begin
    return Windows.Baudrate_Of (The_Device.Data.Channel.all);
  end Actual_Baudrate;


  function Max_Baudrate (The_Device : Device) return Baudrate is
  begin
    return Windows.Max_Baudrate_Of (The_Device.Data.Channel.all);
  end Max_Baudrate;


  procedure Set (The_Device : Device;
                 The_Parity : Parity) is
  begin
    Windows.Set (The_Parity, On => The_Device.Data.Channel.all);
  end Set;


  function Actual_Parity (The_Device : Device) return Parity is
  begin
    return Windows.Parity_Of (The_Device.Data.Channel.all);
  end Actual_Parity;


  procedure Set (The_Device    : Device;
                 The_Stop_Bits : Stop_Bits) is
  begin
    Windows.Set (The_Stop_Bits, On => The_Device.Data.Channel.all);
  end Set;


  function Actual_Stop_Bits (The_Device : Device) return Stop_Bits is
  begin
    return Windows.Stop_Bits_Of (The_Device.Data.Channel.all);
  end Actual_Stop_Bits;


  procedure Set (The_Device       : Device;
                 The_Flow_Control : Flow_Control) is
  begin
    Windows.Set (The_Flow_Control, On => The_Device.Data.Channel.all);
  end Set;


  function Actual_Flow_Control (The_Device : Device) return Flow_Control is
  begin
    return Windows.Flow_Control_Of (The_Device.Data.Channel.all);
  end Actual_Flow_Control;


  procedure Set (The_Device  : Device;
                 The_Timeout : Duration) is
  begin
    Windows.Set (The_Timeout, On => The_Device.Data.Channel.all);
  end Set;


  procedure Set_For_Read (The_Device  : Device;
                          The_Timeout : Duration) is
  begin
    Windows.Set_For_Read (The_Timeout, On => The_Device.Data.Channel.all);
  end Set_For_Read;


  procedure Set_For_Write (The_Device  : Device;
                           The_Timeout : Duration) is
  begin
    Windows.Set_For_Write (The_Timeout, On => The_Device.Data.Channel.all);
  end Set_For_Write;


  function Read_Timeout (The_Device : Device) return Duration is
  begin
    return Windows.Read_Timeout_Of (The_Device.Data.Channel.all);
  end Read_Timeout;


  function Write_Timeout (The_Device : Device) return Duration is
  begin
    return Windows.Write_Timeout_Of (The_Device.Data.Channel.all);
  end Write_Timeout;


  procedure Set (The_Device    : Device;
                 The_Byte_Size : Byte_Size) is
  begin
    Windows.Set (The_Byte_Size, On => The_Device.Data.Channel.all);
  end Set;


  function Actual_Byte_Size (The_Device : Device) return Byte_Size is
  begin
    return Windows.Byte_Size_Of (The_Device.Data.Channel.all);
  end Actual_Byte_Size;


  procedure Send (The_Device : Device;
                  The_Item   : String) is
  begin
    Windows.Send (The_Item, To => The_Device.Data.Channel.all);
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : Character) is
  begin
    Windows.Send (The_Item, To => The_Device.Data.Channel.all);
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : Unsigned.Byte_String) is
  begin
    Windows.Send (The_Item, To => The_Device.Data.Channel.all);
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : Unsigned.Byte) is
  begin
    Windows.Send (The_Item, To => The_Device.Data.Channel.all);
  end Send;


  procedure Receive (The_Device :     Device;
                     The_Item   : out String) is
  begin
    Windows.Receive (The_Item, From => The_Device.Data.Channel.all);
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out Character) is
  begin
    Windows.Receive (The_Item, From => The_Device.Data.Channel.all);
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out Unsigned.Byte_String) is
  begin
    Windows.Receive (The_Item, From => The_Device.Data.Channel.all);
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out Unsigned.Byte) is
  begin
    Windows.Receive (The_Item, From => The_Device.Data.Channel.all);
  end Receive;


  function Next_Character (The_Device : Device) return Character is
  begin
    return Windows.Character_Of (The_Device.Data.Channel.all);
  end Next_Character;


  function Next_Byte (The_Device : Device) return Unsigned.Byte is
  begin
    return Windows.Byte_Of (The_Device.Data.Channel.all);
  end Next_Byte;


  procedure Flush (The_Device  : Device;
                   The_Timeout : Duration := Default_Flush_Timeout) is
  begin
    Windows.Flush (The_Device.Data.Channel.all, The_Timeout);
  end Flush;


  procedure Free (The_Device : Device) is
  begin
    if The_Device.Data.Channel /= null then
      Windows.Free (The_Device.Data.Port);
      The_Device.Data.Channel := null; -- disposed by free;
    end if;
  end Free;


  procedure Close (The_Device  : Device) is
  begin
    Free (The_Device);
  end Close;

end Serial_Io;
