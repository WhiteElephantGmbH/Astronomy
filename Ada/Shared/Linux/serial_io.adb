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

with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Serial_Io.Usb;
with Standard_C_Interface;
with System;
with Termios_Interface;

package body Serial_Io is

  package C  renames Interfaces.C;
  package CI renames Standard_C_Interface;
  package TI renames Termios_Interface;

  type Device_Data is record
    Fd      : CI.File_Descriptor;
    Tio     : aliased TI.Termios;
    Timeout : Duration := Infinite;
  end record;


  procedure Initialize (The_Device : in out Device) is
  begin
    The_Device.Data := new Device_Data;
  end Initialize;


  procedure Free (The_Data : access Device_Data) is
    use type CI.File_Descriptor;
    use type CI.Return_Code;
  begin
    if The_Data.Fd /= CI.Not_Opened then
      if CI.Close (The_Data.Fd) /= CI.Success then
        The_Data.Fd := CI.Not_Opened;
        raise Operation_Failed with "Close Failed.";
      end if;
    end if;
    The_Data.Fd := CI.Not_Opened;
  end Free;


  procedure Finalize (The_Device : in out Device) is
    procedure Dispose is new Ada.Unchecked_Deallocation (Device_Data, Data_Pointer);
  begin
    Free (The_Device.Data);
    Dispose (The_Device.Data);
  end Finalize;


  function New_Device (Item : String) return CI.File_Descriptor is
    Fd : CI.File_Descriptor;
    use type CI.File_Descriptor;
  begin
    Fd := CI.Open (C.Strings.New_String(Item), CI.Read_Write, 0);
    if Fd = CI.Not_Opened then
      raise No_Access with "Failed to open serial port with Device = " & Item;
    end if;
    return Fd;
  end New_Device;


  procedure Allocate (The_Device : Device;
                      Vendor     : Vendor_Id;
                      Product    : Product_Id) is
  begin
    Free (The_Device.Data);
    declare
      Device_Name : constant String := Usb.Device_Name_For (Vid => Vendor, Pid => Product);
      DD : Device_Data renames The_Device.Data.all;
      use type CI.Return_Code;
      use type TI.Tcflag_T;
    begin
      DD.Fd := New_Device (Device_Name);

      if TI.Tcgetattr (DD.Fd, DD.Tio'access) /= CI.Success then
        raise No_Access with "Tcgetattr failed.";
      end if;

      -- Set raw mode
      DD.Tio.C_Iflag := TI.No_Flags;
      DD.Tio.C_Oflag := TI.No_Flags;
      DD.Tio.C_Lflag := TI.No_Flags;
      DD.Tio.C_Cflag := TI.CS8 + TI.CREAD + TI.CLOCAL;

      DD.Tio.C_Cc (TI.VMIN) := TI.Cc_T'val(0);
      DD.Tio.C_Cc (TI.VTIME) := TI.Cc_T'val(0);

      if TI.Cfsetspeed (DD.Tio'access, TI.B19200) /= CI.Success then
        raise No_Access with "Setting baud rate failed.";
      end if;

      if TI.Tcsetattr (DD.Fd, TI.TCSANOW, DD.Tio'access) /= CI.Success then
        raise No_Access with "Tcsetattr failed.";
      end if;

    end;
  end Allocate;


  procedure Check (The_Device : Device) is
    use type CI.File_Descriptor;
  begin
    if The_Device.Data.Fd = CI.Not_Opened then
      raise No_Access;
    end if;
  end Check;


  procedure Set (The_Device   : Device;
                 The_Baudrate : Baudrate) is
    Speed : TI.Speed_T;
  begin
    case The_Baudrate is
    when B19200 =>
      Speed := TI.B19200;
    when others =>
      raise Illegal_Baudrate;
    end case;
    The_Device.Data.Tio.C_Ispeed := Speed;
    The_Device.Data.Tio.C_Ospeed := Speed;
  end Set;


  function Actual_Baudrate (The_Device : Device) return Baudrate is
  begin
    case The_Device.Data.Tio.C_Ispeed is
    when TI.B19200 =>
      return B19200;
    when others =>
      raise Operation_Failed with "not implemented baudrate.";
    end case;
  end Actual_Baudrate;


  function Max_Baudrate (The_Device : Device) return Baudrate is
  begin
    Check (The_Device);
    return Baudrate'last;
  end Max_Baudrate;


  procedure Set (The_Device : Device;
                 The_Parity : Parity) is
  begin
    Check (The_Device);
    case The_Parity is
    when None =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: Parity " & The_Parity'image;
    end case;
  end Set;


  function Actual_Parity (The_Device : Device) return Parity is
  begin
    Check (The_Device);
    return None;
  end Actual_Parity;


  procedure Set (The_Device    : Device;
                 The_Stop_Bits : Stop_Bits) is
  begin
    Check (The_Device);
    case The_Stop_Bits is
    when One =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: Stop_Bits " & The_Stop_Bits'image;
    end case;
  end Set;


  function Actual_Stop_Bits (The_Device : Device) return Stop_Bits is
  begin
    Check (The_Device);
    return One;
  end Actual_Stop_Bits;


  procedure Set (The_Device       : Device;
                 The_Flow_Control : Flow_Control) is
  begin
    Check (The_Device);
    case The_Flow_Control is
    when None =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: Flow_Control " & The_Flow_Control'image;
    end case;
  end Set;


  function Actual_Flow_Control (The_Device : Device) return Flow_Control is
  begin
    Check (The_Device);
    return None;
  end Actual_Flow_Control;


  procedure Set (The_Device  : Device;
                 The_Timeout : Duration) is
  begin
    The_Device.Set_For_Read (The_Timeout);
  end Set;


  procedure Set_For_Read (The_Device  : Device;
                          The_Timeout : Duration) is
  begin
    Check (The_Device);
    The_Device.Data.Timeout := The_Timeout;
  end Set_For_Read;


  procedure Set_For_Write (The_Device  : Device;
                           The_Timeout : Duration) is
  begin
    Check (The_Device);
    raise Operation_Failed with "Not Supported: Write_Timeout =>" & The_Timeout'image;
  end Set_For_Write;


  function Read_Timeout (The_Device : Device) return Duration is
  begin
    Check (The_Device);
    return The_Device.Data.Timeout;
  end Read_Timeout;


  function Write_Timeout (The_Device : Device) return Duration is
  begin
    Check (The_Device);
    raise Operation_Failed with "Not Supported: Write_Timeout_Of";
    return 0.0;
  end Write_Timeout;


  procedure Set (The_Device    : Device;
                 The_Byte_Size : Byte_Size) is
  begin
    Check (The_Device);
    case The_Byte_Size is
    when Eight_Bit_Bytes =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: The_Byte_Size " & The_Byte_Size'image;
    end case;
  end Set;


  function Actual_Byte_Size (The_Device : Device) return Byte_Size is
  begin
    Check (The_Device);
    return Eight_Bit_Bytes;
  end Actual_Byte_Size;


  procedure Send (From_Address : System.Address;
                  The_Amount   : Natural;
                  To           : Device) is
    use type CI.Return_Count;
  begin
    if CI.Write (To.Data.Fd, From_Address, C.size_t(The_Amount)) /= CI.Return_Count(The_Amount) then
      raise No_Access with "Send failed.";
    end if;
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : String) is
  begin
    Check (The_Device);
    Send (The_Item(The_Item'first)'address, The_Item'length, The_Device);
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : Character) is
  begin
    Check (The_Device);
    The_Device.Send (String'(1 => The_Item));
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : Unsigned.Byte_String) is
  begin
    Check (The_Device);
    Send (The_Item(The_Item'first)'address, The_Item'length, The_Device);
  end Send;


  procedure Send (The_Device : Device;
                  The_Item   : Unsigned.Byte) is
    use type Unsigned.Byte_String;
  begin
    Check (The_Device);
    The_Device.Send (Unsigned.Byte_Null_String & The_Item);
  end Send;


  procedure Receive (To_Address : System.Address;
                     The_Amount : Natural;
                     From       : Device) is

    DD : Data_Pointer renames From.Data;

    Result : CI.Return_Count;
    Tio    : aliased CI.Timeval := (Sec => C.long(DD.Timeout), Usec => 0);
    Fd_Set : aliased CI.Fd_Set := [others => False];

    use type CI.File_Descriptor;
    use type CI.Return_Code;

  begin -- Receive
    Fd_Set(DD.Fd) := True;
    Result := CI.Wait_Select (DD.Fd + CI.Fd_Number(1),
                              Read_Fds => Fd_Set'access,
                              Timeout  => Tio'access);
    if Result = CI.Failed then
      raise No_Access with "Wait_Select failed.";
    elsif Result = 0 then
      raise Timeout;
    else
      declare
        Count : constant CI.Return_Count := CI.Read (DD.Fd, To_Address, C.size_t(The_Amount));
      begin
        if Natural(Count) /= The_Amount then
          raise Operation_Failed with "Receive - Result:" & Result'image;
        end if;
      end;
    end if;
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out String) is
  begin
    Check (The_Device);
    Receive (The_Item(The_Item'first)'address, The_Item'length, The_Device);
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out Character) is
    The_String : aliased String(1..1);
  begin
    Check (The_Device);
    The_Device.Receive (The_String);
    The_Item := The_String(1);
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out Unsigned.Byte_String) is
  begin
    Check (The_Device);
    Receive (The_Item(The_Item'first)'address, The_Item'length, The_Device);
  end Receive;


  procedure Receive (The_Device :     Device;
                     The_Item   : out Unsigned.Byte) is
    The_String : Unsigned.Byte_String(1..1);
  begin
    Check (The_Device);
    The_Device.Receive (The_String);
    The_Item := The_String(1);
  end Receive;


  function Next_Character (The_Device : Device) return Character is
    The_Character : Character;
  begin
    Check (The_Device);
    The_Device.Receive (The_Character);
    return The_Character;
  end Next_Character;


  function Next_Byte (The_Device : Device) return Unsigned.Byte is
    The_Byte : Unsigned.Byte;
  begin
    Check (The_Device);
    The_Device.Receive (The_Byte);
    return The_Byte;
  end Next_Byte;


  procedure Flush (The_Device  : Device;
                   The_Timeout : Duration := Default_Flush_Timeout) is
    Saved_Timeout : constant Duration := The_Device.Read_Timeout;
    The_Character : Character;
  begin
    Check (The_Device);
    The_Device.Set_For_Read (The_Timeout);
    loop
      The_Device.Receive (The_Character);
    end loop;
  exception
  when Timeout =>
    The_Device.Set_For_Read (Saved_Timeout);
  end Flush;


  procedure Close (The_Device : Device) is
  begin
    Free (The_Device.Data);
  end Close;

end Serial_Io;
