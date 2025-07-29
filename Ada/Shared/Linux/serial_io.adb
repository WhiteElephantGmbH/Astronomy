-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Standard_C_Interface;
with System;
with Termios_Interface;

package body Serial_Io is

  package C renames Interfaces.C;
  package I renames Standard_C_Interface;
  package T renames Termios_Interface;


  type Data is record
    Fd      : I.File_Descriptor;
    Tio     : aliased T.Termios;
    Timeout : Duration := Infinite;
  end record;

  type Data_Pointers is array (Port) of Data_Pointer;


  function New_Device (Unused : Port) return I.File_Descriptor is
    Fd : I.File_Descriptor;
    use type I.File_Descriptor;
  begin
    Fd := I.Open (C.Strings.New_String("/dev/ttyACM0"), I.Read_Write, 0);
    if Fd = I.Not_Opened then
      raise No_Access with "Failed to open serial port.";
    end if;
    return Fd;
  end New_Device;


  procedure Create (The_Channel : Channel) is
    D : Data renames The_Channel.The_Data.all;
    use type I.Return_Code;
    use type T.Tcflag_T;
  begin
    D.Fd := New_Device (The_Channel.The_Port);

    if T.Tcgetattr (D.Fd, D.Tio'access) /= I.Success then
      raise No_Access with "Tcgetattr failed.";
    end if;

    -- Set raw mode
    D.Tio.C_IFlag := T.No_Flags;
    D.Tio.C_OFlag := T.No_Flags;
    D.Tio.C_LFlag := T.No_Flags;
    D.Tio.C_CFlag := T.CS8 + T.CREAD + T.CLOCAL;

    D.Tio.C_CC (T.VMIN) := T.CC_T'Val(0);
    D.Tio.C_CC (T.VTIME) := T.CC_T'Val(0);

    if T.Cfsetspeed (D.Tio'access, T.B19200) /= I.Success then
      raise No_Access with "Setting baud rate failed.";
    end if;

    if T.Tcsetattr (D.Fd, T.TCSANOW, D.Tio'access) /= I.Success then
      raise No_Access with "Tcsetattr failed.";
    end if;

  end Create;


  procedure Close (The_Data : Data_Pointer) is
    use type I.Return_Code;
  begin
    if I.Close(The_Data.Fd) /= I.Success then
      raise Operation_Failed with "Close Failed.";
    end if;
  end Close;


  protected Port_Allocator is
    procedure Request (The_Channel : in out Channel);
    procedure Check_Aborted (The_Port : Port);
    procedure Release (The_Port : Port);
  private
    The_Data : Data_Pointers := [others => null];
  end Port_Allocator;

  
  protected body Port_Allocator is

    procedure Request (The_Channel : in out Channel) is
    begin
      The_Channel.The_Data := new Data;
      The_Data (The_Channel.The_Port) := The_Channel.The_Data;
    end Request;

    procedure Check_Aborted (The_Port : Port) is
    begin
      if The_Data (The_Port) = null then
        raise Aborted with "Check_Aborted";
      end if;
    end Check_Aborted;

    procedure Release (The_Port : Port) is
      procedure Dispose is new Ada.Unchecked_Deallocation (Data, Data_Pointer);
    begin
      if The_Data (The_Port) /= null then
        Close (The_Data (The_Port));
        Dispose (The_Data (The_Port));
      end if;
    end Release;

  end Port_Allocator;


  function Is_Available (The_Port : Port) return Boolean is
  begin
    declare
      Unused_Channel : Channel(The_Port);  -- UD: raises exception if port not available
    begin
      return True;
    end;
  exception
    when others =>
      return False;
  end Is_Available;


  procedure Free (The_Port : Port) is
  begin
    Port_Allocator.Release (The_Port);
  end Free;


  procedure Set (The_Baudrate : Baudrate;
                 On           : Channel) is
    Speed : T.Speed_T;
  begin
    case The_Baudrate is
    when 19200 =>
      Speed := T.B19200;
    when others =>
      raise Illegal_Baudrate;
    end case;      
    On.The_Data.Tio.C_Ispeed := Speed;
    On.The_Data.Tio.C_Ospeed := Speed;
  end Set;


  function Baudrate_Of (The_Channel : Channel) return Baudrate is
  begin
    case The_Channel.The_Data.Tio.C_Ispeed is
    when T.B19200 =>
      return 19200;
    when others =>
      raise Operation_Failed with "not implemented baudrate.";
    end case;
  end Baudrate_Of;


  function Max_Baudrate_Of (The_Channel : Channel) return Baudrate is
  begin
    return Baudrate'last;
  end Max_Baudrate_Of;


  procedure Set (The_Parity : Parity;
                 On         : Channel) is
  begin
    case The_Parity is
    when None =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: Parity " & The_Parity'image;
    end case;
  end Set;


  function Parity_Of (The_Channel : Channel) return Parity is
  begin
    return None;
  end Parity_Of;


  procedure Set (The_Stop_Bits : Stop_Bits;
                 On            : Channel) is
  begin
    case The_Stop_Bits is
    when One =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: Stop_Bits " & The_Stop_Bits'image;
    end case;
  end Set;


  function Stop_Bits_Of (The_Channel : Channel) return Stop_Bits is
  begin
    return One;
  end Stop_Bits_Of;


  procedure Set (The_Flow_Control : Flow_Control;
                 On               : Channel) is
  begin
    case The_Flow_Control is
    when None =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: Flow_Control " & The_Flow_Control'image;
    end case;
  end Set;


  function Flow_Control_Of (The_Channel : Channel) return Flow_Control is
  begin
    return None;
  end Flow_Control_Of;


  procedure Set_For_Read (The_Timeout : Duration;
                          On          : Channel) is
  begin
    On.The_Data.Timeout := The_Timeout;
  end Set_For_Read;


  procedure Set_For_Write (The_Timeout : Duration;
                           On          : Channel) is
  begin
    raise Operation_Failed with "Not Supported: Write_Timeout";
  end Set_For_Write;


  procedure Set (The_Timeout : Duration;
                 On          : Channel) is
  begin
    Set_For_Read (The_Timeout, On);
  end Set;


  function Read_Timeout_Of (The_Channel : Channel) return Duration is
  begin
    return The_Channel.The_Data.Timeout;
  end Read_Timeout_Of;


  function Write_Timeout_Of (The_Channel : Channel) return Duration is
  begin
    raise Operation_Failed with "Not Supported: Write_Timeout_Of";
    return 0.0;
  end Write_Timeout_Of;


  procedure Set (The_Byte_Size : Byte_Size;
                 On            : Channel) is
  begin
    case The_Byte_Size is
    when Eight_Bit_Bytes =>
      null;
    when others =>
      raise Operation_Failed with "Not_Supported: The_Byte_Size " & The_Byte_Size'image;
    end case;
  end Set;
 

  function Byte_Size_Of (The_Channel : Channel) return Byte_Size is
  begin
    return Eight_Bit_Bytes;
  end Byte_Size_Of;


  procedure Send (From_Address : System.Address;
                  The_Amount   : Natural;
                  To           : Channel) is
    use type I.Return_Count;
  begin
    if I.Write (To.The_Data.Fd, From_Address, C.size_t(The_Amount)) /= I.Return_Count(The_Amount) then
      raise No_Access with "Send failed.";
    end if;
  end Send;


  procedure Send (The_Item : String;
                  To       : Channel) is
  begin
    Send (The_Item(The_Item'first)'address, The_Item'length, To);
  end Send;


  procedure Send (The_Item : Character;
                  To       : Channel) is
  begin
    Send (String'(1 => The_Item), To);
  end Send;


  procedure Send (The_Item : Unsigned.Byte_String;
                  To       : Channel) is
  begin
    Send (The_Item(The_Item'first)'address, The_Item'length, To);
  end Send;


  procedure Send (The_Item : Unsigned.Byte;
                  To       : Channel) is
    use type Unsigned.Byte_String;
  begin
    Send (Unsigned.Byte_Null_String & The_Item, To);
  end Send;


  procedure Receive (To_Address : System.Address;
                     The_Amount : Natural;
                     From       : Channel) is

    D : Data_Pointer renames From.The_Data;                 

    procedure Reset_Device is
    begin
      Close (D);
      D.Fd := New_Device (From.The_Port);
    end Reset_Device;

    Result : I.Return_Count;
    Tio    : aliased I.Timeval := (Sec => C.long(D.Timeout), Usec => 0);
    Fd_Set : aliased I.Fd_Set := [others => False];
    
    use type I.File_Descriptor;
    use type I.Return_Code;

  begin -- Receive
    Fd_Set(D.Fd) := True;
    Result := I.Wait_Select (D.Fd + I.Fd_Number(1),
                             Read_Fds => Fd_Set'access,
                             Timeout  => Tio'access);
    if Result = I.Failed then
      Port_Allocator.Check_Aborted (From.The_Port);
      Reset_Device;
      raise No_Access with "Wait_Select failed.";
    elsif Result = 0 then
      Port_Allocator.Check_Aborted (From.The_Port);
      raise Timeout;
    else
      declare
        Result : constant I.Return_Count := I.Read (D.Fd, To_Address, C.size_t(The_Amount));
      begin
        if Natural(Result) /= The_Amount then
          raise Operation_Failed with "Receive - Result:" & Result'Image;
        end if;
      end;
    end if;
  end Receive;


  procedure Receive (The_Item : out String;
                     From     : Channel) is
  begin
    Receive (The_Item(The_Item'first)'address, The_Item'length, From);
  end Receive;


  procedure Receive (The_Item : out Character;
                     From     : Channel) is
    The_String : aliased String(1..1);
  begin
    Receive (The_String, From);
    The_Item := The_String(1);
  end Receive;


  procedure Receive (The_Item : out Unsigned.Byte_String;
                     From     : Channel) is
  begin
    Receive (The_Item(The_Item'first)'address, The_Item'length, From);
  end Receive;


  procedure Receive (The_Item : out Unsigned.Byte;
                     From     : Channel) is
    The_String : Unsigned.Byte_String(1..1);
  begin
    Receive (The_String, From);
    The_Item := The_String(1);
  end Receive;


  procedure Flush (The_Channel : Channel;
                   The_Timeout : Duration := Default_Flush_Timeout) is

    Saved_Timeout : constant Duration := Read_Timeout_Of (The_Channel);
    The_Character : Character;

  begin
    Set_For_Read (The_Timeout, On => The_Channel);
    loop
      Receive (The_Character, From => The_Channel);
    end loop;
  exception
    when Timeout =>
      Set_For_Read (Saved_Timeout, On => The_Channel);
      return;
  end Flush;


  function Character_Of (The_Channel : Channel) return Character is
    The_Character : Character;
  begin
    Receive (The_Character, The_Channel);
    return The_Character;
  end Character_Of;


  function Byte_Of (The_Channel : Channel) return Unsigned.Byte is
    The_Byte : Unsigned.Byte;
  begin
    Receive (The_Byte, The_Channel);
    return The_Byte;
  end Byte_Of;


  procedure Initialize (The_Channel : in out Channel) is
  begin
    Port_Allocator.Release (The_Channel.The_Port); -- if finalize has not been called
    Port_Allocator.Request (The_Channel);
    Create (The_Channel);
    Set (Default_Baudrate, On => The_Channel);
    Set (Default_Parity, On => The_Channel);
    Set (Default_Stop_Bits, On => The_Channel);
    Set (Default_Flow_Control, On => The_Channel);
    Set (Default_Byte_Size, On => The_Channel);
  end Initialize;


  procedure Finalize (The_Channel : in out Channel) is
  begin
    Port_Allocator.Release (The_Channel.The_Port);
    The_Channel.The_Data := null; -- disposed in Release
  end Finalize;

end Serial_Io;
