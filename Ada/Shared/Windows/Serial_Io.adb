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

with System;
with Win32;
with Win32.Winnt;
with Win32.Winbase;
with Win32.Winerror;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Serial_Io is

------------------------------------------------------------------------------------------------------------------------

  Nt_False : constant := 0;
  Nt_True  : constant := 1;

  function Is_False (The_Item : Win32.Bits1) return Boolean is
    use type Win32.Bits1;
  begin
    return The_Item = Nt_False;
  end Is_False;


  function Is_Not_Valid (The_Item : Win32.Winnt.HANDLE) return Boolean is
    function UC is new Ada.Unchecked_Conversion (Win32.Winnt.HANDLE, Win32.PVOID);
    use type Win32.PVOID;
  begin
    return UC(The_Item) = Win32.Winbase.INVALID_HANDLE_VALUE;
  end Is_Not_Valid;


  function Is_Not_Valid (The_Item : Win32.BOOL) return Boolean is
    use type Win32.BOOL;
  begin
    return The_Item = Win32.FALSE;
  end Is_Not_Valid;


  function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPCVOID);

------------------------------------------------------------------------------------------------------------------------

  No_Timeout : constant := 0;

  Temp_Ptr : constant Win32.LPDWORD := new Win32.DWORD;
  Unused   : Win32.BOOL;

  type Data is record
    The_Device           : Win32.Winnt.HANDLE;
    Read_Timeout         : Win32.DWORD := Win32.Winbase.INFINITE;
    Write_Timeout        : Win32.DWORD := Win32.Winbase.INFINITE;
    The_Control_Block    : aliased Win32.Winbase.DCB;
    The_Comm_Properties  : aliased Win32.Winbase.COMMPROP;
    The_Comm_Timeouts    : aliased Win32.Winbase.COMMTIMEOUTS;
    The_Overlapped_Read  : aliased Win32.Winbase.OVERLAPPED;
    The_Overlapped_Write : aliased Win32.Winbase.OVERLAPPED;
  end record;

  type Data_Pointers is array (Port) of Data_Pointer;


  function Byte_Size_Of (The_Byte_Size : Byte_Size) return Win32.BYTE is
  begin
    case The_Byte_Size is
      when Eight_Bit_Bytes => return 8;
      when Seven_Bit_Bytes => return 7;
      when Six_Bit_Bytes   => return 6;
      when Five_Bit_Bytes  => return 5;
    end case;
  end Byte_Size_Of;


  function New_Device (The_Port : Port) return Win32.Winnt.HANDLE is

    Port_Name : aliased constant String := "\\.\" & The_Port'img & Ascii.Nul;

    use type Unsigned.Longword;

    GENERIC_READ       : constant Unsigned.Longword := Win32.Winnt.GENERIC_READ;
    GENERIC_WRITE      : constant Unsigned.Longword := Win32.Winnt.GENERIC_WRITE;
    GENERIC_READ_WRITE : constant := GENERIC_READ or GENERIC_WRITE;

    FILE_FLAG_OVERLAPPED : constant := Win32.Winbase.FILE_FLAG_OVERLAPPED;

    The_Device : Win32.Winnt.HANDLE;

  begin
    The_Device := Win32.Winbase.CreateFile (Win32.Addr(Port_Name),
                                            GENERIC_READ_WRITE,
                                            0, null,
                                            Win32.Winbase.OPEN_EXISTING,
                                            FILE_FLAG_OVERLAPPED,
                                            System.Null_Address);
    return The_Device;
  end New_Device;


  procedure Create (The_Channel : Channel) is

    function New_Overlapped_Event return Win32.Winbase.OVERLAPPED is
    begin
      return Win32.Winbase.OVERLAPPED'(hEvent => Win32.Winbase.CreateEvent (null,
                                                                            Nt_False,
                                                                            Nt_False,
                                                                            null),

                                       Internal     => 0, -- not used for serial communication
                                       InternalHigh => 0,
                                       Offset       => 0,
                                       OffsetHigh   => 0);
    end New_Overlapped_Event;

    function Control_Block return Win32.Winbase.DCB is
      use type Win32.DWORD;
    begin
      return Win32.Winbase.DCB'(DCBLENGTH         => Win32.Winbase.DCB'size / 8,
                                BaudRate          => Default_Baudrate,
                                Parity            => Win32.Winbase.NOPARITY,
                                ByteSize          => Byte_Size_Of (Default_Byte_Size),
                                StopBits          => Win32.Winbase.ONESTOPBIT,
                                fBinary           => Nt_True,
                                fParity           => Nt_False,
                                fOutxCtsFlow      => Nt_False,
                                fOutxDsrFlow      => Nt_False,
                                fDsrSensitivity   => Nt_False,
                                fTXContinueOnXoff => Nt_False,
                                fOutX             => Nt_False,
                                fInX              => Nt_False,
                                fErrorChar        => Nt_False,
                                fNull             => Nt_False,
                                fAbortOnError     => Nt_False,
                                fDtrControl       => Win32.Winbase.DTR_CONTROL_DISABLE,
                                fRtsControl       => Win32.Winbase.DTR_CONTROL_DISABLE,
                                XonLim            => 0,
                                XoffLim           => 0,
                                XonChar           => Win32.Nul,
                                XoffChar          => Win32.Nul,
                                ErrorChar         => Win32.Nul,
                                EofChar           => Win32.Nul,
                                EvtChar           => Win32.Nul,
                                fDummy2           => 0,
                                wReserved         => 0,
                                wReserved1        => 0);
    end Control_Block;

    function Comm_Properties return Win32.Winbase.COMMPROP is
      subtype Any_Char is Win32.WCHAR_Array (0..Win32.ANYSIZE_ARRAY);
    begin
      return Win32.Winbase.COMMPROP'(wPacketLength       => 0,
                                     wPacketVersion      => 0,
                                     dwServiceMask       => 0,
                                     dwReserved1         => 0,
                                     dwMaxTxQueue        => 0,
                                     dwMaxRxQueue        => 0,
                                     dwMaxBaud           => 0,
                                     dwProvSubType       => 0,
                                     dwProvCapabilities  => 0,
                                     dwSettableParams    => 0,
                                     dwSettableBaud      => 0,
                                     wSettableData       => 0,
                                     wSettableStopParity => 0,
                                     dwCurrentTxQueue    => 0,
                                     dwCurrentRxQueue    => 0,
                                     dwProvSpec1         => 0,
                                     dwProvSpec2         => 0,
                                     wcProvChar          => Any_Char'(others => Win32.WCHAR'first));
    end Comm_Properties;

    function Comm_Timeouts return Win32.Winbase.COMMTIMEOUTS is
    begin
      return Win32.Winbase.COMMTIMEOUTS'(others => No_Timeout);
    end Comm_Timeouts;

  begin
    The_Channel.The_Data.all := Data'(The_Device           => New_Device (The_Channel.The_Port),
                                      Read_Timeout         => Win32.Winbase.INFINITE,
                                      Write_Timeout        => Win32.Winbase.INFINITE,
                                      The_Overlapped_Read  => New_Overlapped_Event,
                                      The_Overlapped_Write => New_Overlapped_Event,
                                      The_Control_Block    => Control_Block,
                                      The_Comm_Properties  => Comm_Properties,
                                      The_Comm_Timeouts    => Comm_Timeouts);
  end Create;


  procedure Close (The_Data : Data_Pointer) is
  begin
    Unused := Win32.Winbase.PurgeComm (The_Data.The_Device, Win32.Winbase.PURGE_RXABORT);
    Unused := Win32.Winbase.PurgeComm (The_Data.The_Device, Win32.Winbase.PURGE_TXABORT);
    Unused := Win32.Winbase.CloseHandle (The_Data.The_Device);
    Unused := Win32.Winbase.CloseHandle (The_Data.The_Overlapped_Read.hEvent);
    Unused := Win32.Winbase.CloseHandle (The_Data.The_Overlapped_Write.hEvent);
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
        raise Aborted;
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
    DCB : Win32.Winbase.DCB renames On.The_Data.The_Control_Block;
  begin
    case The_Baudrate is
    when Win32.Winbase.CBR_110    |
         Win32.Winbase.CBR_300    |
         Win32.Winbase.CBR_600    |
         Win32.Winbase.CBR_1200   |
         Win32.Winbase.CBR_2400   |
         Win32.Winbase.CBR_4800   |
         Win32.Winbase.CBR_9600   |
         Win32.Winbase.CBR_19200  |
         Win32.Winbase.CBR_38400  |
         Win32.Winbase.CBR_56000  |
         Win32.Winbase.CBR_57600  |
         Win32.Winbase.CBR_115200 |
         Win32.Winbase.CBR_128000 |
         Win32.Winbase.CBR_256000 |
         Win32.Winbase.CBR_14400 => null;
    when others =>
      raise Illegal_Baudrate;
    end case;
    DCB.BaudRate := Win32.DWORD(The_Baudrate);
    if Is_Not_Valid (Win32.Winbase.SetCommState (On.The_Data.The_Device, DCB'access)) then
      raise No_Access;
    end if;
  end Set;


  function Baudrate_Of (The_Channel : Channel) return Baudrate is
  begin
    return Baudrate(The_Channel.The_Data.The_Control_Block.BaudRate);
  end Baudrate_Of;


  function Max_Baudrate_Of (The_Channel : Channel) return Baudrate is
    type Longword is mod 2**32;
    The_Settable_Baudrates : constant Longword
      := Longword(The_Channel.The_Data.The_Comm_Properties.dwSettableBaud);
  begin
    if (The_Settable_Baudrates and Longword(Win32.Winbase.BAUD_115200)) /= 0 then
      return Win32.Winbase.CBR_115200;
    elsif (The_Settable_Baudrates and Longword(Win32.Winbase.BAUD_57600)) /= 0 then
      return Win32.Winbase.CBR_57600;
    elsif (The_Settable_Baudrates and Longword(Win32.Winbase.BAUD_38400)) /= 0 then
      return Win32.Winbase.CBR_38400;
    elsif (The_Settable_Baudrates and Longword(Win32.Winbase.BAUD_19200)) /= 0 then
      return Win32.Winbase.CBR_19200;
    elsif (The_Settable_Baudrates and Longword(Win32.Winbase.BAUD_9600)) /= 0 then
      return Win32.Winbase.CBR_9600;
    else
      raise Operation_Failed;
    end if;
  end Max_Baudrate_Of;


  procedure Set (The_Parity : Parity;
                 On         : Channel) is
    DCB : Win32.Winbase.DCB renames On.The_Data.The_Control_Block;
  begin
    if The_Parity = None then
      DCB.fParity := Nt_False;
    else
      DCB.fParity := Nt_True;
    end if;
    case The_Parity is
    when None  => DCB.Parity := Win32.Winbase.NOPARITY;
    when Odd   => DCB.Parity := Win32.Winbase.ODDPARITY;
    when Even  => DCB.Parity := Win32.Winbase.EVENPARITY;
    when Mark  => DCB.Parity := Win32.Winbase.MARKPARITY;
    when Space => DCB.Parity := Win32.Winbase.SPACEPARITY;
    end case;
    if Is_Not_Valid (Win32.Winbase.SetCommState (On.The_Data.The_Device, DCB'access)) then
      Port_Allocator.Check_Aborted (On.The_Port);
      raise No_Access;
    end if;
  end Set;


  function Parity_Of (The_Channel : Channel) return Parity is
    DCB : Win32.Winbase.DCB renames The_Channel.The_Data.The_Control_Block;
  begin
    if Is_False (DCB.fParity) then
      return None;
    else
      case DCB.Parity is
      when Win32.Winbase.NOPARITY    => return None;
      when Win32.Winbase.ODDPARITY   => return Odd;
      when Win32.Winbase.EVENPARITY  => return Even;
      when Win32.Winbase.MARKPARITY  => return Mark;
      when Win32.Winbase.SPACEPARITY => return Space;
      when others
        => raise Operation_Failed;
      end case;
    end if;
  end Parity_Of;


  procedure Set (The_Stop_Bits : Stop_Bits;
                 On            : Channel) is
    DCB : Win32.Winbase.DCB renames On.The_Data.The_Control_Block;
  begin
    case The_Stop_Bits is
    when One            => DCB.StopBits := Win32.Winbase.ONESTOPBIT;
    when One_And_A_Half => DCB.StopBits := Win32.Winbase.ONE5STOPBITS;
    when Two            => DCB.StopBits := Win32.Winbase.TWOSTOPBITS;
    end case;
    if Is_Not_Valid (Win32.Winbase.SetCommState (On.The_Data.The_Device, DCB'access)) then
      Port_Allocator.Check_Aborted (On.The_Port);
      raise No_Access;
    end if;
  end Set;


  function Stop_Bits_Of (The_Channel : Channel) return Stop_Bits is
    DCB : Win32.Winbase.DCB renames The_Channel.The_Data.The_Control_Block;
  begin
    case DCB.StopBits is
    when Win32.Winbase.ONESTOPBIT   => return One;
    when Win32.Winbase.ONE5STOPBITS => return One_And_A_Half;
    when Win32.Winbase.TWOSTOPBITS  => return Two;
    when others
      => raise Operation_Failed;
    end case;
  end Stop_Bits_Of;


  procedure Set (The_Flow_Control : Flow_Control;
                 On               : Channel) is
    DCB : Win32.Winbase.DCB renames On.The_Data.The_Control_Block;
  begin
    case The_Flow_Control is
    when None =>
      DCB.fOutxCtsFlow := Nt_False;
      DCB.fOutxDsrFlow := Nt_False;
    when Cts =>
      DCB.fOutxCtsFlow := Nt_True;
      DCB.fOutxDsrFlow := Nt_False;
    when Dsr =>
      DCB.fOutxCtsFlow := Nt_False;
      DCB.fOutxDsrFlow := Nt_True;
    when Cts_And_Dsr =>
      DCB.fOutxCtsFlow := Nt_True;
      DCB.fOutxDsrFlow := Nt_True;
    end case;
    if Is_Not_Valid (Win32.Winbase.SetCommState (On.The_Data.The_Device, DCB'access)) then
      Port_Allocator.Check_Aborted (On.The_Port);
      raise No_Access;
    end if;
  end Set;


  function Flow_Control_Of (The_Channel : Channel) return Flow_Control is
    DCB : Win32.Winbase.DCB renames The_Channel.The_Data.The_Control_Block;
  begin
    case DCB.fOutxCtsFlow is
    when Nt_False =>
      case DCB.fOutxDsrFlow is
      when Nt_False => return None;
      when Nt_True  => return Dsr;
      end case;
    when Nt_True =>
      case DCB.fOutxDsrFlow is
      when Nt_False => return Cts;
      when Nt_True  => return Cts_And_Dsr;
      end case;
    end case;
  end Flow_Control_Of;


  procedure Set_For_Read (The_Timeout : Duration;
                          On          : Channel) is
  begin
    if The_Timeout = Infinite then
      On.The_Data.Read_Timeout := Win32.Winbase.INFINITE;
    else
      On.The_Data.Read_Timeout := Win32.DWORD(The_Timeout * 1000);
    end if;
  end Set_For_Read;


  procedure Set_For_Write (The_Timeout : Duration;
                           On          : Channel) is
  begin
    if The_Timeout = Infinite then
      On.The_Data.Write_Timeout := Win32.Winbase.INFINITE;
    else
      On.The_Data.Write_Timeout := Win32.DWORD(The_Timeout * 1000);
    end if;
  end Set_For_Write;


  procedure Set (The_Timeout : Duration;
                 On          : Channel) is
  begin
    Set_For_Read (The_Timeout, On);
    Set_For_Write (The_Timeout, On);
  end Set;


  function Read_Timeout_Of (The_Channel : Channel) return Duration is
    use type Win32.DWORD;
  begin
    if The_Channel.The_Data.Read_Timeout = Win32.Winbase.INFINITE then
      return Infinite;
    else
      return Duration(The_Channel.The_Data.Read_Timeout) / 1000.0;
    end if;
  end Read_Timeout_Of;


  function Write_Timeout_Of (The_Channel : Channel) return Duration is
    use type Win32.DWORD;
  begin
    if The_Channel.The_Data.Write_Timeout = Win32.Winbase.INFINITE then
      return Infinite;
    else
      return Duration(The_Channel.The_Data.Write_Timeout) / 1000.0;
    end if;
  end Write_Timeout_Of;


  procedure Set (The_Byte_Size : Byte_Size;
                 On            : Channel) is
    DCB : Win32.Winbase.DCB renames On.The_Data.The_Control_Block;
  begin
    DCB.ByteSize := Byte_Size_Of (The_Byte_Size);
    if Is_Not_Valid (Win32.Winbase.SetCommState (On.The_Data.The_Device, DCB'access)) then
      Port_Allocator.Check_Aborted (On.The_Port);
      raise No_Access;
    end if;
  end Set;


  function Byte_Size_Of (The_Channel : Channel) return Byte_Size is
    DCB : Win32.Winbase.DCB renames The_Channel.The_Data.The_Control_Block;
  begin
    case DCB.ByteSize is
      when 7 => return Seven_Bit_Bytes;
      when 8 => return Eight_Bit_Bytes;
      when others => raise Illegal_Byte_Size;
    end case;
  end Byte_Size_Of;


  procedure Send (From_Address : System.Address;
                  The_Amount   : Natural;
                  To           : Channel) is

    use type Win32.DWORD;

  begin
    if Is_Not_Valid (Win32.Winbase.WriteFile (To.The_Data.The_Device,
                                              Convert(From_Address),
                                              Win32.DWORD(The_Amount),
                                              Temp_Ptr,
                                              To.The_Data.The_Overlapped_Write'access))
    then
      declare
        The_Error : constant Win32.DWORD := Win32.Winbase.GetLastError;
      begin
        if The_Error = Win32.Winerror.ERROR_SUCCESS then
          return;
        elsif The_Error /= Win32.Winerror.ERROR_IO_PENDING then
          Port_Allocator.Check_Aborted (To.The_Port);
          raise No_Access;
        end if;
      end;
      case Win32.Winbase.WaitForSingleObject (To.The_Data.The_Overlapped_Write.hEvent,
                                              To.The_Data.Write_Timeout) is
      when Win32.Winbase.WAIT_OBJECT_0 =>
        if Is_Not_Valid (Win32.Winbase.GetOverlappedResult (To.The_Data.The_Device,
                                                            To.The_Data.The_Overlapped_Write'access,
                                                            Temp_Ptr,
                                                            Nt_False))
        then
          Port_Allocator.Check_Aborted (To.The_Port);
          raise Operation_Failed;
        end if;
      when Win32.Winbase.WAIT_TIMEOUT =>
        Port_Allocator.Check_Aborted (To.The_Port);
        Unused := (Win32.Winbase.GetOverlappedResult (To.The_Data.The_Device,
                                                      To.The_Data.The_Overlapped_Write'access,
                                                      Temp_Ptr,
                                                      Nt_False));
        raise Timeout;
      when others =>
        Port_Allocator.Check_Aborted (To.The_Port);
        raise No_Access;
      end case;
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

    procedure Reset_Device is
    begin
      Unused := Win32.Winbase.PurgeComm (From.The_Data.The_Device, Win32.Winbase.PURGE_RXABORT);
      Unused := Win32.Winbase.CloseHandle (From.The_Data.The_Device);
      From.The_Data.The_Device := New_Device (From.The_Port);
      Unused := Win32.Winbase.SetCommState (From.The_Data.The_Device, From.The_Data.The_Control_Block'access);
      Unused := Win32.Winbase.SetCommTimeouts (From.The_Data.The_Device, From.The_Data.The_Comm_Timeouts'access);
    end Reset_Device;

    use type Win32.DWORD;

  begin
    if Is_Not_Valid (Win32.Winbase.ReadFile (From.The_Data.The_Device,
                                             Convert(To_Address),
                                             Win32.DWORD(The_Amount),
                                             Temp_Ptr,
                                             From.The_Data.The_Overlapped_Read'access))
    then
      declare
        The_Error : constant Win32.DWORD := Win32.Winbase.GetLastError;
      begin
        if The_Error = Win32.Winerror.ERROR_SUCCESS then
          return;
        elsif The_Error /= Win32.Winerror.ERROR_IO_PENDING then
          Port_Allocator.Check_Aborted (From.The_Port);
          Reset_Device;
          raise No_Access;
        end if;
      end;
      case Win32.Winbase.WaitForSingleObject (From.The_Data.The_Overlapped_Read.hEvent,
                                              From.The_Data.Read_Timeout) is
      when Win32.Winbase.WAIT_OBJECT_0 =>
        if Is_Not_Valid (Win32.Winbase.GetOverlappedResult (From.The_Data.The_Device,
                                                            From.The_Data.The_Overlapped_Read'access,
                                                            Temp_Ptr,
                                                            Nt_False))
        then
          Port_Allocator.Check_Aborted (From.The_Port);
          Reset_Device;
          raise Operation_Failed;
        end if;
      when Win32.Winbase.WAIT_TIMEOUT =>
        Port_Allocator.Check_Aborted (From.The_Port);
        Unused := Win32.Winbase.PurgeComm (From.The_Data.The_Device, Win32.Winbase.PURGE_RXABORT);
        raise Timeout;
      when others =>
        Port_Allocator.Check_Aborted (From.The_Port);
        Reset_Device;
        raise No_Access;
      end case;
    end if;
  end Receive;


  procedure Receive (The_Item : out String;
                     From     : Channel) is
  begin
    Receive (The_Item(The_Item'first)'address, The_Item'length, From);
  end Receive;


  procedure Receive (The_Item : out Character;
                     From     : Channel) is
    The_String : String(1..1);
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
    if Is_Not_Valid (The_Channel.The_Data.The_Device) then
      raise No_Access;
    end if;
    if Is_Not_Valid (Win32.Winbase.GetCommProperties (The_Channel.The_Data.The_Device,
                                                      The_Channel.The_Data.The_Comm_Properties'access))
    then
      raise No_Access;
    end if;
    if Is_Not_Valid (Win32.Winbase.SetCommTimeouts (The_Channel.The_Data.The_Device,
                                                    The_Channel.The_Data.The_Comm_Timeouts'access))
    then
      raise No_Access;
    end if;
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
