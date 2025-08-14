-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Characters.Handling;
with Win32.Windef;
with Win32.Winerror;
with Win32.Winnt;
with System;

package body Serial_Io.Usb is

  pragma Linker_Options ("-lsetupapi");

  Wide_Nul : constant Wide_Character := Wide_Character'first;


  function To_Wide (Item : String) return Wide_String renames Ada.Characters.Handling.To_Wide_String;


  function Ports_For (Vid : Vendor_Id;
                      Pid : Product_Id) return Ports is

    type HKEY is new Win32.LONG_PTR; -- from wtypes.h

    The_Port_Count : Port_Count := 0;
    The_Com_Ports  : Ports(Port_Index'first .. Port_Index'last);

    procedure Handle_Port_Name_For (Device_Registry_Key : HKEY) is

      Value_Name : aliased constant Wide_String := "PortName" & Wide_Nul;

      type Port_Buffer is new Wide_String(1 .. 7); -- COM256 & Wide_Nul
      type Port_Access is access all Port_Buffer;

      Port_Name : aliased Port_Buffer;

      type SIZETYPE is new Win32.DWORD;
      type PSIZETYPE is access all SIZETYPE;

      Port_Size : aliased SIZETYPE := Port_Name'length * 2; -- in bytes

      type VALTYPE is new Win32.DWORD;
      type PVALTYPE is access all VALTYPE;

      Port_Type : aliased VALTYPE := 0;

      type LSTATUS is new Win32.LONG; -- from winreg.h

      -- from winreg.h
      function Reg_Query_Value_Ex (Key        : HKEY;
                                   Value_Name : Win32.LPCWSTR;
                                   Reserved   : Win32.LPDWORD;
                                   Value_Type : PVALTYPE;
                                   Data       : Port_Access;
                                   Data_Size  : PSIZETYPE) return LSTATUS
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "RegQueryValueExW";

      function Reg_Close_Key (Key : HKEY) return LSTATUS
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "RegCloseKey";

    begin
      if Reg_Query_Value_Ex (Key        => Device_Registry_Key,
                             Value_Name => Win32.Addr (Value_Name),
                             Reserved   => null,
                             Value_Type => Port_Type'access,
                             Data       => Port_Name'access,
                             Data_Size  => Port_Size'access) = Win32.Winerror.ERROR_SUCCESS
                                                               and then Port_Type = Win32.Winnt.REG_SZ
      then
        declare
          Data_Length : constant Natural := Natural(Port_Size) / 2; -- in words
        begin
          if Data_Length > 3 and then Port_Name(Port_Name'first .. Port_Name'first + 2) = "COM" then
            declare
              Last_Index   : constant Natural := Port_Name'first + Data_Length - 2; -- before zero terminator
              Number_Image : constant Wide_String := Wide_String(Port_Name(Port_Name'first + 3 .. Last_Index));
            begin
              The_Com_Ports(The_Port_Count + 1) := Port'val(Natural'wide_value(Number_Image) - 1);
              The_Port_Count := The_Port_Count + 1;
            exception
            when others =>
              null; -- not a number
            end;
          end if;
        end;
        if Reg_Close_Key (Device_Registry_Key) /= Win32.Winerror.ERROR_SUCCESS then
          raise Program_Error;
        end if;
      end if;
    end Handle_Port_Name_For;


    type HDEVINFO is new Win32.LONG_PTR; -- from setupapi.h

    type ACCESS_MASK is new Win32.ULONG; -- from wdm.h
    type REGSAM      is new ACCESS_MASK; -- from winreg.h

    type GUID is record
      Unused_Data1 : Win32.ULONG  := 0;
      Unused_Data2 : Win32.USHORT := 0;
      Unused_Data3 : Win32.USHORT := 0;
      Unused_Data4 : String(1..8) := [others => Ascii.Nul];
    end record
    with Convention => C;

    type PGUID is access GUID;

    type SP_DEVINFO_DATA is record
      Cb_Size           : Win32.DWORD;
      Unused_Class_Guid : GUID;
      Unused_Dev_Inst   : Win32.DWORD := 0;
      Unused_Reserved   : Win32.ULONG_PTR := 0;
    end record
    with Convention => C;

    type PSP_DEVINFO_DATA is access all SP_DEVINFO_DATA;

    type DEVPROPTYPE is new Win32.DWORD;

    type PDEVPROPTYPE is access all DEVPROPTYPE;

    -- from setupapi.h
    function Setup_Di_Get_Class_Devs (Guid       : PGUID;
                                      Enumerator : Win32.PCWSTR;
                                      Parent     : Win32.Windef.HWND;
                                      Flags      : Win32.DWORD)
                                      return HDEVINFO
    with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetupDiGetClassDevsW";

    function Setup_Di_Enum_Device_Info (Device_Info_Set  : HDEVINFO;
                                        Member_Index     : Win32.DWORD;
                                        Device_Info_Data : PSP_DEVINFO_DATA) return Win32.BOOL
    with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetupDiEnumDeviceInfo";

    function Setup_Di_Get_Device_Registry_Property (Device_Info_Set        : HDEVINFO;
                                                    Device_Info_Data       : PSP_DEVINFO_DATA;
                                                    Property               : Win32.DWORD;
                                                    Property_Reg_Data_Type : PDEVPROPTYPE;
                                                    Property_Buffer        : System.Address;
                                                    Property_Buffer_Size   : Win32.DWORD;
                                                    Required_Size          : System.Address) return Win32.BOOL
    with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetupDiGetDeviceRegistryPropertyW";

    function Setup_Di_Open_Dev_Reg_Key (Device_Info_Set  : HDEVINFO;
                                        Device_Info_Data : PSP_DEVINFO_DATA;
                                        Scope            : Win32.DWORD;
                                        Profile          : Win32.DWORD;
                                        Key_Type         : Win32.DWORD;
                                        Sam_Desired      : REGSAM) return HKEY
    with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetupDiOpenDevRegKey";

    function Setup_Di_Destroy_Device_Info_List (Device_Info_Set : HDEVINFO) return Win32.BOOL
    with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetupDiDestroyDeviceInfoList";

    INVALID_HANDLE_VALUE : constant := -1;

    -- constants from setupapi.h
    DICS_FLAG_GLOBAL : constant Win32.DWORD := 16#00000001#;
    DIGCF_ALLCLASSES : constant Win32.DWORD := 16#00000004#;
    DIGCF_PRESENT    : constant Win32.DWORD := 16#00000002#;
    DIREG_DEV        : constant Win32.DWORD := 16#00000001#;
    SPDRP_HARDWAREID : constant Win32.DWORD := 16#00000001#;

    Dev_Enum : aliased constant Wide_String := "Usb" & Wide_Nul;

    Device_Info_Set  : HDEVINFO;
    Device_Index     : Win32.DWORD := 0;
    Device_Info_Data : aliased SP_DEVINFO_DATA;
    Property_Type    : aliased DEVPROPTYPE;

    use type Win32.DWORD;
    use type Win32.BOOL;

    Device_Id : constant Wide_String := To_Wide ("USB\VID_" & Hex_Image_Of (Vid) & "&PID_" & Hex_Image_Of (Pid));

    The_Buffer_Size : aliased Win32.DWORD := 0;
    Unused          : Win32.BOOL;

  begin -- Com_Ports_For
	  Device_Info_Set := Setup_Di_Get_Class_Devs (Guid       => null,
                                                Enumerator => Win32.Addr (Dev_Enum),
                                                Parent     => System.Null_Address,
                                                Flags      => DIGCF_ALLCLASSES + DIGCF_PRESENT);
    if Device_Info_Set = INVALID_HANDLE_VALUE then
      raise Program_Error;
    end if;

    Device_Info_Data.Cb_Size := SP_DEVINFO_DATA'size / System.Storage_Unit;

    -- Receive information about an enumerated device
    while Setup_Di_Enum_Device_Info (Device_Info_Set  => Device_Info_Set,
                                     Member_Index     => Device_Index,
                                     Device_Info_Data => Device_Info_Data'access) = Win32.TRUE
    loop
      -- Retrieves a specified Plug and Play device property
      Unused := Setup_Di_Get_Device_Registry_Property (Device_Info_Set        => Device_Info_Set,
                                                       Device_Info_Data       => Device_Info_Data'access,
                                                       Property               => SPDRP_HARDWAREID,
                                                       Property_Reg_Data_Type => Property_Type'access,
                                                       Property_Buffer        => System.Null_Address,
                                                       Property_Buffer_Size   => 0,
                                                       Required_Size          => The_Buffer_Size'address);
      declare
        Buffer              : aliased Wide_String(1 .. Integer(The_Buffer_Size) / 2);
        Device_Registry_Key : HKEY;
      begin
        if Setup_Di_Get_Device_Registry_Property (Device_Info_Set        => Device_Info_Set,
                                                  Device_Info_Data       => Device_Info_Data'access,
                                                  Property               => SPDRP_HARDWAREID,
                                                  Property_Reg_Data_Type => Property_Type'access,
                                                  Property_Buffer        => Buffer'address,
                                                  Property_Buffer_Size   => The_Buffer_Size,
                                                  Required_Size          => System.Null_Address) = Win32.TRUE
        then
          if Buffer'length >= Device_Id'length and then Buffer(1 .. Device_Id'length) = Device_Id then

            -- Get our device registry key
            Device_Registry_Key := Setup_Di_Open_Dev_Reg_Key (Device_Info_Set  => Device_Info_Set,
                                                              Device_Info_Data => Device_Info_Data'access,
                                                              Scope            => DICS_FLAG_GLOBAL,
                                                              Profile          => 0,
                                                              Key_Type         => DIREG_DEV,
                                                              Sam_Desired      => Win32.Winnt.KEY_READ);
            if Device_Registry_Key = INVALID_HANDLE_VALUE then
              raise Program_Error;
            end if;

            Handle_Port_Name_For (Device_Registry_Key);

          end if;
        end if;
      end;
      Device_Index := Device_Index + 1;
    end loop;
    if Setup_Di_Destroy_Device_Info_List (Device_Info_Set) /= Win32.TRUE then
      raise Program_Error;
    end if;
    return The_Com_Ports(The_Com_Ports'first .. The_Port_Count);
  end Ports_For;

end Serial_Io.Usb;
