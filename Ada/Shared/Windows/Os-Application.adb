-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with Program;
with System;
with Win32;
with Win32.Winbase;
with Win32.Windef;
with Win32.Winnt;
with Win32.Winver;

package body Os.Application is

  pragma Linker_Options ("-lversion");

  package Base renames Win32.Winbase;
  package C    renames Interfaces.C;
  package Nt   renames Win32.Winnt;

  No_Module_Information    : exception;

  subtype Process_Id is Win32.DWORD;
  subtype Hmodule is Nt.HANDLE;

  type Module_Entry is record
    Entry_Size         : Win32.DWORD;
    The_Module_Id      : Win32.DWORD;
    The_Process_Id     : Process_Id;
    Global_Usage_Count : Win32.DWORD;
    Usage_This_Process : Win32.DWORD;
    Base_Address       : Win32.LPVOID;
    Module_Size        : Win32.DWORD;
    The_Handle         : Hmodule;
    Module_Name        : Win32.CHAR_Array (0..255);
    Module_Filename    : Win32.CHAR_Array (0..Win32.Windef.MAX_PATH);
  end record;
  for Module_Entry use record
    Entry_Size         at   0 range 0 .. 31;
    The_Module_Id      at   4 range 0 .. 31;
    The_Process_Id     at   8 range 0 .. 31;
    Global_Usage_Count at  12 range 0 .. 31;
    Usage_This_Process at  16 range 0 .. 31;
    Base_Address       at  20 range 0 .. 31;
    Module_Size        at  24 range 0 .. 31;
    The_Handle         at  28 range 0 .. 31;
    Module_Name        at  32 range 0 .. 2047;
    Module_Filename    at 288 range 0 .. (Win32.Windef.MAX_PATH + 1) * 8 - 1;
  end record;
  for Module_Entry'size use 4392;
  type Module_Entry_Ptr is access all Module_Entry;


  pragma Linker_Options ("-lntdll");

  function Is_First_Instance_Of (The_Name : String) return Boolean is
    Unused : Win32.Windef.HWND;
    use type Win32.DWORD;
  begin
    Unused := Base.CreateMutex (null, Win32.FALSE, Win32.Addr(The_Name));
    return Base.GetLastError = 0; -- We succeeded in creating the first occurrence of the mutex
  end Is_First_Instance_Of;


  function Is_First_Instance return Boolean is
    Filename_Size     : Natural;
    Max_Filename_Size : constant := Win32.Windef.MAX_PATH;
    The_Filename      : aliased String (1..Max_Filename_Size);
    Start_Position    : Natural;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PSTR);
  begin
    Filename_Size := Natural(Base.GetModuleFileName (System.Null_Address,
                                                     Convert(The_Filename(The_Filename'first)'address),
                                                     Win32.DWORD(Max_Filename_Size)));
    Start_Position := Filename_Size;
    while (Start_Position > 0) and then (The_Filename(Start_Position) /= '\') loop
      Start_Position := Start_Position - 1;  -- look for delimeter
    end loop;
    return Is_First_Instance_Of (The_Filename (Start_Position + 1 .. Filename_Size + 1));
  end Is_First_Instance;


  function Create_Tool_Help32_Snapshot (Flags : Win32.DWORD;
                                        Id    : Process_Id)
           return Nt.HANDLE
  with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "CreateToolhelp32Snapshot";


  function Module32_First (Handle      : Nt.HANDLE;
                           Information : Module_Entry_Ptr)
           return Win32.BOOL with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "Module32First";


  function Module32_Next (Handle      : Nt.HANDLE;
                          Information : Module_Entry_Ptr)
           return Win32.BOOL with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "Module32Next";


  function Module_From_Address (The_Address : System.Address) return Module_Entry is
    The_Handle          : Nt.HANDLE;
    Th32cs_Snap_Module : constant Win32.DWORD := 16#08#;
    The_Information    : aliased Module_Entry;
    Unused             : Win32.BOOL;
    Have_Data          : Boolean := False;
    use type Nt.HANDLE;
    use type Win32.BOOL;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.DWORD);
    use type Win32.DWORD;
  begin
    The_Handle := Create_Tool_Help32_Snapshot (Th32cs_Snap_Module, 0);
    if The_Handle /= Base.INVALID_HANDLE_VALUE then
      The_Information.Entry_Size := Win32.DWORD(Module_Entry'size / 8); -- size in bytes.
      if Module32_First (The_Handle, The_Information'unchecked_access) /= Win32.FALSE then
        loop
          Have_Data := (The_Address >= The_Information.Base_Address) and then
                       (Convert(The_Address) <= Convert(The_Information.Base_Address) + The_Information.Module_Size);
          exit when Have_Data or else Module32_Next (The_Handle, The_Information'unchecked_access) = Win32.FALSE;
        end loop;
      end if;
      Unused := Base.CloseHandle (The_Handle);
    end if;
    if Have_Data then
      return The_Information;
    else
      raise No_Module_Information;
    end if;
  end Module_From_Address;


  function Origin_Folder return String is
    Module        : constant Module_Entry
                  := Module_From_Address (Origin_Folder'address);
    Size          : Natural;
    Return_String : aliased String (1..300);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PSTR);
  begin
    Size := Natural (Base.GetModuleFileName (Module.The_Handle,
                                             Convert(Return_String(Return_String'first)'address),
                                             Return_String'length));
    while (Size > 0) and then Return_String (Size) /= '\' loop
      Size := Size - 1;
    end loop;
    return Return_String (Return_String'first..Return_String'first + Size - 1);
  end Origin_Folder;


  The_Language_Id : Unsigned.Longword := 16#040904E4#; -- US English, Multilingual codepage


  procedure Set_Language_Id (The_Id : Unsigned.Longword) is
  begin
    The_Language_Id := The_Id;
  end Set_Language_Id;


  function Filename_Of (The_Module : Module_Entry) return String is
  begin
    return C.Strings.Value (Win32.To_Chars_Ptr (Win32.PSTR'(Win32.Addr (The_Module.Module_Filename))));
  end Filename_Of;


  The_Module  : constant Module_Entry := Module_From_Address (Set_Language_Id'address);
  Module_Name : constant String := Filename_Of (The_Module);


  procedure Get_Value_For (The_Key       : String;
                           The_Filename  : String;
                           The_Data_Ptr  : out System.Address;
                           The_Data_Size : out Natural) is

    Key           : aliased constant String := The_Key & Ascii.Nul;
    Filename      : aliased constant String := The_Filename & Ascii.Nul;
    The_Handle    : aliased Win32.DWORD;
    The_Buffer    : aliased System.Address;
    The_Length    : aliased Win32.UINT;
    The_Info_Size : Win32.DWORD;

    use type Win32.DWORD;
    use type Win32.BOOL;

  begin
    The_Info_Size := Win32.Winver.GetFileVersionInfoSize (Win32.Addr(Filename), The_Handle'unchecked_access);
    if The_Info_Size = 0 then
      raise No_Information;
    end if;
    declare
      The_Data : Unsigned.Byte_String (1 .. Natural (The_Info_Size));
    begin
      if Win32.Winver.GetFileVersionInfo (Win32.Addr(Filename), The_Handle,
                                          The_Info_Size, The_Data'address) = Win32.FALSE
      then
        raise No_Information;
      end if;
      if Win32.Winver.VerQueryValue (The_Data'address, Win32.Addr(Key),
                                     The_Buffer'unchecked_access, The_Length'unchecked_access) = Win32.FALSE
      then
        raise No_Information;
      end if;
    end;
    The_Data_Ptr := The_Buffer;
    The_Data_Size := Natural (The_Length);
  end Get_Value_For;


  function Value_Of (The_Key      : String;
                     The_Filename : String) return String is
    The_Full_Key : constant String := "\StringFileInfo\" & Unsigned.Hex_Image_Of(The_Language_Id) & "\" & The_Key;
    The_Buffer   : System.Address;
    The_Size     : Natural;
  begin
    Get_Value_For (The_Full_Key, The_Filename, The_Buffer, The_Size);
    declare
      type Stringptr is access String (1..The_Size);
      function Convert is new Ada.Unchecked_Conversion (System.Address, Stringptr);
    begin
      return Convert(The_Buffer)(1 .. The_Size - 1);
    end;
  end Value_Of;


  function Product_Name return String is
  begin
    return Value_Of ("ProductName", Module_Name);
  end Product_Name;


  function Name_Of (The_Entry : Module_Entry) return String is
    The_String : constant String
               := C.Strings.Value (Win32.To_Chars_Ptr (Win32.PSTR'(Win32.Addr (The_Entry.Module_Name))));
  begin
    for Index in reverse The_String'range loop
      if The_String(Index) = '.' then
        return The_String (The_String'first..Index - 1);
      end if;
    end loop;
    return  The_String;
  end Name_Of;


  function Name return String is
  begin
    return Product_Name;
  exception
  when others =>
    declare
      Module : constant Module_Entry := Module_From_Address (Name'address);
    begin
      return Name_Of (Module);
    end;
  end Name;


  function Version return String is
  begin
    return Program.Version;
  end Version;


  function Version return Unsigned.Quadword is
  begin
   return Program.Version;
  end Version;

end Os.Application;
