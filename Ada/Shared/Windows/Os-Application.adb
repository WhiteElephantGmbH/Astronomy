-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with System;
with Text;
with Win32;
with Win32.Winbase;
with Win32.Windef;
with Win32.Winnt;
with Win32.Winver;

package body Os.Application is

  pragma Linker_Options ("-lversion");

  package Base renames Win32.Winbase;
  package Nt   renames Win32.Winnt;

  No_Module_Information    : exception;

  subtype Hmodule is Nt.HANDLE;

  use type Win32.DWORD;

  Hmodule_Length : constant Win32.DWORD := Hmodule'size / 8;


  type Hmodule_Array is array (Positive range <>) of Hmodule;

  type Module_Information is record
    Base_Address       : Win32.LPVOID;
    Module_Size        : Win32.DWORD;
    Entry_Point        : Win32.LPVOID with Unreferenced;
  end record
  with Convention => C;


  pragma Linker_Options ("-lntdll");

  function Is_First_Instance_Of (The_Name : String) return Boolean is
    Unused : Win32.Windef.HWND;
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


  function Current_Process return Nt.HANDLE
  with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "GetCurrentProcess";


  pragma Linker_Options ("-lpsapi");

  function Get_Module_Information (Handle      :        Nt.HANDLE;
                                   Module      :        Hmodule;
                                   Module_Info : access Module_Information;
                                   Cb          :        Win32.DWORD)
    return Win32.BOOL
  with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "GetModuleInformation";


  function Get_Module_Base_Name (Handle   : Nt.HANDLE;
                                 Module   : Hmodule;
                                 Filename : System.Address;
                                 Size     : Win32.DWORD)
    return Win32.BOOL
  with
    Import        => True,
    Convention    => Stdcall,
    External_Name => "GetModuleBaseNameA";


  function Module_From_Address (The_Address : System.Address) return Hmodule is
    The_Count          : aliased Win32.DWORD;
    Unused             :         Win32.BOOL;
    use type Nt.HANDLE;
    use type Win32.BOOL;
    Max_Int_Bit : constant := Standard'address_size - 1;
    type Int is range -2 ** (Max_Int_Bit) .. 2 ** Max_Int_Bit - 1;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Int);
    function Get_Modules_Count (Handle    :        Nt.HANDLE;
                                Hmodules  : access Nt.HANDLE; -- dummy
                                Cb        :        Win32.DWORD;
                                Cb_Needed : access Win32.DWORD)
      return Win32.BOOL
    with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "EnumProcessModules";
  begin
    if Get_Modules_Count (Current_Process,
                          null,
                          0,
                          The_Count'access) /= Win32.FALSE
    then
      declare
        type Modules is new Hmodule_Array(1 .. Positive(The_Count / Hmodule_Length));

        function Enum_Process_Modules (Handle    :        Nt.HANDLE;
                                       Hmodules  : access Modules;
                                       Cb        :        Win32.DWORD;
                                       Cb_Needed : access Win32.DWORD)
          return Win32.BOOL
        with
          Import        => True,
          Convention    => Stdcall,
          External_Name => "EnumProcessModules";

        The_Modules     : aliased Modules;
        Actual_Count    : aliased Win32.DWORD;
        The_Information : aliased Module_Information;
        Info_Size       : constant Win32.DWORD := Module_Information'size / 8;
      begin
        if Enum_Process_Modules (Current_Process,
                                 The_Modules'access,
                                 The_Count,
                                 Actual_Count'access) /= Win32.FALSE
        then
          if Actual_Count /= The_Count then
            raise No_Module_Information;
          end if;
          for The_Module of The_Modules loop
            if Get_Module_Information (Current_Process,
                                       The_Module,
                                       The_Information'access,
                                       Info_Size) /= Win32.FALSE
            then
              if (The_Address >= The_Information.Base_Address) and then
                (Convert(The_Address) <= Convert(The_Information.Base_Address) + Int(The_Information.Module_Size))
              then
                return The_Module;
              end if;
            end if;
          end loop;
        end if;
      end;
    end if;
    raise No_Module_Information;
  end Module_From_Address;


  function Origin_Folder return String is
    Module        : constant Hmodule
                  := Module_From_Address (Origin_Folder'address);
    Size          : Natural;
    Return_String : aliased String (1..300);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.PSTR);
  begin
    Size := Natural (Base.GetModuleFileName (Module,
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


  function Base_Name_Of (The_Module : Hmodule) return String is

    The_Name  : aliased String(1..512); -- name.extension and terminator
    The_Index : Natural;

    use type Win32.BOOL;

  begin
    if Get_Module_Base_Name (Current_Process,
                             The_Module,
                             The_Name'address,
                             The_Name'length) /= Win32.FALSE
    then
      The_Index := Text.Location_Of (Ascii.Nul, The_Name);
      return The_Name (The_Name'first .. The_Index - 1);
    end if;
    return "";
  end Base_Name_Of;


  The_Module  : constant Hmodule := Module_From_Address (Set_Language_Id'address);
  Module_Name : constant String  := Base_Name_Of (The_Module);


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
    return Value_Of ("InternalName", Module_Name);
  end Product_Name;


  function Name_Of (The_Entry : Hmodule) return String is
    Name : constant String := Base_Name_Of (The_Entry);
  begin
    for Index in reverse Name'range loop
      if Name(Index) = '.' then
        return Name (Name'first..Index - 1);
      end if;
    end loop;
    return  Name;
  end Name_Of;


  function Name return String is
  begin
    return Product_Name;
  exception
  when others =>
    declare
      Module : constant Hmodule := Module_From_Address (Name'address);
    begin
      return Name_Of (Module);
    end;
  end Name;

  function Main_Version return String is
  begin
    return Value_Of ("FileVersion", Module_Name);
  exception
  when others =>
    declare
      Version_Numbers : constant Unsigned.Word_String := Unsigned.String_Of (Version);
      Major_Id        : constant String := Text.Trimmed (Version_Numbers(Version_Numbers'first + 3)'img);
      Minor_Id        : constant String := Text.Trimmed (Version_Numbers(Version_Numbers'first + 2)'img);
    begin
      return Major_Id & '.' & Minor_Id;
    end;
  end Main_Version;

  function Version return String is
  begin
    return Value_Of ("FileVersion", Module_Name);
  exception
  when others =>
    declare
      Version_Numbers : constant Unsigned.Word_String := Unsigned.String_Of (Version);
      Major_Id        : constant String := Text.Trimmed (Version_Numbers(Version_Numbers'first + 3)'img);
      Minor_Id        : constant String := Text.Trimmed (Version_Numbers(Version_Numbers'first + 2)'img);
      Variant         : constant String := Text.Trimmed (Version_Numbers(Version_Numbers'first + 1)'img);
      Revision        : constant String := Text.Trimmed (Version_Numbers(Version_Numbers'first)'img);
    begin
      return Major_Id & '.' & Minor_Id & '/' & Variant & '-' & Revision;
    end;
  end Version;


  function Version return Unsigned.Quadword is

    Root_Block : constant String := "\";
    The_Buffer : System.Address;
    The_Size   : Natural;

    type Fixedfileinfoaccess is access Win32.Winver.VS_FIXEDFILEINFO;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Fixedfileinfoaccess);

  begin -- Version
    Get_Value_For (Root_Block, Module_Name, The_Buffer, The_Size);
    return Unsigned.Quadword_Of (Unsigned.Longword(Convert(The_Buffer).dwFileVersionMS),
                                 Unsigned.Longword(Convert(The_Buffer).dwFileVersionLS));
  end Version;

end Os.Application;
