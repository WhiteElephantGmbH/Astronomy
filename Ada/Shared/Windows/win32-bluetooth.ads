with Win32.Bthdef;
with Win32.Winnt;

package Win32.Bluetooth is

  pragma Linker_Options ("-lbluetoothapis");
  pragma Linker_Options ("-Wl,--enable-stdcall-fixup");

  subtype HANDLE is Winnt.HANDLE;

  type Find_Radio_Params is record
    Size : aliased DWORD := DWORD(Find_Radio_Params'size / 8);
  end record
  with
    Convention => C_Pass_By_Copy;

  subtype Radio_Find_Handle is LPVOID;

  subtype Radio_Handle is HANDLE;

  subtype Anon1529_Rg_Bytes_Array is Interfaces.C.char_array (0 .. 5);

  type Anon1529_Anon1530_Union (Discr : Natural := 0) is record
    case Discr is
    when 0 =>
      Ull_Long : aliased Bthdef.Address;
    when others =>
      Rg_Bytes : aliased Anon1529_Rg_Bytes_Array;
    end case;
  end record
  with
    Convention      => C_Pass_By_Copy,
    Unchecked_Union => Standard.True;

  type Bth_Address is record
    Anon2422 : aliased Anon1529_Anon1530_Union;
  end record
  with
    Convention => C_Pass_By_Copy;

  type Anon1547_SzName_Array is array (0 .. 247) of aliased WCHAR;

  type Radio_Info is record
    Size            : aliased DWORD := DWORD(Radio_Info'size / 8);
    Address         : aliased Bth_Address;
    Name            : aliased Anon1547_SzName_Array;
    Class_Of_Device : aliased ULONG;
    Lmp_Subversion  : aliased USHORT;
    Manufacturer    : aliased USHORT;
   end record
   with
     Convention => C_Pass_By_Copy;


  function Is_Discoverable (The_Handle : HANDLE) return BOOL
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "BluetoothIsDiscoverable";

  function Find_First_Radio (Arg1 : access Find_Radio_Params;
                             Arg2 : access Radio_Handle) return Radio_Find_Handle
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "BluetoothFindFirstRadio";

  function Find_Next_Radio (Arg1 : Radio_Find_Handle;
                            Arg2 : access Radio_Handle) return BOOL
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "BluetoothFindNextRadio";

  function Get_Radio_Info (Arg1 :        Radio_Handle;
                           Arg2 : access Radio_Info) return DWORD
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "BluetoothGetRadioInfo";

  function Find_Radio_Close (Arg1 : Radio_Find_Handle) return BOOL
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "BluetoothFindRadioClose";

end Win32.Bluetooth;
