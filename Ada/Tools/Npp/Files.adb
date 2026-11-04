-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Directory;
with File;
with Interfaces.C;
with Os;
with System;
with Win32.Winbase;
with Win32.Winerror;
with Win32.Winnetwk;
with Win32.Winnt;
with Win32.Winreg;

package body Files is

  pragma Linker_Options ("-lmpr");

  function Is_On_Network (Drive : Character) return Boolean is
    Name   : aliased constant String := Drive & ":" & Ascii.Nul;
    Length : aliased Win32.DWORD := 0;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPSTR);
    use type Win32.DWORD;
  begin
    return Win32.Winnetwk.WNETGETCONNECTIONA (Win32.Addr (Name),
                                                          Convert (System.Null_Address),
                                                          Length'unchecked_access)
           = Win32.Winerror.ERROR_MORE_DATA;
  end Is_On_Network;


  function Unc_Of (Name : String) return String is

    function Path_Of (Value : String) return String is
      Lc_Value : constant String := Ada.Characters.Handling.To_Lower (Value);
      Start    : constant Natural := Ada.Strings.Fixed.Index (Lc_Value, "path=");
    begin
      if Start /= 0 then
        for Index in Start + 5 .. Lc_Value'last loop
          if Lc_Value (Index) = Ascii.Nul then
            return Lc_Value (Start + 5 .. Index - 1);
          end if;
        end loop;
      end if;
      return "";
    end Path_Of;

    Length      : aliased Win32.DWORD := 0;
    Return_Code : Win32.DWORD;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPSTR);

    use type Win32.DWORD;

  begin -- Unc_Of
    if Name'length > 2 and then Name(Name'first + 1) = ':' then
      declare
        Drive : aliased constant String := Name(Name'first .. Name'first + 1) & Ascii.Nul;
      begin
        Return_Code := Win32.Winnetwk.WNETGETCONNECTIONA (Win32.Addr(Drive),
                                                          Convert (System.Null_Address),
                                                          Length'unchecked_access);
        if Return_Code = Win32.Winerror.ERROR_MORE_DATA then
          declare
            Unc_Name : String (1..Natural(Length));
          begin
            Return_Code := Win32.Winnetwk.WNETGETCONNECTIONA (Win32.Addr(Drive),
                                                              Convert (Unc_Name(Unc_Name'first)'address),
                                                              Length'unchecked_access);
            if Return_Code = Win32.Winerror.NOERROR then
              for Index in Unc_Name'range loop
                if Unc_Name(Index) = Ascii.Nul then
                  return Unc_Name (Unc_Name'first .. Index - 1) & Name(Name'first + 2 .. Name'last);
                end if;
              end loop;
            end if;
          end;
        elsif Return_Code = Win32.Winerror.ERROR_NOT_CONNECTED then
          declare
            Lc_Name        : constant String := Ada.Characters.Handling.To_Lower (Name);
            Computers_Name : constant String := Os.Computer_Name;
            Root_Key       : constant String := "SYSTEM\CurrentControlSet\Services\LanmanServer\Shares" & Ascii.Nul;
            The_Key        : aliased Win32.Winreg.HKEY;
            The_Long_Code  : Win32.LONG;
            The_Index      : Win32.DWORD := Win32.DWORD'first;
            The_Name       : aliased String (1..1024);
            pragma Warnings (Off, The_Name);
            The_Name_Size  : aliased Win32.DWORD;
            The_Value_Type : aliased Win32.DWORD;
            The_Value      : aliased String (1..500);
            The_Value_Size : aliased Win32.DWORD;
            function Convert is new Ada.Unchecked_Conversion (System.Address, Win32.LPBYTE);
            use type Win32.LONG;
          begin
            if Computers_Name /= "" and then
               Win32.Winreg.RegOpenKeyA (Win32.Winreg.HKEY_LOCAL_MACHINE,
                                         Win32.Addr(Root_Key),
                                         The_Key'unchecked_access) = Win32.Winerror.NO_ERROR
            then -- Scan registry for shared resources
              loop
                The_Name_Size := The_Name'length;
                The_Value_Size := The_Value'length;
                The_Long_Code := Win32.Winreg.RegEnumValueA (The_Key,
                                                             The_Index,
                                                             Win32.Addr (The_Name),
                                                             The_Name_Size'unchecked_access,
                                                             null,
                                                             The_Value_Type'unchecked_access,
                                                             Convert(The_Value(The_Value'first)'address),
                                                             The_Value_Size'unchecked_access);
                if The_Long_Code = Win32.Winerror.NOERROR and then The_Value_Type = Win32.Winnt.REG_MULTI_SZ then
                  declare
                    Path : constant String := Path_Of (The_Value (The_Value'first ..
                                                                  The_Value'first + Natural(The_Value_Size) - 1));
                  begin
                    if Path'length <= Lc_Name'length and then
                       Path = Lc_Name (Lc_Name'first .. Lc_Name'first + Path'length - 1)
                    then
                      The_Long_Code := Win32.Winreg.RegCloseKey (The_Key);
                      return "\\" & Computers_Name & "\" &
                             The_Name (The_Name'first .. The_Name'first + Natural(The_Name_Size) - 1) &
                             Lc_Name(Lc_Name'first + Path'length .. Lc_Name'last);
                    end if;
                  end;
                end if;
                exit when The_Long_Code = Win32.Winerror.ERROR_NO_MORE_ITEMS;
                The_Index := The_Index + 1;
              end loop;
              The_Long_Code := Win32.Winreg.RegCloseKey (The_Key);
            end if;
          end;
        end if;
      end;
    end if;
    return Name; -- Can't be translated
  end Unc_Of;


  function Original_Name_Of (Filename : String) return String is

    The_Filename  : String := Filename;
    The_First     : Positive := The_Filename'first;
    The_Index     : Positive := The_First;
    Was_Separator : Boolean := False;

    procedure Append_Original is

      function Original_Of (Segment : String) return String is
        use type Win32.Winnt.HANDLE;
        Segment_Name : aliased constant String := Segment & Ascii.Nul;
        The_Data     : aliased Win32.Winbase.WIN32_FIND_DATAA;
        The_Handle   : Win32.Winnt.HANDLE;
        Unused       : Win32.BOOL;
      begin
        The_Handle := Win32.Winbase.FindFirstFileA (Win32.Addr(Segment_Name),
                                                    The_Data'unchecked_access);
        if The_Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
          return "?";
        else
          Unused := Win32.Winbase.FindClose (The_Handle);
          return Interfaces.C.To_Ada (Win32.To_C (The_Data.cFileName));
        end if;
      end Original_Of;

      Original_Name : constant String := Original_Of (Filename(Filename'first .. The_Index - 1));
      Last          : constant Natural := The_First + Original_Name'length;

    begin -- Append_Original
      The_Filename(The_First) := '\';
      The_Filename(The_First + 1 .. Last) := Original_Name;
      The_First := Last + 1;
    end Append_Original;

  begin
    if Filename'length > 3 and then Filename(Filename'first + 1) = ':' then
      The_Filename(1) := Ada.Characters.Handling.To_Upper(Filename(Filename'first));
      The_Filename(2) := ':';
      The_Index := Filename'first + 3;
      The_First := 3;
    end if;
    while The_Index <= Filename'last loop
      case Filename(The_Index) is
      when '.' =>
        null; -- Skip special directores
      when '\' | '/' =>
        if not Was_Separator then
          if Filename(The_First) = '.' then
            The_First := The_Index;
          else
            Append_Original;
            Was_Separator := True;
          end if;
        end if;
      when others =>
        Was_Separator := False;
      end case;
      The_Index := The_Index + 1;
    end loop;
    if not Was_Separator then
      Append_Original;
    end if;
    return The_Filename(The_Filename'first .. The_First - 1);
  end Original_Name_Of;


  function Bits_Are_Set_In (The_Set  : Win32.DWORD;
                            The_Bits : Win32.DWORD) return Boolean is
    use type Win32.DWORD;
  begin
    return (The_Set and The_Bits)= The_Bits;
  end Bits_Are_Set_In;


  subtype Find_Data is Win32.Winbase.WIN32_FIND_DATAA;


  function Directory_Exists (The_Filename : String) return Boolean is

    Filename : aliased constant String := The_Filename & Ascii.Nul;

    The_Handle : Win32.Winnt.HANDLE;
    The_Data   : aliased Find_Data;
    Unused     : Win32.BOOL;

    use type Win32.Winnt.HANDLE;

  begin
    The_Handle := Win32.Winbase.FindFirstFile (Win32.Addr(Filename), The_Data'unchecked_access);
    if The_Handle /= Win32.Winbase.INVALID_HANDLE_VALUE then
      Unused := Win32.Winbase.FindClose(The_Handle);
      if Bits_Are_Set_In (The_Data.dwFileAttributes, Win32.Winnt.FILE_ATTRIBUTE_DIRECTORY) then
         declare
           Name : constant String := Interfaces.C.To_Ada (Win32.To_C (The_Data.cFileName));
         begin
           return Name = The_Filename (The_Filename'last - Name'length + 1 .. The_Filename'last);
         end;
      end if;
    end if;
    return False;
  end Directory_Exists;


  function Exists (The_Filename : String) return Boolean is

    Filename : aliased constant String := The_Filename & Ascii.Nul;

    The_Handle : Win32.Winnt.HANDLE;
    The_Data   : aliased Find_Data;
    Unused     : Win32.BOOL;

    use type Win32.Winnt.HANDLE;

  begin
    The_Handle := Win32.Winbase.FindFirstFile (Win32.Addr(Filename), The_Data'unchecked_access);
    if The_Handle /= Win32.Winbase.INVALID_HANDLE_VALUE then
      Unused := Win32.Winbase.FindClose(The_Handle);
      if not Bits_Are_Set_In (The_Data.dwFileAttributes, Win32.Winnt.FILE_ATTRIBUTE_DIRECTORY) then
         declare
           Name : constant String := Interfaces.C.To_Ada (Win32.To_C (The_Data.cFileName));
         begin
           return Name = The_Filename (The_Filename'last - Name'length + 1 .. The_Filename'last);
         end;
      end if;
    end if;
    return False;
  end Exists;


  function Normalized (Item : String) return String is
    The_Item : String := Text.Trimmed (Item);
  begin
    for Index in The_Item'range loop
      if The_Item(Index) = Other_Separator then
        The_Item(Index) := Separator;
      end if;
    end loop;
    return The_Item;
  end Normalized;


  function Normalized_Folder (Folder : String) return String is
    The_Folder : constant String := Normalized (Folder);
  begin
    if The_Folder'length = 0 then
      return "";
    elsif The_Folder(The_Folder'last) = Separator then
      return The_Folder;
    else
      return The_Folder & Separator;
    end if;
  end Normalized_Folder;


  function Directory_Of (Full_Name : String) return String is
  begin
    for Index in reverse Full_Name'range loop
      if Full_Name(Index) = Separator or Full_Name(Index) = Other_Separator then
        return Full_Name (Full_Name'first .. Index - 1);
      end if;
    end loop;
    return "";
  end Directory_Of;


  function Folder_Of (Full_Name : String) return String is
  begin
    return Directory_Of (Full_Name) & Separator;
  end Folder_Of;


  function Name_Of (Full_Name : String) return String is
  begin
    for Index in reverse Full_Name'range loop
      if Full_Name(Index) = Separator or Full_Name(Index) = Other_Separator then
        return Full_Name (Index + 1 .. Full_Name'last);
      end if;
    end loop;
    return Full_Name;
  end Name_Of;


  function Module_Of (Filename : String) return String is
    Module_Start  : Natural := Filename'first;
    Module_End    : Natural := Filename'last;
    Is_Ada_Source : Boolean := False;
  begin
    for Index in reverse Filename'range loop
      case Filename(Index) is
      when Files.Separator | Files.Other_Separator =>
        Module_Start := Index + 1;
        exit;
      when '.' =>
        Is_Ada_Source := Filename(Index + 1) in 'a' | 'A';
        Module_End := Index - 1;
      when '-' =>
        if not Is_Ada_Source then
          Module_End := Index - 1;
        end if;
      when others =>
        null;
      end case;
    end loop;
    return Filename(Module_Start .. Module_End);
  end Module_Of;


  function Target_Of (Filename : String) return String is
    Module_End : Natural := Filename'last;
  begin
    for Index in reverse Filename'range loop
      case Filename(Index) is
      when Separator | Other_Separator =>
        exit;
      when '.' =>
        if Filename(Index + 1) in 'a' | 'A' then
          return ""; -- no target for ada sources
        end if;
        Module_End := Index - 1;
      when '-' =>
        return Filename(Index + 1 .. Module_End);
      when others =>
        null;
      end case;
    end loop;
    return "";
  end Target_Of;


  function Is_Root_Directory (The_Filename : String) return Boolean is
  begin
    if The_Filename'length <= 3 then
      declare
        Last_Character : constant Character := The_Filename(The_Filename'last);
      begin
        case Last_Character is
        when Separator | Other_Separator | ':' =>
          return Directory.Exists (The_Filename);
        when others =>
          null;
        end case;
      end;
    end if;
    return False;
  end Is_Root_Directory;


  function Path_Of (Folder : String) return String is
  begin
    if Folder(Folder'last) = Separator or Folder(Folder'last) = Other_Separator then
      return Folder(Folder'first .. Folder'last - 1);
    else
      return Folder;
    end if;
  end Path_Of;


  function Folder_Exists (Folder : String) return Boolean is
  begin
    if Is_Root_Directory (Folder) then
      return True;
    end if;
    return Directory_Exists (Path_Of (Folder));
  end Folder_Exists;


  procedure Create_Folder (Folder : String) is
  begin
    Directory.Create (Path_Of (Folder));
  end Create_Folder;


  procedure Delete_Folder (Folder : String) is
  begin
    Directory.Delete (Path_Of (Folder));
  exception
  when others =>
    null;
  end Delete_Folder;


  function Found_Directory_In (Folder : String;
                               Area   : String) return String is
  begin
    return Directory.Found (Name         => Area,
                            In_Directory => Path_Of (Folder));
  exception
  when others =>
    return "";
  end Found_Directory_In;


  function Is_Uptodate (Generated_Filename : String;
                        From_Filename      : String) return Boolean is
  begin
    begin
      if not File.Is_Newer (From_Filename, Generated_Filename) then
        return True;
      end if;
    exception
    when others =>
      null;
    end;
    File.Delete (Generated_Filename);
    return False;
  end Is_Uptodate;


  function Project_Parts_Of (Name          :     String;
                             Area          :     String;
                             The_Directory : out Text.String) return Text.Vector is

    The_Index : Natural := Name'first;
    The_Parts : Text.Vector;

  begin
    for Index in Name'range loop
      if Name(Index) = Separator or Name(Index) = Other_Separator then
        if The_Index /= Name'first then
          declare
            Part : constant String := Name(The_Index + 1 .. Index - 1);
          begin
            if Part = Area then
              The_Directory := [Normalized (Name(Name'first .. Index - 1))];
              The_Index := Name'last;
              for Inner_Index in reverse Index .. Name'last loop
                if Name(Inner_Index) = Separator or Name(Inner_Index) = Other_Separator then
                  declare
                    Inner_Part : constant String := Name(Inner_Index + 1 .. The_Index);
                  begin
                    if Inner_Part /= "" then
                      The_Parts.Prepend (Inner_Part);
                    end if;
                  end;
                  The_Index := Inner_Index - 1;
                end if;
              end loop;
              exit;
            end if;
          end;
        end if;
        The_Index := Index;
      end if;
    end loop;
    return The_Parts;
  end Project_Parts_Of;

end Files;
