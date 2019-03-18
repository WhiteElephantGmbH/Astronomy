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

with Log;
with Text;
with Win32.Winbase;
with Win32.Winerror;
with Win32.Winnt;

package body Os.Process is

  package Base renames Win32.Winbase;
  package Nt   renames Win32.Winnt;

  procedure Create (Executable     : String;
                    Parameters     : String := "";
                    Environment    : String := "";
                    Current_Folder : String := "";
                    Std_Input      : Handle := No_Handle;
                    Std_Output     : Handle := No_Handle;
                    Std_Error      : Handle := No_Handle;
                    Console        : Console_Type := Normal) is

    Startup_Info        : aliased Win32.Winbase.STARTUPINFOA;
    Process_Information : aliased Win32.Winbase.PROCESS_INFORMATION;

    Environment_Block  : aliased constant String
                       := Environment & Ascii.Nul & Ascii.Nul; -- An extra null in case caller forgot
    Environment_Ptr    : Win32.LPVOID := System.Null_Address;

    Current_Directory  : aliased constant String := Current_Folder & Ascii.Nul;
    Directory_Ptr      : Win32.LPCSTR := null;

    The_Creation_Flags : Win32.DWORD := 0;
    Inherit_Handles    : Win32.BOOL := Win32.FALSE;
    Unused             : Win32.BOOL;

    use type Win32.BOOL;
    use type Win32.DWORD;
    use type System.Address;
  begin
    Startup_Info.cb             := Win32.DWORD (Win32.Winbase.STARTUPINFOA'size / 8);
    Startup_Info.lpReserved     := null;
    Startup_Info.lpDesktop      := null;
    Startup_Info.lpTitle        := null;
    Startup_Info.dwX            := 0;
    Startup_Info.dwY            := 0;
    Startup_Info.dwXSize        := 0;
    Startup_Info.dwYSize        := 0;
    Startup_Info.dwXCountChars  := 0;
    Startup_Info.dwYCountChars  := 0;
    Startup_Info.dwFillAttribute:= 0;
    Startup_Info.dwFlags        := 0;
    Startup_Info.wShowWindow    := 0;
    Startup_Info.cbReserved2    := 0;
    Startup_Info.lpReserved2    := null;
    Startup_Info.hStdInput      := Nt.HANDLE(Std_Input);
    Startup_Info.hStdOutput     := Nt.HANDLE(Std_Output);
    Startup_Info.hStdError      := Nt.HANDLE(Std_Error);

    if (Std_Input  /= No_Handle) or
       (Std_Output /= No_Handle) or
       (Std_Error  /= No_Handle)
    then
      Startup_Info.dwFlags := Win32.Winbase.STARTF_USESTDHANDLES;
      Inherit_Handles := Win32.TRUE;
      if Std_Input = No_Handle then
        Startup_Info.hStdInput  := Base.GetStdHandle (Base.STD_INPUT_HANDLE);
      end if;
      if Std_Output = No_Handle then
        Startup_Info.hStdOutput := Base.GetStdHandle (Base.STD_OUTPUT_HANDLE);
      end if;
      if Std_Error = No_Handle then
        Startup_Info.hStdError  := Base.GetStdHandle (Base.STD_ERROR_HANDLE);
      end if;
    end if;
    if Environment /= "" then
      Environment_Ptr := Environment_Block(Environment_Block'first)'address;
    end if;
    if Current_Folder /= "" then
      Directory_Ptr := Win32.Addr (Current_Directory);
    end if;
    case Console is
    when None =>
      The_Creation_Flags := The_Creation_Flags + Base.DETACHED_PROCESS;
    when Invisible =>
      The_Creation_Flags := The_Creation_Flags + Base.CREATE_NO_WINDOW;
    when Normal =>
      null;
    end case;
    if Parameters = "" then
      declare
        Executable_Name : aliased constant String := Executable & Ascii.Nul;
      begin
        if Win32.Winbase.CreateProcess (Win32.Addr (Executable_Name),
                                        null,
                                        null,
                                        null,
                                        Inherit_Handles,
                                        The_Creation_Flags,
                                        Environment_Ptr,
                                        Directory_Ptr,
                                        Startup_Info'unchecked_access,
                                        Process_Information'unchecked_access
                                       ) /= Win32.TRUE
        then
          raise Creation_Failure;
        end if;
      end;
    else
      declare
        Executable_With_Parameters : aliased constant String := Executable & " " & Parameters & Ascii.Nul;
      begin
        if Win32.Winbase.CreateProcess (null,
                                        Win32.Addr (Executable_With_Parameters),
                                        null,
                                        null,
                                        Inherit_Handles,
                                        The_Creation_Flags,
                                        Environment_Ptr,
                                        Directory_Ptr,
                                        Startup_Info'unchecked_access,
                                        Process_Information'unchecked_access
                                       ) /= Win32.TRUE
        then
          raise Creation_Failure;
        end if;
      end;
    end if;
    Unused := Base.CloseHandle (Process_Information.hProcess);
    Unused := Base.CloseHandle (Process_Information.hThread);
  end Create;


  function Execution_Of (Executable     : String;
                         Parameters     : String;
                         Environment    : String  := "";
                         Current_Folder : String  := "";
                         Handle_Output  : Boolean := True;
                         Handle_Errors  : Boolean := True) return String
  is
    In_Temp      : aliased Nt.HANDLE;
    Inbound      : aliased Nt.HANDLE;
    Outbound     : aliased Nt.HANDLE;
    Security     : aliased Base.SECURITY_ATTRIBUTES;
    Default_Size : constant Win32.DWORD := 0;
    The_Data     : String (1..1000);
    The_Length   : aliased Win32.DWORD;
    Unused       : Win32.BOOL;
    The_Result   : Text.String;

    use type Win32.BOOL;
    use type Win32.DWORD;

    function Error_Output return Nt.HANDLE is
    begin
      if Handle_Errors then
        return Outbound;
      else
        return System.Null_Address;
      end if;
    end Error_Output;

    function Standard_Output return Nt.HANDLE is
    begin
      if Handle_Output then
        return Outbound;
      else
        return System.Null_Address;
      end if;
    end Standard_Output;

  begin
    Security.nLength             := Win32.DWORD (Base.SECURITY_ATTRIBUTES'size / 8);
    Security.lpSecurityDescriptor:= System.Null_Address;
    Security.bInheritHandle      := Win32.TRUE;
    if Base.CreatePipe (hReadPipe        => In_Temp'unchecked_access,
                        hWritePipe       => Outbound'unchecked_access,
                        lpPipeAttributes => Security'unchecked_access,
                        nSize            => Default_Size) /= Win32.TRUE
    then
      Log.Write ("!!! Process.Createpipe failed");
      raise Execution_Failed;
    end if;
    --
    -- Duplicate handle but make non inheritable so that child can close pipe
    --
    if Base.DuplicateHandle (Base.GetCurrentProcess, In_Temp,
                             Base.GetCurrentProcess, Inbound'unchecked_access,
                             0, -- Desired access ignored if same access
                             Win32.FALSE, -- Non inheritable
                             Nt.DUPLICATE_SAME_ACCESS) /= Win32.TRUE
    then
      Log.Write ("!!! Process.Duplicatehandle failed");
      raise Execution_Failed;
    end if;
    Unused := Base.CloseHandle (In_Temp); -- No longer used

    Create (Executable     => Executable,
            Parameters     => Parameters,
            Environment    => Environment,
            Current_Folder => Current_Folder,
            Std_Error      => Handle(Error_Output),
            Std_Output     => Handle(Standard_Output),
            Console        => Invisible);
    Unused := Base.CloseHandle (Outbound); -- No longer used, child has a copy
    loop
      if Base.ReadFile (Inbound,
                        The_Data'address,
                        The_Data'length,
                        The_Length'unchecked_access,
                        null) /= Win32.TRUE
      then
        exit when Base.GetLastError = Win32.Winerror.ERROR_BROKEN_PIPE;
        Log.Write ("!!! Process.Readfile Error =" & Win32.DWORD'image(Base.GetLastError));
        raise Execution_Failed;
      else
        Text.Append_To (The_Result, The_Data (The_Data'first .. The_Data'first + Natural(The_Length) - 1));
      end if;
    end loop;
    Unused := Base.CloseHandle (Inbound); -- No longer used
    return Text.String_Of(The_Result);

  exception
  when Execution_Failed =>
    raise;
  when Item: others =>
    Log.Write ("!!! Process.Execution_Of", Item);
    Log.Write ("    Last Error =" & Win32.DWORD'image(Base.GetLastError));
    raise Execution_Failed;
  end Execution_Of;


  procedure Set_Priority_Class (Priority : Priority_Class) is
    ABOVE_NORMAL_PRIORITY_CLASS : constant := 16#8000#;  -- definition is missing in Win32.Winbase
    Unused       : Win32.BOOL;
    Process      : constant Nt.HANDLE := Base.GetCurrentProcess;
    The_Priority : Win32.DWORD;
    use type Win32.BOOL;
  begin
    case Priority is
    when Idle =>
      The_Priority := Win32.Winbase.IDLE_PRIORITY_CLASS;
    when Normal =>
      The_Priority := Win32.Winbase.NORMAL_PRIORITY_CLASS;
    when Above_Normal =>
      The_Priority := ABOVE_NORMAL_PRIORITY_CLASS;
    when High =>
      The_Priority := Win32.Winbase.HIGH_PRIORITY_CLASS;
    when Realtime =>
      The_Priority := Win32.Winbase.REALTIME_PRIORITY_CLASS;
    end case;
    if Base.SetPriorityClass (Process, The_Priority) = Win32.FALSE then
      raise Program_Error;
    end if;
  end Set_Priority_Class;

end Os.Process;
