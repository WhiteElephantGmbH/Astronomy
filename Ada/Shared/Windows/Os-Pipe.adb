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

with Ada.Unchecked_Deallocation;
with Log;
with Win32.Winerror;
with Win32.Winbase;
with Win32.Winnt;
with Semaphore;

package body Os.Pipe is

  type Item_Access is access String;

  type Named_Pipe (Name_Length : Positive;
                   Kind        : Role) is
  record
    Name       : String (1 .. Name_Length);
    Connection : Win32.Winnt.HANDLE := Win32.Winbase.INVALID_HANDLE_VALUE;
    Timeout    : Win32.DWORD;
    Item       : Item_Access;
    Mode       : Access_Mode;
    Size       : Win32.DWORD;
    case Kind is
    when Server =>
      Connect_Overlapped : aliased Win32.Winbase.OVERLAPPED;
      Write_Overlapped   : aliased Win32.Winbase.OVERLAPPED;
      Read_Overlapped    : aliased Win32.Winbase.OVERLAPPED;
      Item_Event         : aliased Win32.Winnt.HANDLE;
      Item_Taken         : Semaphore.Binary;
      Get_Call           : Get_Callback;
    when Client =>
      null;
    end case;
  end record;


  procedure Dispose is new Ada.Unchecked_Deallocation (String, Item_Access);

  procedure Handle_Error (Error : Win32.DWORD) is
    use type Win32.DWORD;
  begin
    case Error is
    when Win32.Winerror.NO_ERROR =>
      null;
    when Win32.Winerror.ERROR_ACCESS_DENIED =>
      raise Access_Denied;
    when Win32.Winerror.ERROR_BROKEN_PIPE =>
      raise Broken;
    when Win32.Winerror.ERROR_FILE_NOT_FOUND =>
      raise No_Server;
    when Win32.Winerror.ERROR_INVALID_NAME =>
      raise Invalid_Name;
    when Win32.Winerror.ERROR_MORE_DATA =>
      raise More_Data;
    when Win32.Winerror.ERROR_NO_DATA =>
      Log.Write ("Os.Pipe - No_Data");
      raise No_Data;
    when Win32.Winerror.ERROR_PIPE_BUSY =>
      raise Name_In_Use;
    when Win32.Winerror.ERROR_BAD_PIPE =>
      raise Bad_Pipe;
    when Win32.Winerror.ERROR_PIPE_NOT_CONNECTED =>
      raise No_Server;
    when others =>
      Log.Write ("Os.Pipe - Unknown_Error:" & Win32.DWORD'image(Error));
      raise Unknown_Error;
    end case;
  end Handle_Error;


  procedure Check (The_Pipe : Handle) is
    use type Win32.BOOL;
  begin
    if The_Pipe = null then
      raise No_Handle;
    end if;
  end Check;


  procedure Check (Result   : Win32.BOOL;
                   The_Pipe : Handle) is
    use type Win32.BOOL;
  begin
    if Result /= Win32.TRUE then
      if The_Pipe.Kind = Server then
        begin
          The_Pipe.Item_Taken.Signal;
        exception
        when others =>
          null; -- in case of termination
        end;
      end if;
      Handle_Error (Win32.Winbase.GetLastError);
    end if;
  end Check;


  function Pipe_Name_Of (The_Pipe : Handle) return String is
  begin
    return "\\.\pipe\" & The_Pipe.Name;
  end Pipe_Name_Of;


  procedure Create_Client_Connection (The_Pipe : Handle) is

    function Desired_Access return Win32.DWORD is
    begin
      case The_Pipe.Mode is
      when Duplex =>
        return Win32.DWORD(Win32.Winnt.GENERIC_READ + Win32.Winnt.GENERIC_WRITE);
      when Inbound =>
        return Win32.DWORD(Win32.Winnt.GENERIC_WRITE);
      when Outbound =>
        return Win32.DWORD(Win32.Winnt.GENERIC_READ + Win32.Winnt.FILE_WRITE_ATTRIBUTES);
      end case;
    end Desired_Access;

    Share_Mode : constant Win32.DWORD := Win32.DWORD(Win32.Winnt.FILE_SHARE_READ +
                                                     Win32.Winnt.FILE_SHARE_WRITE);
    Pipe_Mode  : aliased Win32.DWORD;

    use type System.Address;

    Pipe_Name : aliased constant String := Pipe_Name_Of (The_Pipe) & Ascii.Nul;

  begin
    The_Pipe.Connection := Win32.Winbase.CreateFile
                             (lpFileName            => Win32.Addr (Pipe_Name),
                              dwDesiredAccess       => Desired_Access,
                              dwShareMode           => Share_Mode,
                              lpSecurityAttributes  => null,
                              dwCreationDisposition => Win32.Winbase.OPEN_EXISTING,
                              dwFlagsAndAttributes  => 0,
                              hTemplateFile         => System.Null_Address);
    if The_Pipe.Connection = Win32.Winbase.INVALID_HANDLE_VALUE then
      Handle_Error (Win32.Winbase.GetLastError);
    end if;

    Pipe_Mode := Win32.DWORD(Win32.Winbase.PIPE_READMODE_MESSAGE + Win32.Winbase.PIPE_WAIT);
    Check (Win32.Winbase.SetNamedPipeHandleState
             (hNamedPipe           => The_Pipe.Connection,
              lpMode               => Pipe_Mode'unchecked_access,
              lpMaxCollectionCount => null,
              lpCollectDataTimeout => null), The_Pipe);
  end Create_Client_Connection;


  procedure Create_Server_Connection (The_Pipe : Handle) is

    function Pipe_Access_Mode return Win32.DWORD is
    begin
      case The_Pipe.Mode is
      when Duplex =>
        return Win32.DWORD(Win32.Winbase.PIPE_ACCESS_DUPLEX + Win32.Winbase.FILE_FLAG_OVERLAPPED);
      when Inbound =>
        return Win32.DWORD(Win32.Winbase.PIPE_ACCESS_INBOUND + Win32.Winbase.FILE_FLAG_OVERLAPPED);
      when Outbound =>
        return Win32.DWORD(Win32.Winbase.PIPE_ACCESS_OUTBOUND + Win32.Winbase.FILE_FLAG_OVERLAPPED);
      end case;
    end Pipe_Access_Mode;

    Pipe_Mode : aliased Win32.DWORD;

    use type System.Address;

    Pipe_Name : aliased constant String := Pipe_Name_Of (The_Pipe) & Ascii.Nul;

  begin
    Pipe_Mode := Win32.DWORD(Win32.Winbase.PIPE_TYPE_MESSAGE +
                             Win32.Winbase.PIPE_READMODE_MESSAGE +
                             Win32.Winbase.PIPE_WAIT);

    The_Pipe.Connection := Win32.Winbase.CreateNamedPipe
                             (lpName               => Win32.Addr (Pipe_Name),
                              dwOpenMode           => Pipe_Access_Mode,
                              dwPipeMode           => Pipe_Mode,
                              nMaxInstances        => Win32.DWORD(1),
                              nOutBufferSize       => The_Pipe.Size,
                              nInBufferSize        => The_Pipe.Size,
                              nDefaultTimeOut      => 0,
                              lpSecurityAttributes => null);

    if The_Pipe.Connection = Win32.Winbase.INVALID_HANDLE_VALUE then
      The_Pipe.Item_Taken.Signal;
      Handle_Error (Win32.Winbase.GetLastError);
    end if;
  end Create_Server_Connection;


  procedure Connect (The_Pipe : Handle) is
    use type Win32.BOOL;
    use type Win32.DWORD;
  begin
    if Win32.Winbase.ConnectNamedPipe
             (hNamedPipe   => The_Pipe.Connection,
              lpOverlapped => The_Pipe.Connect_Overlapped'unchecked_access) /= Win32.TRUE
    then
      declare
        Error : constant Win32.DWORD := Win32.Winbase.GetLastError;
      begin
        if Error = Win32.Winerror.ERROR_IO_PENDING then
          case Win32.Winbase.WaitForSingleObject
                 (hHandle        => The_Pipe.Connect_Overlapped.hEvent,
                  dwMilliseconds => The_Pipe.Timeout) is
          when Win32.Winbase.WAIT_TIMEOUT =>
            raise Timeout;
          when Win32.Winbase.WAIT_OBJECT_0 =>
            null;
          when others =>
            The_Pipe.Item_Taken.Signal;
            Handle_Error (Error);
          end case;
        else
          The_Pipe.Item_Taken.Signal;
          Handle_Error (Error);
        end if;
      end;
    end if;
  end Connect;


  procedure Set_Timeout_For (The_Pipe  : Handle;
                             Wait_Time : Duration) is
  begin
    if Wait_Time = Forever then
      The_Pipe.Timeout := Win32.Winbase.NMPWAIT_WAIT_FOREVER;
    else
      The_Pipe.Timeout := Win32.DWORD(Float(Wait_Time) * 1000.0);
    end if;
  end Set_Timeout_For;


  ------------------
  -- Specification
  ------------------

  procedure Open (The_Pipe  : in out Handle;
                  Name      :        String;
                  Kind      :        Role;
                  Mode      :        Access_Mode;
                  Size      :        Natural;
                  Wait_Time :        Duration := Forever;
                  Get_Call  :        Get_Callback := null) is
  begin
    if Kind = Client then
      if (Wait_Time /= Forever) or (Get_Call /= null) then
        raise Not_Server;
      end if;
    end if;
    Close (The_Pipe);
    The_Pipe := new Named_Pipe (Name'length, Kind);
    The_Pipe.Mode := Mode;
    The_Pipe.Name := Name;
    The_Pipe.Size := Win32.DWORD(Size);
    if The_Pipe.Kind = Server then
      The_Pipe.Get_Call := Get_Call;
      Set_Timeout_For (The_Pipe, Wait_Time);
      The_Pipe.Connect_Overlapped.hEvent := Win32.Winbase.CreateEvent
                                              (lpEventAttributes => null,
                                               bManualReset      => Win32.TRUE,
                                               bInitialState     => Win32.TRUE,
                                               lpName            => null);
      The_Pipe.Write_Overlapped.hEvent := Win32.Winbase.CreateEvent
                                            (lpEventAttributes => null,
                                             bManualReset      => Win32.TRUE,
                                             bInitialState     => Win32.TRUE,
                                             lpName            => null);
      The_Pipe.Read_Overlapped.hEvent := Win32.Winbase.CreateEvent
                                           (lpEventAttributes => null,
                                            bManualReset      => Win32.TRUE,
                                            bInitialState     => Win32.TRUE,
                                            lpName            => null);
      The_Pipe.Item_Event := Win32.Winbase.CreateEvent
                               (lpEventAttributes => null,
                                bManualReset      => Win32.FALSE,
                                bInitialState     => Win32.FALSE,
                                lpName            => null);
      Create_Server_Connection (The_Pipe);
      Connect (The_Pipe);
    else
      Create_Client_Connection (The_Pipe);
    end if;
  exception
  when others =>
    Close (The_Pipe);
    raise;
  end Open;


  procedure Close (The_Pipe : in out Handle) is
    use type System.Address;
    procedure Dispose is new Ada.Unchecked_Deallocation (Named_Pipe, Handle);
    Dummy : Win32.BOOL;
  begin
    if The_Pipe /= null then
      begin
        if The_Pipe.Item /= null then
          Dispose (The_Pipe.Item);
        end if;
        Dummy := Win32.Winbase.DisconnectNamedPipe (The_Pipe.Connection);
        if The_Pipe.Kind = Server then
          Dummy := Win32.Winbase.CloseHandle(hObject => The_Pipe.Connect_Overlapped.hEvent);
          Dummy := Win32.Winbase.CloseHandle(hObject => The_Pipe.Read_Overlapped.hEvent);
          Dummy := Win32.Winbase.CloseHandle(hObject => The_Pipe.Write_Overlapped.hEvent);
          Dummy := Win32.Winbase.CloseHandle(hObject => The_Pipe.Item_Event);
        end if;
        if The_Pipe.Connection /= Win32.Winbase.INVALID_HANDLE_VALUE then
          Dummy := Win32.Winbase.CloseHandle(hObject => The_Pipe.Connection);
        end if;
      exception
      when others =>
        null;
      end;
      Dispose (The_Pipe);
    end if;
  end Close;


  procedure Read (From_Pipe :     Handle;
                  Data      :     System.Address;
                  Length    : out Natural;
                  Wait_Time :     Duration := Forever) is

    Count : aliased Win32.DWORD;

    use type Win32.BOOL;

  begin
    Length := 0;
    Check (From_Pipe);
    if From_Pipe.Kind = Client then
      if Wait_Time /= Forever then
        raise Not_Server;
      end if;
      if Win32.Winbase.ReadFile
          (hFile                => From_Pipe.Connection,
           lpBuffer             => Data,
           nNumberOfBytesToRead => From_Pipe.Size,
           lpNumberOfBytesRead  => Count'unchecked_access,
           lpOverlapped         => null) /= Win32.TRUE
      then
        Handle_Error (Win32.Winbase.GetLastError);
      end if;
    else
      declare
        use type Win32.DWORD;

        type Handles is array (0..1) of aliased Win32.Winnt.HANDLE;

        Event_Handles : Handles := (From_Pipe.Read_Overlapped.hEvent, From_Pipe.Item_Event);
      begin
        Set_Timeout_For (From_Pipe, Wait_Time);
        if Win32.Winbase.ReadFile
            (hFile                => From_Pipe.Connection,
             lpBuffer             => Data,
             nNumberOfBytesToRead => From_Pipe.Size,
             lpNumberOfBytesRead  => Count'unchecked_access,
             lpOverlapped         => From_Pipe.Read_Overlapped'unchecked_access) /= Win32.TRUE
        then
          declare
            Error : constant Win32.DWORD := Win32.Winbase.GetLastError;
          begin
            if Error = Win32.Winerror.ERROR_IO_PENDING then
              loop
                case Win32.Winbase.WaitForMultipleObjects
                       (nCount         => Event_Handles'length,
                        lpHandles      => Event_Handles(Event_Handles'first)'unchecked_access,
                        bWaitAll       => Win32.FALSE,
                        dwMilliseconds => From_Pipe.Timeout) is
                when Win32.Winbase.WAIT_TIMEOUT =>
                  raise Timeout;
                when Win32.Winbase.WAIT_OBJECT_0 =>
                  Check (Win32.Winbase.GetOverlappedResult
                           (hFile                      => From_Pipe.Connection,
                            lpOverlapped               => From_Pipe.Read_Overlapped'unchecked_access,
                            lpNumberOfBytesTransferred => Count'unchecked_access,
                            bWait                      => Win32.FALSE), From_Pipe);
                  exit;
                when Win32.Winbase.WAIT_OBJECT_0 + 1 =>
                  From_Pipe.Get_Call (From_Pipe.Item.all);
                  Dispose (From_Pipe.Item);
                  From_Pipe.Item_Taken.Signal;
                when others =>
                  From_Pipe.Item_Taken.Signal;
                  Handle_Error (Error);
                end case;
              end loop;
            else
              From_Pipe.Item_Taken.Signal;
              Handle_Error (Error);
            end if;
          end;
        end if;
      end;
    end if;
    Length := Natural(Count);
  end Read;


  procedure Write (To_Pipe : Handle;
                   Data    : System.Address;
                   Length  : Natural) is

    Count : aliased Win32.DWORD;

    use type Win32.BOOL;
    use type Win32.DWORD;

  begin
    Check (To_Pipe);
    if To_Pipe.Kind = Client then
      if Win32.Winbase.WriteFile (hFile                  => To_Pipe.Connection,
                                  lpBuffer               => Data,
                                  nNumberOfBytesToWrite  => Win32.DWORD(Length),
                                  lpNumberOfBytesWritten => Count'unchecked_access,
                                  lpOverlapped           => null) /= Win32.TRUE
      then
        Handle_Error (Win32.Winbase.GetLastError);
      end if;
    else
      if Win32.Winbase.WriteFile
           (hFile                  => To_Pipe.Connection,
            lpBuffer               => Data,
            nNumberOfBytesToWrite  => Win32.DWORD(Length),
            lpNumberOfBytesWritten => Count'unchecked_access,
            lpOverlapped           => To_Pipe.Write_Overlapped'unchecked_access) /= Win32.TRUE
      then
        declare
          Error : constant Win32.DWORD := Win32.Winbase.GetLastError;
        begin
          if Win32.Winbase.GetLastError = Win32.Winerror.ERROR_IO_PENDING then
            case Win32.Winbase.WaitForSingleObject
                   (hHandle        => To_Pipe.Write_Overlapped.hEvent,
                    dwMilliseconds => Win32.Winbase.NMPWAIT_WAIT_FOREVER) is
            when Win32.Winbase.WAIT_TIMEOUT =>
              raise Timeout;
            when Win32.Winbase.WAIT_OBJECT_0 =>
              Check (Win32.Winbase.GetOverlappedResult
                       (hFile                      => To_Pipe.Connection,
                        lpOverlapped               => To_Pipe.Write_Overlapped'unchecked_access,
                        lpNumberOfBytesTransferred => Count'unchecked_access,
                        bWait                      => Win32.FALSE), To_Pipe);
            when others =>
              To_Pipe.Item_Taken.Signal;
              Handle_Error (Error);
            end case;
          else
            To_Pipe.Item_Taken.Signal;
            Handle_Error (Error);
          end if;
        end;
      end if;
    end if;
    if Count /= Win32.DWORD(Length) then
      raise Write_Incomplete;
    end if;
  end Write;


  procedure Put (To_Pipe : Handle;
                 Item    : String) is
  begin
    Check (To_Pipe);
    if To_Pipe.Get_Call = null then
      raise Program_Error;
    end if;
    To_Pipe.Item := new String'(Item);
    Check (Win32.Winbase.SetEvent (To_Pipe.Item_Event), To_Pipe);
    To_Pipe.Item_Taken.Wait;
  end Put;

end Os.Pipe;
