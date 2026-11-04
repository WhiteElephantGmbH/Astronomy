-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Time;
with Log;
with System.Storage_Elements;

package body Os.Pipe is

  package C renames Interfaces.C;

  type File_Descriptor is new C.int;

  type Poll_Events is mod 2 ** 16;

  type Poll_Fd is record
    Fd      : File_Descriptor;
    Events  : Poll_Events;
    Revents : Poll_Events;
  end record with Convention => C;

  function C_Mkfifo (Name : System.Address;
                     Mode : C.unsigned) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "mkfifo";

  function C_Open (Name  : System.Address;
                   Flags : C.int;
                   Mode  : C.unsigned) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "open";

  function C_Close (Fd : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "close";

  function C_Read (Fd     : C.int;
                   Buffer : System.Address;
                   Count  : C.size_t) return C.long with
    Import        => True,
    Convention    => C,
    External_Name => "read";

  function C_Write (Fd     : C.int;
                    Buffer : System.Address;
                    Count  : C.size_t) return C.long with
    Import        => True,
    Convention    => C,
    External_Name => "write";

  function C_Poll (Fds       : System.Address;
                   Count     : C.unsigned_long;
                   P_Timeout : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "poll";

  function C_Unlink (Name : System.Address) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "unlink";

  procedure C_Memcpy (Destination : System.Address;
                      Source      : System.Address;
                      Length      : C.size_t) with
    Import        => True,
    Convention    => C,
    External_Name => "memcpy";

  function C_Fcntl (Fd      : C.int;
                    Command : C.int;
                    Argument : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "fcntl";

  function C_Errno return System.Address with
    Import        => True,
    Convention    => C,
    External_Name => "__errno_location";

  type Errno_Access is access all C.int;

  function Errno_Value return C.int is
    function Address_To_Errno is new Ada.Unchecked_Conversion (System.Address, Errno_Access);
  begin
    return Address_To_Errno (C_Errno).all;
  end Errno_Value;

  -- Linux file/open constants.
  O_RDONLY   : constant C.int := 0;
  O_WRONLY   : constant C.int := 1;
  O_RDWR     : constant C.int := 2;
  O_NONBLOCK : constant C.int := 2048;

  -- fcntl commands.
  F_GETFL : constant C.int := 3;
  F_SETFL : constant C.int := 4;

  -- poll events.
  POLLIN   : constant Poll_Events := 1;
  POLLOUT  : constant Poll_Events := 4;
  POLLERR  : constant Poll_Events := 8;
  POLLHUP  : constant Poll_Events := 16;
  POLLNVAL : constant Poll_Events := 32;

  -- errno values used here.
  EINTR  : constant C.int := 4;
  ENXIO  : constant C.int := 6;
  EAGAIN : constant C.int := 11;
  EPIPE  : constant C.int := 32;

  FIFO_Mode : constant C.unsigned := 8#600#;

  type Buffer_Access is access String;

  type Named_Pipe (Name_Length : Positive;
                   Kind        : Role;
                   Size        : Positive) is
  record
    Name        : String(1 .. Name_Length);
    Connection  : File_Descriptor := -1;
    Mode        : Access_Mode;
    Data        : Buffer_Access;
    Data_Length : Natural := 0;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation (Named_Pipe, Handle);
  procedure Dispose is new Ada.Unchecked_Deallocation (String, Buffer_Access);


  procedure Check (The_Pipe : Handle) is
  begin
    if The_Pipe = null then
      raise No_Handle;
    end if;
  end Check;


  function Path_Of (The_Pipe : Handle) return String is
  begin
    return The_Pipe.Name;
  end Path_Of;


  function Open_Flags (The_Pipe : Handle) return C.int is
  begin
    case The_Pipe.Kind is
    when Server =>
      case The_Pipe.Mode is
      when Duplex   => return O_RDWR;
      when Inbound  => return O_RDONLY;
      when Outbound => return O_WRONLY;
      end case;
    when Client =>
      case The_Pipe.Mode is
      when Duplex   => return O_RDWR;
      when Inbound  => return O_WRONLY;
      when Outbound => return O_RDONLY;
      end case;
    end case;
  end Open_Flags;


  function Wanted_Events (The_Pipe : Handle) return Poll_Events is
  begin
    case The_Pipe.Kind is
    when Server =>
      case The_Pipe.Mode is
      when Duplex | Inbound => return POLLIN;
      when Outbound         => return POLLOUT;
      end case;
    when Client =>
      case The_Pipe.Mode is
      when Duplex | Outbound => return POLLIN;
      when Inbound           => return POLLOUT;
      end case;
    end case;
  end Wanted_Events;


  procedure Wait (The_Pipe  : Handle;
                  Events    : Poll_Events;
                  Wait_Time : Timer) is
    Poll : aliased Poll_Fd := (Fd      => The_Pipe.Connection,
                               Events  => Events,
                               Revents => 0);
    Milliseconds : C.int;
    Result       : C.int;
    use type C.int;
  begin
    if Wait_Time = Forever then
      Milliseconds := -1;
    else
      Milliseconds := C.int(Float(Wait_Time) * 1000.0);
    end if;

    loop
      Result := C_Poll
        (Poll'address, 1, Milliseconds);

      if Result > 0 then
        if (Poll.Revents and POLLNVAL) /= 0 then
          raise Bad_Pipe;
        elsif (Poll.Revents and POLLERR) /= 0 then
          raise Broken;
        elsif (Poll.Revents and POLLHUP) /= 0 and then
              (Poll.Revents and Events) = 0
        then
          raise Broken;
        end if;
        return;
      elsif Result = 0 then
        raise Timeout;
      elsif Errno_Value = EINTR then
        null;
      else
        raise Unknown_Error;
      end if;
    end loop;
  end Wait;


  procedure Make_Server_Fifo (The_Pipe : Handle) is
    Name : aliased constant String := Path_Of (The_Pipe) & Ascii.Nul;
    Result : C.int;
    use type C.int;
  begin
    Result := C_Mkfifo (Name'address, FIFO_Mode);

    if Result /= 0 then
      case Errno_Value is
      when 17 => -- EEXIST
        raise Name_In_Use;
      when 13 => -- EACCES
        raise Access_Denied;
      when 2 =>  -- ENOENT
        raise Invalid_Name;
      when others =>
        raise Unknown_Error;
      end case;
    end if;
  end Make_Server_Fifo;


  procedure Open_Connection (The_Pipe : Handle;
                             Wait_Time : Timer) is
    use type C.int;
    Name        : aliased constant String := Path_Of (The_Pipe) & Ascii.Nul;
    Flags       : constant C.int := Open_Flags (The_Pipe) + O_NONBLOCK;
    Retry_Delay : constant Duration := 0.1;
    Retries     : Natural := 0;
    Max_Retries : Natural;
    Fd          : C.int;
    Dummy       : C.int;
  begin
    if Wait_Time = Forever then
      Max_Retries := Natural'last;
    else
      Max_Retries := Natural(Float(Wait_Time) / Float(Retry_Delay));
    end if;

    loop
      Fd := C_Open (Name'address, Flags, FIFO_Mode);

      exit when Fd >= 0;

      case Errno_Value is
      when ENXIO => -- no reader yet
        if Wait_Time /= Forever and then Retries >= Max_Retries then
          raise No_Server;
        end if;
        Retries := Retries + 1;
        Time.Wait (Retry_Delay);

      when EAGAIN | EINTR =>
        Time.Wait (Retry_Delay);

      when 13 =>
        raise Access_Denied;

      when 2 =>
        raise No_Server;

      when others =>
        raise Unknown_Error;
      end case;
    end loop;

    The_Pipe.Connection := File_Descriptor (Fd);

    -- Return to blocking I/O after the connection has been established.
    declare
      Current_Flags : constant C.int := C_Fcntl (Fd, F_GETFL, 0);
    begin
      if Current_Flags < 0 then
        raise Unknown_Error;
      end if;

      Dummy := C_Fcntl (Fd, F_SETFL, Current_Flags - O_NONBLOCK);

      if Dummy < 0 then
        raise Unknown_Error;
      end if;
    end;
  end Open_Connection;


  procedure Open (The_Pipe                 : in out Handle;
                  Name                     :        String;
                  Kind                     :        Role;
                  Mode                     :        Access_Mode;
                  Size                     :        Natural;
                  Wait_Time                :        Timer := Forever;
                  Get_Call                 :        Get_Callback := null;
                  Allow_Remote_Connections :        Boolean := False) is
    pragma Unreferenced (Allow_Remote_Connections);
  begin
    if Kind = Client and then Get_Call /= null then
      raise Not_Server;
    end if;

    Close (The_Pipe);

    if Name'length = 0 or else Size = 0 then
      raise Invalid_Name;
    end if;

    The_Pipe := new Named_Pipe (Name'length, Kind, Size);
    The_Pipe.Name := Name;
    The_Pipe.Mode := Mode;
    The_Pipe.Data := new String(1 .. Size);

    if Kind = Server then
      Make_Server_Fifo (The_Pipe);
    end if;

    begin
      Open_Connection (The_Pipe, Wait_Time);
    exception
    when others =>
      Close (The_Pipe);
      raise;
    end;
  exception
  when others =>
    Close (The_Pipe);
    raise;
  end Open;


  procedure Close (The_Pipe : in out Handle) is
    Dummy : C.int;
  begin
    if The_Pipe = null then
      return;
    end if;

    begin
      if The_Pipe.Connection >= 0 then
        Dummy := C_Close (C.int(The_Pipe.Connection));
        The_Pipe.Connection := -1;
      end if;

      if The_Pipe.Kind = Server then
        declare
          Name : aliased constant String := Path_Of (The_Pipe) & Ascii.Nul;
        begin
          Dummy := C_Unlink (Name'address);
        end;
      end if;

      if The_Pipe.Data /= null then
        Dispose (The_Pipe.Data);
      end if;
    exception
    when Item : others =>
      Log.Write ("Os.Pipe.Close", Item);
    end;

    Dispose (The_Pipe);
  end Close;


  procedure Read (From_Pipe :     Handle;
                  Data      :     System.Address;
                  Length    : out Natural;
                  Wait_Time :     Timer := Forever) is
    Available   : Natural;
    Count       : C.long;
    Dummy       : C.long;
    use type C.long;
    use type C.int;
  begin
    Length := 0;
    Check (From_Pipe);

    loop
      -- First return a complete line already buffered from a previous read.
      if From_Pipe.Data_Length > 0 then
        declare
          Line_End : Natural := 0;
        begin
          for Index in 1 .. From_Pipe.Data_Length loop
            if From_Pipe.Data (Index) = Ascii.Lf then
              Line_End := Index;
              exit;
            end if;
          end loop;

          if Line_End /= 0 then
            C_Memcpy (Destination => Data,
                      Source      => From_Pipe.Data'address,
                      Length      => C.size_t(Line_End));

            Available := From_Pipe.Data_Length - Line_End;
            if Available > 0 then
              From_Pipe.Data (1 .. Available) := From_Pipe.Data (Line_End + 1 .. From_Pipe.Data_Length);
            end if;
            From_Pipe.Data_Length := Available;
            Length := Line_End;
            return;
          end if;
        end;
      end if;

      Wait (From_Pipe, Wanted_Events (From_Pipe), Wait_Time);

      Count := C_Read (C.int(From_Pipe.Connection),
                       From_Pipe.Data (From_Pipe.Data_Length + 1)'address,
                       C.size_t(From_Pipe.Data'length - From_Pipe.Data_Length));

      if Count = 0 then
        raise Broken;
      elsif Count < 0 then
        if Errno_Value = EINTR then
          null;
        elsif Errno_Value = EAGAIN then
          null;
        else
          raise Unknown_Error;
        end if;
      else
        From_Pipe.Data_Length := From_Pipe.Data_Length + Natural(Count);
      end if;
    end loop;
  end Read;


  procedure Write (To_Pipe : Handle;
                   Data    : System.Address;
                   Length  : Natural) is
    Remaining : Natural := Length;
    Offset    : Natural := 0;
    Count     : C.long;
    Address   : System.Address;
    use type System.Storage_Elements.Integer_Address;
    use type C.long;
    use type C.int;
  begin
    Check (To_Pipe);

    while Remaining > 0 loop
      Wait (To_Pipe, POLLOUT, Forever);

      Address := System.Storage_Elements.To_Address
        (System.Storage_Elements.To_Integer (Data) + System.Storage_Elements.Integer_Address(Offset));
      Count := C_Write (C.int(To_Pipe.Connection), Address, C.size_t(Remaining));

      if Count > 0 then
        Offset := Offset + Natural(Count);
        Remaining := Remaining - Natural(Count);
      elsif Count < 0 and then Errno_Value = EINTR then
        null;
      elsif Count < 0 and then Errno_Value = EPIPE then
        raise Broken;
      else
        raise Unknown_Error;
      end if;
    end loop;
  end Write;


  procedure Put (To_Pipe : Handle;
                 Item    : String) is
  begin
    Check (To_Pipe);
    Write (To_Pipe, Item'address, Item'length);
  end Put;

end Os.Pipe;
