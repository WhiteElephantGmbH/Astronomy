-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Strings.Fixed;
with Interfaces.C;
with Log;
with System.Storage_Elements;
with Text;

package body Os.Process is

  package C renames Interfaces.C;

  type Pid is new C.int;

  function Fork return Pid with
    Import        => True,
    Convention    => C,
    External_Name => "fork";

  function Chdir (Path : System.Address) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "chdir";

  function Setsid return Pid with
    Import        => True,
    Convention    => C,
    External_Name => "setsid";

  function Dup2 (Old_Fd : C.int;
                 New_Fd : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "dup2";

  function Close_Fd (Fd : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "close";

  function Exec_Shell (Command : System.Address) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "system";

  function Wait_Pid (Id      : Pid;
                     Status  : System.Address;
                     Options : C.int) return Pid with
    Import        => True,
    Convention    => C,
    External_Name => "waitpid";

  function Pipe_Create (File_Descriptors : System.Address) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "pipe";

  function Read_Fd (Fd    : C.int;
                    Data  : System.Address;
                    Length : C.size_t) return C.long with
    Import        => True,
    Convention    => C,
    External_Name => "read";

  function Set_Priority (Which : C.int;
                         Who   : C.int;
                         Value : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "setpriority";

  function Put_Environment (Name  : System.Address;
                            Value : System.Address;
                            Overwrite : C.int) return C.int with
    Import        => True,
    Convention    => C,
    External_Name => "setenv";

  PRIO_PROCESS : constant C.int := 0;

  function Shell_Executable_Of (Executable : String) return String is
    Last_Separator : Natural := 0;
    Last : constant Natural := Executable'last;
  begin
    for Index in Executable'range loop
      if Executable (Index) in '/' | '\' then
        Last_Separator := Index;
      end if;
    end loop;

    if Last_Separator = 0 then
      return Executable;
    elsif Last_Separator = Last then
      return Executable;
    else
      return Executable (Last_Separator + 1 .. Last);
    end if;
  end Shell_Executable_Of;


  function Linux_Executable_Of (Executable : String) return String is
    Name : constant String := Shell_Executable_Of (Executable);
  begin
    if Name'length > 4 and then
       Name (Name'last - 3 .. Name'last) = ".exe"
    then
      return Name (Name'first .. Name'last - 4);
    end if;

    return Name;
  end Linux_Executable_Of;


  function Command_Line_Of (Executable : String;
                            Parameters : String) return String is
    Name : constant String := Linux_Executable_Of (Executable);
  begin
    if Parameters = "" then
      return Name;
    else
      return Name & " " & Parameters;
    end if;
  end Command_Line_Of;


  procedure Set_Environment (Environment : String) is
    First : Positive := Environment'first;
  begin
    while First <= Environment'last loop
      declare
        Last : Natural := First;
      begin
        while Last <= Environment'last and then Environment (Last) /= Ascii.Nul loop
          Last := Last + 1;
        end loop;

        exit when Last = First;

        declare
          Environment_Entry : constant String := Environment (First .. Last - 1);
          Equal             : constant Natural := Ada.Strings.Fixed.Index (Environment_Entry, "=");
        begin
          if Equal > 1 then
            declare
              Name  : aliased constant String := Environment_Entry (Environment_Entry'first .. Equal - 1) & Ascii.Nul;
              Value : aliased constant String := Environment_Entry (Equal + 1 .. Environment_Entry'last) & Ascii.Nul;
              Dummy : C.int;
            begin
              Dummy := Put_Environment (Name'address, Value'address, 1);
            end;
          end if;
        end;
        exit when Last = Environment'last;
        First := Last + 1;
      end;
    end loop;
  end Set_Environment;


  procedure Create (Executable     : String;
                    Parameters     : String := "";
                    Environment    : String := "";
                    Current_Folder : String := "";
                    Std_Input      : Handle := No_Handle;
                    Std_Output     : Handle := No_Handle;
                    Std_Error      : Handle := No_Handle;
                    Console        : Console_Type := Normal) is

    Pid_Value : Pid;
    Command   : aliased constant String := Command_Line_Of (Executable, Parameters) & Ascii.Nul;
    Folder    : aliased constant String := Current_Folder & Ascii.Nul;

    use type C.int;

    Input_Fd  : C.int := -1;
    Output_Fd : C.int := -1;
    Error_Fd  : C.int := -1;

  begin -- Create
    Pid_Value := Fork;

    if Pid_Value < 0 then
      raise Creation_Failure;
    elsif Pid_Value = 0 then
      if Console = None then
        declare
          Dummy : Pid;
        begin
          Dummy := Setsid;
        end;
      end if;

      if Current_Folder /= "" then
        declare
          Dummy : C.int;
        begin
          Dummy := Chdir (Folder'address);
        end;
      end if;

      if Std_Input /= No_Handle then
        Input_Fd := C.int(System.Storage_Elements.To_Integer (System.Address(Std_Input)));
        declare
          Dummy : C.int;
        begin
          Dummy := Dup2 (Input_Fd, 0);
        end;
      end if;

      if Std_Output /= No_Handle then
        Output_Fd := C.int(System.Storage_Elements.To_Integer (System.Address(Std_Output)));
        declare
          Dummy : C.int;
        begin
          Dummy := Dup2 (Output_Fd, 1);
        end;
      end if;

      if Std_Error /= No_Handle then
        Error_Fd := C.int(System.Storage_Elements.To_Integer (System.Address(Std_Error)));
        declare
          Dummy : C.int;
        begin
          Dummy := Dup2 (Error_Fd, 2);
        end;
      end if;

      if Environment /= "" then
        Set_Environment (Environment);
      end if;

      -- On Linux the executable is normally found through PATH.
      -- A Windows ".exe" name is reduced to its basename so that the
      -- same Siril_Interface body can be used on both systems.
      declare
        Dummy : C.int;
      begin
        Dummy := Exec_Shell (Command'address);
      end;

      -- Reaching this point means that /bin/sh could not execute the
      -- command.  The child has no useful work left to do.
      raise Creation_Failure;
    end if;

    null; -- The child continues independently.
  exception
  when Creation_Failure =>
    raise;
  when Item : others =>
    Log.Write ("Os.Process.Create", Item);
    raise Creation_Failure;
  end Create;


  function Execution_Of (Executable     : String;
                         Parameters     : String;
                         Environment    : String  := "";
                         Current_Folder : String  := "";
                         Handle_Output  : Boolean := True;
                         Handle_Errors  : Boolean := True) return String
  is
    File_Descriptors : aliased array (1 .. 2) of C.int;
    Pid_Value        : Pid;
    Data             : aliased String (1 .. 1000);
    Length           : C.long;
    Result           : Text.String;
    Command          : aliased constant String := Command_Line_Of (Executable, Parameters) & Ascii.Nul;
    Folder           : aliased constant String := Current_Folder & Ascii.Nul;
    Status           : aliased C.int := 0;
    Dummy            : C.int;
    Dummy_Pid        : Pid;
    use type C.int;
    use type C.long;
  begin
    if Pipe_Create (File_Descriptors'address) /= 0 then
      raise Execution_Failed;
    end if;

    Pid_Value := Fork;

    if Pid_Value < 0 then
      Dummy := Close_Fd (File_Descriptors (1));
      Dummy := Close_Fd (File_Descriptors (2));
      raise Execution_Failed;
    elsif Pid_Value = 0 then
      Dummy := Close_Fd (File_Descriptors (1));

      if Handle_Output then
        Dummy := Dup2 (File_Descriptors (2), 1);
      end if;

      if Handle_Errors then
        Dummy := Dup2 (File_Descriptors (2), 2);
      end if;

      if Current_Folder /= "" then
        Dummy := Chdir (Folder'address);
      end if;

      if Environment /= "" then
        Set_Environment (Environment);
      end if;

      Dummy := Exec_Shell (Command'address);
      raise Execution_Failed;
    end if;

    Dummy := Close_Fd (File_Descriptors (2));

    loop
      Length := Read_Fd (File_Descriptors (1), Data'address, Data'length);

      exit when Length <= 0;

      declare
        Count : constant Natural := Natural'min(Natural(Length), Max_Result_Length - Result.Count);
      begin
        exit when Count = 0;
        Text.Append (Result, Data (Data'first .. Data'first + Count - 1));
      end;
    end loop;

    Dummy := Close_Fd (File_Descriptors (1));
    Dummy_Pid := Wait_Pid (Pid_Value, Status'address, 0);

    if Result.Count > Max_Result_Length then
      return Result.Slice (Text.First_Index, Max_Result_Length);
    end if;

    return Result.S;
  exception
  when Execution_Failed =>
    raise;
  when Item : others =>
    Log.Write ("Os.Process.Execution_Of", Item);
    raise Execution_Failed;
  end Execution_Of;


  procedure Set_Priority_Class (Priority : Priority_Class) is
    Value : C.int;
    Dummy : C.int;
    use type C.int;
  begin
    case Priority is
    when Idle         => Value := 19;
    when Normal       => Value := 0;
    when Above_Normal => Value := -5;
    when High         => Value := -10;
    when Realtime     => Value := -20;
    end case;

    Dummy := Set_Priority (PRIO_PROCESS, 0, Value);

    if Dummy /= 0 then
      raise Program_Error;
    end if;
  end Set_Priority_Class;

end Os.Process;
