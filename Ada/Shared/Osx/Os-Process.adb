-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Directories;
with Ada.Unchecked_Conversion;
with File;
with GNAT.OS_Lib;
with Log;

package body Os.Process is

  Not_Supported : exception;

  procedure Create (Executable     : String;
                    Parameters     : String := "";
                    Environment    : String := "";
                    Current_Folder : String := "";
                    Std_Input      : Handle := No_Handle;
                    Std_Output     : Handle := No_Handle;
                    Std_Error      : Handle := No_Handle;
                    Console        : Console_Type := Normal) is
  begin
    raise Not_Supported;
  end Create;


  function Created (Executable     : String;
                    Current_Folder : String := "";
                    Parameters     : String := "") return Id is

    function Convert is new Ada.Unchecked_Conversion (GNAT.OS_Lib.Process_Id, Id_Value);

    The_Process_Id : GNAT.OS_Lib.Process_Id;
    The_Arguments  : GNAT.OS_Lib.Argument_List_Access;

    procedure Cleanup is
      use type GNAT.OS_Lib.Argument_List_Access;
    begin
      if The_Arguments /= null then
        GNAT.OS_Lib.Free (The_Arguments);
      end if;
    end Cleanup;

    use type GNAT.OS_Lib.Process_Id;

  begin -- Create
    if not File.Exists (Executable) then
      Log.Write ("Executable " & Executable & " not found");
      raise Execution_Failed;
    end if;
    The_Arguments := GNAT.OS_Lib.Argument_String_To_List (Parameters);
    if Current_Folder /= "" then
      begin
        Ada.Directories.Set_Directory (Current_Folder);
      exception
      when others =>
        Log.Write ("Current_Folder " & Current_Folder & " not set");
        raise Execution_Failed;
      end;
    end if;
    The_Process_Id := GNAT.OS_Lib.Non_Blocking_Spawn (Program_Name => Executable,
                                                      Args         => The_Arguments.all);
    Cleanup;
    return (Is_Defined => The_Process_Id /= GNAT.OS_Lib.Invalid_Pid,
            Value      => Convert (The_Process_Id));
  exception
  when Execution_Failed =>
    Cleanup;
    raise;
  when Item: others =>
    Log.Write ("Os.Process.Create", Item);
    Cleanup;
    raise Execution_Failed;
  end Created;


  procedure Terminate_With (Process_Id : Id) is
    function Convert is new Ada.Unchecked_Conversion (Id_Value, GNAT.OS_Lib.Process_Id);
  begin
    if Process_Id.Is_Defined then
      GNAT.OS_Lib.Kill (Pid       => Convert (Process_Id.Value),
                        Hard_Kill => True);
    end if;
  end Terminate_With;


  function Execution_Of (Executable     : String;
                         Parameters     : String;
                         Environment    : String  := "";
                         Current_Folder : String  := "";
                         Handle_Output  : Boolean := True;
                         Handle_Errors  : Boolean := True) return String is

    The_Arguments   : GNAT.OS_Lib.Argument_List_Access;
    The_Return_Code : Integer;
    The_File        : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
    The_Filename    : GNAT.OS_Lib.String_Access;

    procedure Cleanup is
      Unused : Boolean;
      use type GNAT.OS_Lib.String_Access;
      use type GNAT.OS_Lib.Argument_List_Access;
    begin
      if The_Filename /= null then
        GNAT.OS_Lib.Close (The_File);
        GNAT.OS_Lib.Delete_File (The_Filename.all, Unused);
        GNAT.OS_Lib.Free (The_Filename);
      end if;
      if The_Arguments /= null then
        GNAT.OS_Lib.Free (The_Arguments);
      end if;
    end Cleanup;

    use type GNAT.OS_Lib.File_Descriptor;

  begin -- Execution_Of
    if Environment /= "" then
      Log.Write ("Os.Process.Execution_Of - Environment NOT SUPPORTED");
      raise Not_Supported;
    elsif Current_Folder /= "" then
      begin
        Ada.Directories.Set_Directory (Current_Folder);
      exception
      when others =>
        Log.Write ("Current_Folder " & Current_Folder & " not set");
        raise Execution_Failed;
      end;
    end if;
    if not File.Exists (Executable) then
      Log.Write ("Executable " & Executable & " not found");
      raise Execution_Failed;
    end if;
    if not Handle_Output then
      Log.Write ("Os.Process.Execution_Of - Output must be handled");
      raise Not_Supported;
    end if;
    The_Arguments := GNAT.OS_Lib.Argument_String_To_List (Parameters);
    GNAT.OS_Lib.Create_Temp_Output_File (FD   => The_File,
                                         Name => The_Filename);
    if The_File = GNAT.OS_Lib.Invalid_FD then
      Log.Write ("Execution_Of (" & Executable & " failed (temp file creation failed)");
      raise Execution_Failed;
    end if;
    GNAT.OS_Lib.Spawn (Program_Name           => Executable,
                       Args                   => The_Arguments.all,
                       Output_File_Descriptor => The_File,
                       Return_Code            => The_Return_Code,
                       Err_To_Out             => Handle_Errors);
    if The_Return_Code /= 0 then
      Log.Write ("Execution_Of " & Executable & " failed with " & GNAT.OS_Lib.Errno_Message);
      raise Execution_Failed;
    end if;
    GNAT.OS_Lib.Close (The_File);
    The_File := GNAT.OS_Lib.Open_Read (Name  => The_Filename.all,
                                       Fmode => GNAT.OS_Lib.Text);
    declare
      The_String : aliased String(1..200000);
      The_Count  : Integer;
    begin
      The_Count := GNAT.OS_Lib.Read (FD => The_File,
                                     A  => The_String(The_String'first)'address,
                                     N  => The_String'length);
      if The_Count = The_String'length then
        Log.Write ("Execution_Of " & Executable & " warning result truncated");
      end if;
      Cleanup;
      return The_String(The_String'first .. The_String'first + The_Count - 1);
    end;
  exception
  when Execution_Failed =>
    Cleanup;
    raise;
  when Not_Supported =>
    raise;
  when Item: others =>
    Log.Write ("Os.Process.Execution_Of", Item);
    Cleanup;
    raise Execution_Failed;
  end Execution_Of;


  procedure Set_Priority_Class (Priority : Priority_Class) is
  begin
    Log.Write ("Os.Process.Set_Priority_Class (Priority => " & Priority'image & ") - not IMPLEMENTED");
  end Set_Priority_Class;

end Os.Process;
