-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Os.Process is

  procedure Create (Executable     : String;
                    Parameters     : String := "";
                    Environment    : String := "";
                    Current_Folder : String := "";
                    Std_Input      : Handle := No_Handle;
                    Std_Output     : Handle := No_Handle;
                    Std_Error      : Handle := No_Handle;
                    Console        : Console_Type := Normal) is
  begin
    raise Program_Error; -- not implemented
  end Create;


  function Created (Executable : String;
                    Parameters : String := "") return Id is
  begin
    raise Program_Error; -- not implemented
    return (Id_Value   => 0,
            Is_Defined => False);
  end Created;


  procedure Terminate_With (Process_Id : Id) is
  begin
    null;
  end Terminate_With;


  function Execution_Of (Executable     : String;
                         Parameters     : String;
                         Environment    : String  := "";
                         Current_Folder : String  := "";
                         Handle_Output  : Boolean := True;
                         Handle_Errors  : Boolean := True) return String is
  begin
    raise Program_Error; -- not implemented
    return "";
  end Execution_Of;


  procedure Set_Priority_Class (Priority : Priority_Class) is
  begin
    null; -- not implemented
  end Set_Priority_Class;

end Os.Process;
