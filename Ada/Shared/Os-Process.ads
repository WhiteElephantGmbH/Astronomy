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

with System;

package Os.Process is

  type Handle is private;

  No_Handle : constant Handle;

  type Console_Type is (None, Normal, Invisible);

  procedure Create (Executable     : String;
                    Parameters     : String := "";
                    Environment    : String := "";
                    Current_Folder : String := "";
                    Std_Input      : Handle := No_Handle;
                    Std_Output     : Handle := No_Handle;
                    Std_Error      : Handle := No_Handle;
                    Console        : Console_Type := Normal);

  Creation_Failure : exception;
  --
  -- Procedure to create a detached process.
  --
  -- Note: The environment should be a single string composed of a series of variables names
  --       followed by an equals symbol followed by its value terminated by an ascii zero
  --       The null string causes the new process to inherit the environment of the parent.
  --
  -- Note: If the current folder is set to the null string then the current folder of new process
  --       is set to equal the current folder of the parent (creating process)
  --

  function Execution_Of (Executable     : String;
                         Parameters     : String;
                         Environment    : String  := "";
                         Current_Folder : String  := "";
                         Handle_Output  : Boolean := True;
                         Handle_Errors  : Boolean := True) return String;

  Execution_Failed : exception;


  type Priority_Class is (Idle, Normal, Above_Normal, High, Realtime);

  procedure Set_Priority_Class (Priority : Priority_Class);

private

  type Handle is new System.Address;

  No_Handle : constant Handle := Handle(System.Null_Address);

end Os.Process;
