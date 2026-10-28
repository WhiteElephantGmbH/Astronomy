-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Ada.Exceptions;
with Ada.Task_Identification;


package Thread is

  subtype Thread_Id is Ada.Task_Identification.Task_Id;

  Null_Thread_Id : constant Thread_Id := Ada.Task_Identification.Null_Task_Id;

  function Id_Of_Current_Thread return Thread_Id;

  function Is_Current_Thread (The_Thread : Thread_Id) return Boolean;

  function Image_Of (The_Thread : Thread_Id) return String;

  procedure Register (Task_Name : String);
  -- Associates a name for current thread

  procedure Terminated;
  -- Logs that the current task is about to terminate normally

  procedure Terminated (The_Occurrence : Ada.Exceptions.Exception_Occurrence);

  function Name_Of (The_Thread : Thread_Id) return String;
  -- returns the name associated with the specifed thread
  -- If a name hasn't been associated then the function returns the string <Unknown>

  procedure Log_All_Active_Threads;

end Thread;
