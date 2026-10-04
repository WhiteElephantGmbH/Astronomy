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

with Indefinite_Doubly_Linked_Lists;
with Log;

package body Thread is

  type Task_Information (Name_Size : Natural) is record
    The_Thread_Id : Thread_Id;
    The_Name      : String (1..Name_Size);
  end record;

  package Task_List is new Indefinite_Doubly_Linked_Lists (Task_Information);

  The_Tasks : Task_List.Item;

  function Id_Of_Current_Thread return Thread_Id is
  begin
    return Ada.Task_Identification.Current_Task;
  end Id_Of_Current_Thread;


  function Is_Current_Thread (The_Thread : Thread_Id) return Boolean is
    use type Thread_Id;
  begin
    return The_Thread = Id_Of_Current_Thread;
  end Is_Current_Thread;


  function Image_Of (The_Thread : Thread_Id) return String is
  begin
    return Ada.Task_Identification.Image (The_Thread);
  end Image_Of;


  procedure Register (Task_Name : String) is
    The_Task_Information : Task_Information (Name_Size => Task_Name'length);
  begin
    The_Task_Information.The_Name := Task_Name;
    The_Task_Information.The_Thread_Id := Id_Of_Current_Thread;
    The_Tasks.Append (The_Task_Information);
    Log.Write ("<Task>+ Task " & Task_Name & " Created");
  end Register;


  function Name_Of (The_Thread : Thread_Id) return String is
    use type Thread_Id;
  begin
    for The_Information of The_Tasks loop
      if The_Information.The_Thread_Id = The_Thread then
        return The_Information.The_Name;
      end if;
    end loop;
    return "<Unknown>";
  end Name_Of;


  procedure Terminated (The_Occurrence : Ada.Exceptions.Exception_Occurrence) is
    The_Thread_Id    : constant Thread_Id := Id_Of_Current_Thread;
    The_Cursor       : Task_List.Cursor;
    The_Exception_Id : constant Ada.Exceptions.Exception_Id := Ada.Exceptions.Exception_Identity (The_Occurrence);
    use type Task_List.Cursor;
    use type Thread_Id;
    use type Ada.Exceptions.Exception_Id;
  begin
    The_Cursor := Task_List.First (The_Tasks);
    while The_Cursor /= Task_List.No_Element loop
      if Task_List.Element_At (The_Cursor).The_Thread_Id = The_Thread_Id then
        if The_Exception_Id = Ada.Exceptions.Null_Id then
          Log.Write ("<Task>- Task " & Task_List.Element_At (The_Cursor).The_Name & " terminated normally");
        else
          Log.Write ("<Task>- Task " & Task_List.Element_At (The_Cursor).The_Name & " terminated by exception");
          Log.Write (The_Occurrence);
        end if;
        Task_List.Delete (The_Tasks, The_Cursor);
        return;
      end if;
      Task_List.Next (The_Cursor);
    end loop;
    -- Here if the task was not registered.
    if The_Exception_Id = Ada.Exceptions.Null_Id then
      Log.Write ("<Task>- Unknown task terminated normally");
    else
      Log.Write ("<Task>- Unknown task terminated by exception");
      Log.Write (The_Occurrence);
    end if;
  end Terminated;


  procedure Terminated is
  begin
    Terminated (Ada.Exceptions.Null_Occurrence);
  end Terminated;


  procedure Log_All_Active_Threads is
  begin
    null; -- Not implemented on OSX
  end Log_All_Active_Threads;


end Thread;
