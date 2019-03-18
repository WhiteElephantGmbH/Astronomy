-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with GNAT.Lock_Files;
with File;
with Program;
with System;

package body Os.Application is

  function Lock_Name return String is
  begin
    return "/tmp/" & Name;
  end Lock_Name;


  protected Controlled is

    procedure Lock;

    procedure Termination_Handler (Unused_Cause                : Ada.Task_Termination.Cause_Of_Termination;
                                   Unused_Task_Id              : Ada.Task_Identification.Task_Id;
                                   Unused_Exception_Occurrence : Ada.Exceptions.Exception_Occurrence);
  private
    Is_Locked : Boolean := False;
  end Controlled;


  protected body Controlled is

    procedure Lock is
    begin
      GNAT.Lock_Files.Lock_File (Lock_File_Name => Lock_Name,
                                 Wait           => 1.0,
                                 Retries        => 1);
      Is_Locked := True;
    end Lock;


    procedure Termination_Handler (Unused_Cause                : Ada.Task_Termination.Cause_Of_Termination;
                                   Unused_Task_Id              : Ada.Task_Identification.Task_Id;
                                   Unused_Exception_Occurrence : Ada.Exceptions.Exception_Occurrence) is
    begin
      if Is_Locked then
        GNAT.Lock_Files.Unlock_File (Lock_Name);
      end if;
    end Termination_Handler;

  end Controlled;


  function Is_First_Instance return Boolean is
  begin
    Controlled.Lock;
    return True;
  exception
  when others =>
    return False;
  end Is_First_Instance;


  function Application_Name return String is

    type Access_Integer is access all Integer;
   
    function GetExecutablePath (The_Buffer     : System.Address;
                                Size_Of_Buffer : Access_Integer) return Integer
    with
      Import        => True,
      Convention    => C,
      External_Name => "_NSGetExecutablePath";
      
    Small_Buffer : aliased String (1 .. 200);  -- An arbitary sized buffer big enough to satisfy most requests.
    Size         : aliased Integer := Small_Buffer'length;
    Status       : Integer;
   
  begin
    Status := GetExecutablePath (The_Buffer     => Small_Buffer'address,
                                 Size_Of_Buffer => Size'access);
    if Status = 0 then -- Pasth was obtained
      for Index in Small_Buffer'range loop -- look for zero termination
        if Small_Buffer(Index) = Character'first then
          return Small_Buffer (1 .. Index - 1);
        end if;
      end loop;
      return Small_Buffer;
    elsif Status /= -1 then
      raise No_Information; -- Unexpected return code     
    end if;
    declare -- Here if Small buffer too small - Size set to be how big it should be
      Big_Buffer : aliased String (1 .. Size);
    begin
      Status := GetExecutablePath (The_Buffer     => Big_Buffer'address,
                                   Size_Of_Buffer => Size'access);
      if Status /= 0 then
        raise No_Information;
      end if;
      for Index in Big_Buffer'range loop -- look for zero termination
        if Big_Buffer(Index) = Character'first then
          return Big_Buffer (1 .. Index - 1);
        end if;
      end loop;
      return Big_Buffer;
    end;
  end Application_Name;  


  function Origin_Folder return String is
  begin
    return File.Containing_Directory_Of (Application_Name) & File.Folder_Separator;
  exception
  when others =>
    return "";
  end Origin_Folder;


  function Name return String is
  begin
    return File.Base_Name_Of (Application_Name);
  exception
  when others =>
    return "ApplicationName";
  end Name;


  function Version return String is
  begin
    return Program.Version;
  end Version;


  function Version return Unsigned.Quadword is
  begin
   return Program.Version;
  end Version;

begin
  Ada.Task_Termination.Set_Specific_Handler (T       => Ada.Task_Identification.Current_Task,
                                             Handler => Controlled.Termination_Handler'access);
end Os.Application;
