-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                              Interface to the Standard C library  (Linux simulation)                              *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Text_IO;
with Time;

package body Standard_C_Interface is

  function Clock_Getres (Clock : Clock_Id;
                         Res   : access Timespec) return Return_Code is
    pragma Unreferenced (Clock, Res);
  begin
    return Failed;
  end Clock_Getres;


  function Clock_Gettime (Clock : Clock_Id;
                          Tp    : access Timespec) return Return_Code is
    pragma Unreferenced (Clock, Tp);
  begin
    return Failed;
  end Clock_Gettime;


  function Clock_Settime (Clock : Clock_Id;
                          Tp    : access constant Timespec) return Return_Code is
  begin
    Ada.Text_IO.Put_Line ("Clock " & Clock'image & " set" & Tp.all'image);
    return Success;
  end Clock_Settime;


  Is_Aborting   : Boolean := False;
  Expected_Edge : Edge := Falling;

  function Wait_Select (Nfds       : Fd_Number;
                        Read_Fds   : access Fd_Set;
                        Write_Fds  : access Fd_Set := null;
                        Except_Fds : access Fd_Set := null;
                        Timeout    : access Timeval := null) return Return_Count is
    pragma Unreferenced (Nfds, Write_Fds, Except_Fds, Timeout);
  begin
    Read_Fds(GPIO_Edge) := False;
    Read_Fds(GPIO_Aborter) := False;
    loop
      declare
        Day_Time             : constant Duration := Time.Day_Seconds;
        Truncated_To_Minutes : constant Duration := Duration(Natural(Day_Time / 60.0 + Duration(0.5)) - 1) * 60.0;
        Time_To_Next_Minute  : constant Duration := 60.0 - (Day_Time - Truncated_To_Minutes);
      begin
        if Is_Aborting then
          Ada.Text_IO.Put_Line ("Wait_Select - aborting");
          Read_Fds(GPIO_Aborter) := True;
          exit;
        elsif Time_To_Next_Minute > 1.5 then
          Time.Wait (1.0);
        else
          Read_Fds(GPIO_Edge) := True;
          case Expected_Edge is
          when Falling =>
            Time.Wait (Time_To_Next_Minute - 0.1);
          when Rising =>
            Time.Wait (Time_To_Next_Minute);
          end case;
          Ada.Text_IO.Put_Line (Expected_Edge'image & " edge at day time" & Time.Day_Seconds'image);
          exit;
        end if;
      end;
    end loop;
    return 1;
  end Wait_Select;


  procedure Signal_Abort is
  begin
    Is_Aborting := True;
  end Signal_Abort;


  procedure Set (Expected : Edge) is
  begin
    Expected_Edge := Expected;
  end Set;

end Standard_C_Interface;
