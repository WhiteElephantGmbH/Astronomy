-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Real_Time;
with Ten_Micron;
with Time_Client;
with Protected_Storage;


package body Clock is

  package RT renames Ada.Real_Time;

  use type Time.JD;

  Clock_Correction_Limit : constant Time.JD := Time.JD_Minute / 3.0;


  package Protected_Information is new Protected_Storage (Time.Server.Information);


  function Actual_Information return Time.Server.Information is
  begin
    return Protected_Information.Data;
  end Actual_Information;


  task type Control is

    entry Start;

    entry Define_Time;

    entry Finalize;

  end Control;


  The_Control : access Control;


  procedure Start is
  begin
    Log.Write ("start");
    The_Control := new Control;
    The_Control.Start;
  end Start;


  procedure Define_Time is
  begin
    if The_Control /= null then
      Log.Write ("Define Time");
      The_Control.Define_Time;
    end if;
  end Define_Time;


  procedure Finish is
  begin
    Log.Write ("finish");
    The_Control.Finalize;
  end Finish;


  task body Control is

    Pc_Time_Offset   : Time.JD_Seconds;
    The_Second_Count : RT.Seconds_Count := 0;

    Synchronize_Mount : Boolean := False;

    function Wakeup_Handling return RT.Time is
      Unused_TS       : RT.Time_Span;
      The_Information : Time.Server.Information;
      use type RT.Seconds_Count;
    begin
      if The_Second_Count = 0 then
        RT.Split (RT.Clock, The_Second_Count, Unused_TS);
      end if;
      if Time_Client.Exists then
        begin
          The_Information := Time_Client.Actual_Information;
          if The_Information.Clock_Set then
            if abs (The_Information.Clock_Time - Time.Julian_Date) > Clock_Correction_Limit then
              if Time_Client.Set (Time.Julian_Date) then
                Log.Write ("Set time from PC");
              end if;
            elsif The_Information.Clock_Synchronized and then Synchronize_Mount then
              if Time_Client.Synchronize_Mount then
                Log.Write ("Mount synchronized");
                Synchronize_Mount := False;
              end if;
            end if;
          end if;
        exception
        when Time_Client.Server_Not_Available =>
          The_Information := Time.Server.No_Information;
          The_Second_Count := 0;
          Log.Error ("Server not available");
        end;
        Protected_Information.Set (The_Information);
      end if;
      The_Second_Count := @ + 1;
      return RT.Time_Of (The_Second_Count, RT.To_Time_Span (0.0));
    end Wakeup_Handling;

    Wakeup_Time : RT.Time;

  begin -- Control
    accept Start;
    Wakeup_Time := Wakeup_Handling;
    loop
      select
        accept Finalize;
        exit;
      or
        accept Define_Time do
          if Time_Client.Exists then
            Synchronize_Mount := True;
          else
            if Ten_Micron.Gps_Is_Synchronized then
              Pc_Time_Offset := Time.JD_Seconds_Of (Time.Julian_Date - Ten_Micron.Julian_Date);
              Log.Write ("GPS is synchronized - PC time offset =" & Pc_Time_Offset'image);
            end if;
          end if;
        end Define_Time;
      or
        delay until Wakeup_Time;
        Wakeup_Time := Wakeup_Handling;
      end select;
    end loop;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
    accept Finalize;
  end Control;

end Clock;
