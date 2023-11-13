-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Calendar.Time_Zones;
with Ten_Micron;
with Time;
with Network.Udp;
with Parameter;
with System;
with Traces;
with Unsigned;

package body Clock is

  package Log is new Traces ("Clock");

  Pc_Time_Tolerance : constant Duration := 1.0;

  task type Control with Priority => System.Max_Priority is

    entry Start;

    entry Define_Time;

    entry Finalize;

  end Control;

  The_Control : access Control;


  task body Control is

    The_Socket : Network.Udp.Socket;

    One_Microsecond : constant := 10.0**(-6);

    type Seconds is delta One_Microsecond range 0.0 .. (2**32 - 1) * One_Microsecond with
      Small => One_Microsecond,
      Size  => 32;

    type Time_Server_Data is record
      Delta_Time : Seconds;
      One_Minute : Seconds;
    end record with
      Convention => C,
      Size       => 64;

    Undefined_Time : constant Seconds := 0.0;
    Undefined_Data : constant Time_Server_Data := (others => Undefined_Time);

    One_Minute : constant := Time.JD_Minute;
    One_Second : constant := Time.JD_Second;


    procedure Get (The_Data : out Time_Server_Data) is

      Sentinel : aliased constant Unsigned.Longword := 16#42424242#;

      function Datagram_From is new Network.Udp.Datagram_From  (Time_Server_Data);

    begin -- Get
      The_Data := Undefined_Data;
      Network.Udp.Send (Message     => Sentinel'address,
                        Size        => Sentinel'size / 8,
                        Used_Socket => The_Socket);
      The_Data := Datagram_From (The_Socket);
    exception
    when Network.Timeout =>
      Log.Error ("RECEIVE TIMEOUT");
    when Network.Receive_Error =>
      Log.Error ("RECEIVE ERROR");
    end Get;

    Pc_Time_Offset : Duration;

    use type Time.JD;

  begin -- Control
    accept Start;
    The_Socket := Parameter.Clock_Socket;
    loop
      select
        accept Finalize;
        exit;
      or
        accept Define_Time do
          if Ten_Micron.Gps_Is_Synchronized then
            Pc_Time_Offset := Duration((Time.Julian_Date - Ten_Micron.Julian_Date) / One_Second);
            Log.Write ("GPS is synchronized - PC time offset =" & Pc_Time_Offset'image);
          else
            if Parameter.Clock_Configured then
              declare
                Julian_Date     : constant Time.JD := Time.Julian_Date;
                Last_Minute     : Time.JD;
                The_Data        : Time_Server_Data;
                The_Actual_Date : Time.JD;
              begin
                Last_Minute := Time.JD(Long_Long_Integer(Julian_Date / One_Minute - Time.JD(0.5))) * One_Minute;
                Get (The_Data);
                if The_Data.One_Minute /= Undefined_Time then
                  The_Actual_Date := Last_Minute + The_Data.Delta_Time * One_Second;
                  if abs (The_Actual_Date - Julian_Date) > 30 * One_Second then
                    if The_Actual_Date > Julian_Date then
                      The_Actual_Date := @ - One_Minute;
                    else
                      The_Actual_Date := @ + One_Minute;
                    end if;
                  end if;
                  Ten_Micron.Set_Julian_Date (The_Actual_Date);
                  Pc_Time_Offset := Duration((Julian_Date - The_Actual_Date) / One_Second);
                  Log.Write ("external time set - PC time offset =" & Pc_Time_Offset'image);
                  if abs Pc_Time_Offset > Pc_Time_Tolerance then
                    Log.Warning ("PC time is inaccuarate");
                  end if;
                else
                  Ten_Micron.Set_Julian_Date (Time.Julian_Date);
                  Log.Warning ("external time undefined -> PC time set");
                end if;
              end;
            else
              Ten_Micron.Set_Julian_Date (Time.Julian_Date);
              Log.Write ("PC time set");
            end if;
          end if;
          Ten_Micron.Set_Time_Offset (Duration(Ada.Calendar.Time_Zones.UTC_Time_Offset) * 60.0);
        end Define_Time;
      end select;
    end loop;
    Network.Udp.Close (The_Socket);
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Control;


  procedure Start is
  begin
    Log.Write ("start");
    The_Control := new Control;
    The_Control.Start;
  end Start;


  procedure Define_Time is
  begin
    The_Control.Define_Time;
  end Define_Time;


  procedure Finish is
  begin
    Log.Write ("finish");
    The_Control.Finalize;
  end Finish;

end Clock;
