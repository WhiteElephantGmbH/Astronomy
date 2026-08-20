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
pragma Style_Astronomy;

with GPIO;
with Ten_Micron;
with Time;
with Traces;

package body Clock is

  package Log is new Traces ("Clock");

  Clock_Accuracy : constant Time.JD_Seconds := 0.001;

  Pulse_Input : constant GPIO.Line := GPIO.Line27;

  task Handler is
    entry Shutdown;
  end Handler;


  procedure Shutdown is
  begin
    GPIO.Release (Pulse_Input);
    Handler.Shutdown;
  end Shutdown;

  -- no protection necessary
  Time_Set_From_Pc  : Boolean := False;
  Time_Synchronized : Boolean := False;

  function Is_Set_From_Pc return Boolean is (Time_Set_From_Pc);

  function Is_Synchronized return Boolean is (Time_Synchronized);


  function Synchronized_Mount return Boolean is
    The_Time : Time.JD;
   begin
    if Time_Synchronized then
      The_Time := Time.Julian_Date;
      Ten_Micron.Synchronize (The_Time);
      if Ten_Micron.Is_Synchronized then
        Log.Write ("Mount synchronized at " & Time.Image_Of (The_Time));
        return True;
      end if;
      Log.Warning ("Mount not synchronized");
    else
      Log.Warning ("Time not synchronized");
    end if;
    return False;
  end Synchronized_Mount;


  procedure Set (Pc_Time : String) is
  begin
    Time_Set_From_Pc := False;
    declare
      Julian_Date : constant Time.Unix_JD := Time.Unix_JD'value(Pc_Time);
    begin
      Time_Synchronized := False;
      Ten_Micron.Clear_Synchronized;
      Time.Set (Julian_Date);
      if Time.Is_Set then
        Time_Set_From_Pc := True;
        Log.Write ("Calendar set to Pc time " & Time.Image_Of (Julian_Date));
      end if;
    end;
  exception
  when others =>
    null;
  end Set;


  task body Handler is

    The_Time       : Time.JD;
    The_Synch_Time : Time.JD;
    The_Inaccuracy : Time.JD_Seconds;

    use type Time.JD;
    use type Time.JD_Seconds;

  begin -- Handler
    Log.Write ("Handler started");
    GPIO.Request_Dynamic_Input (Pulse_Input);
    loop
      GPIO.Await_Change_To_High (Pulse_Input);
      if Time.Is_Set then
        The_Time := Time.Julian_Date;
        if Time_Synchronized then
          The_Synch_Time := @ + Time.JD_Minute;
          The_Inaccuracy := abs Time.JD_Seconds_Of (The_Time - The_Synch_Time);
          if The_Inaccuracy > Clock_Accuracy then
            Time_Synchronized := False;
            Log.Write ("Time inaccuracy is" & The_Inaccuracy'image & " seconds");
          end if;
        else
          The_Synch_Time := Time.Rounded (The_Time, To_Nearest => Time.JD_Minute);
          Time.Set (The_Synch_Time);
          Time_Synchronized := True;
        end if;
        if Time_Synchronized then
          if Ten_Micron.Startup then
            Ten_Micron.Synchronize (The_Synch_Time);
            Log.Write ("First Time synchronized at " & Time.Image_Of (The_Synch_Time));
          else
            Log.Write ("Time synchronized at " & Time.Image_Of (The_Synch_Time));
          end if;
        end if;
      end if;
    end loop;
  exception
  when GPIO.Released =>
    accept Shutdown;
    Log.Write ("Handler terminated");
  when Item: others =>
    Log.Termination (Item);
    accept Shutdown;
  end Handler;

end Clock;
