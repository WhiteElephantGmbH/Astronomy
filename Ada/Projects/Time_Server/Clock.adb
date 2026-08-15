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
with Traces;

package body Clock is

  package Log is new Traces ("Clock");

  Clock_Accuracy : constant Duration := Time.One_Second / 1000.0;

  Pulse_Input : constant GPIO.Line := GPIO.Line27;

  task Handler is
    entry Shutdown;
  end Handler;


  procedure Shutdown is
  begin
    GPIO.Release (Pulse_Input);
    Handler.Shutdown;
  end Shutdown;


  Time_Set_From_Pc  : Boolean := False;
  Time_Synchronized : Boolean := False;

  function Is_Set_From_Pc return Boolean is (Time_Set_From_Pc);

  function Is_Synchronized return Boolean is (Time_Synchronized);


  function Synchronized_Mount return Boolean is
    The_Time   : Time.JD;
    Mount_Time : Time.JD;
   begin
    if Time_Synchronized then
      The_Time := Time.Julian_Date;
      Ten_Micron.Set (The_Time);
      if Ten_Micron.Has_New (Mount_Time) then
        Log.Write ("Mount Set at " & Time.Image_Of (The_Time));
        Log.Write ("Mount Get at " & Time.Image_Of (Mount_Time));
        return True;
      end if;
      Log.Warning ("Mount not synchronized");
    else
      Log.Warning ("Time not synchronized");
    end if;
    return False;
  end Synchronized_Mount;


  function Set (Item : Time.JD) return Boolean is
  begin
    return True;
  exception
  when others =>
    Log.Error ("Set to time " & Time.Image_Of (Item) & " failed");
    return False;
  end Set;


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

    procedure Round_To_Nearest_Minute (The_Time : in out Time.JD) is
    begin
      The_Time := Time.Rounded (The_Time, To_Nearest => Time.JD_Minute);
    end Round_To_Nearest_Minute;

    The_Time       : Time.JD;
    Last_Time      : Time.JD := Time.JD_Undefined;
    The_Inaccuracy : Duration;

    use type Time.JD;

  begin -- Handler
    Log.Write ("Handler started");
    GPIO.Request_Dynamic_Input (Pulse_Input);
    loop
      GPIO.Await_Change_To_High (Pulse_Input);
      if Time.Is_Set then
        The_Time := Time.Julian_Date;
        if not Time_Synchronized then
          Round_To_Nearest_Minute (The_Time);
          Time.Set (The_Time);
          Time_Synchronized := True;
          Log.Write ("Time synchronized at " & Time.Image_Of (The_Time));
        else
          The_Inaccuracy := abs (Duration((The_Time - Last_Time) / Time.JD_Second) - Time.One_Minute);
          if The_Inaccuracy > Clock_Accuracy then
            Time_Synchronized := False;
            Log.Write ("Time inaccuracy is" & The_Inaccuracy'image);
          end if;
        end if;
        Last_Time := The_Time;
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
