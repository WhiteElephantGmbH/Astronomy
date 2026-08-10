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

with Ada.Real_Time;
with Ada.Text_IO;
with Exceptions;
with GPIO;
with Ten_Micron;
with Time;
with Traces;

package body Clock is

  package Log is new Traces ("Clock");

  package RT renames Ada.Real_Time;

  Pulse_Input : constant GPIO.Line := GPIO.Line27;

  task Handler is
    entry Shutdown;
  end Handler;


  procedure Shutdown is
  begin
    GPIO.Release (Pulse_Input);
    Handler.Shutdown;
  end Shutdown;


  The_Data : Data;

  Do_Synchronize : Boolean := False;


  function Synchronize_Mount_Started return Boolean is
  begin
    if The_Data.Is_Synchronized and The_Data.Mount_Connected then
      The_Data.Mount_Synchronized := False;
      Do_Synchronize := True;
      Ada.Text_IO.Put_Line ("<<< Synchronize Mount Started >>>");
      return True;
    else
      Ada.Text_IO.Put_Line ("<<< Synchronize Mount NOT Started >>>");
      return False;
    end if;
  end Synchronize_Mount_Started;


  function Information return Data is
  begin
    return The_Data;
  end Information;


  task body Handler is

    procedure Round_To_Nearest_Second (The_Time : in out Time.JD) is
    begin
      The_Time := Time.Rounded (The_Time, To_Nearest => Time.JD_Second);
    end Round_To_Nearest_Second;

    procedure Round_To_Nearest_Minute (The_Time : in out Time.JD) is
    begin
      The_Time := Time.Rounded (The_Time, To_Nearest => Time.JD_Minute);
    end Round_To_Nearest_Minute;

    The_Time               : Time.JD;
    Falling_Edge_Time      : RT.Time;
    Last_Falling_Edge_Time : RT.Time := RT.Time_First;
    Pulse_Duration         : Duration;
    Is_Minute_Change       : Boolean := False;

    use type RT.Time;

  begin -- Handler
    Log.Write ("Handler started");
    GPIO.Request_Dynamic_Input (Pulse_Input);
    loop
      GPIO.Await_Change_To_Low (Pulse_Input);
      Falling_Edge_Time := RT.Clock;
      if Ten_Micron.Has_New (The_Time) then
        if not The_Data.Mount_Connected then
          The_Data.Mount_Connected := True;
          Time.Set (The_Time);
          Ada.Text_IO.Put_Line ("Mount Time : " & Time.Image_Of (Time.Ut_Of (The_Time)) & " - " & The_Time'image);
          Ada.Text_IO.Put_Line ("Actual Time: " & Time.Image_Of (Time.Universal)  & " - " & Time.Julian_Date'image);
        end if;
      else
        The_Data.Mount_Connected := False;
        The_Data.Mount_Synchronized := False;
      end if;
      if The_Data.Exists then
        Pulse_Duration := RT.To_Duration (Falling_Edge_Time - Last_Falling_Edge_Time);
        Is_Minute_Change := Pulse_Duration > 1.5;
      end if;
      Last_Falling_Edge_Time := Falling_Edge_Time;
      GPIO.Await_Change_To_High (Pulse_Input);
      The_Time := Time.Julian_Date;
      if Is_Minute_Change and The_Data.Mount_Connected then
        Round_To_Nearest_Minute (The_Time);
        Time.Set (The_Time);
        Ada.Text_IO.Put_Line ("Synchronized Time : " & Time.Image_Of (Time.Ut_Of (The_Time)) & " - " & The_Time'image);
        The_Data.Is_Synchronized := True;
      elsif The_Data.Exists then
        Round_To_Nearest_Second (The_Time);
        if Do_Synchronize and The_Data.Is_Synchronized then
          if Ten_Micron.Set (The_Time) then
            The_Data.Mount_Synchronized := True;
            Ada.Text_IO.Put_Line ("Mount Time Set : " & Time.Image_Of (Time.Ut_Of (The_Time)) & " - " & The_Time'image);
            Do_Synchronize := False;
          end if;
        end if;
      end if;
      The_Data.Exists := True;
    end loop;
  exception
  when GPIO.Released =>
    accept Shutdown;
    Log.Write ("Handler terminated");
  when Item: others =>
    Ada.Text_IO.Put_Line ("Clock.Handler.Exception: " & Exceptions.Information_Of (Item));
    Log.Termination (Item);
    accept Shutdown;
  end Handler;

end Clock;
