-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Real_Time;
with Traces;

package body Progress is

  package Log is new Traces ("Progress");

  package RT renames Ada.Real_Time;

  The_Startup_Time      : RT.Time;
  The_Progress_Duration : Duration := 0.0;


  procedure Start (The_Duration : Duration) is
    use type RT.Time;
  begin
    Log.Write ("Start with duration " & The_Duration'image & " seconds");
    The_Progress_Duration := The_Duration;
    The_Startup_Time := RT.Clock + RT.To_Time_Span (The_Progress_Duration);
  end Start;


  function In_Percent return Percent is
  begin
    if Is_Active then
      declare
        use type RT.Time;
        Remaining_Duration : constant Duration := RT.To_Duration(The_Startup_Time - RT.Clock);
      begin
        if Remaining_Duration <= 0.0 then
          The_Progress_Duration := 0.0; -- set inactive
          return Percent'last;
        else
          Log.Write ("Remaining_Duration: " & Remaining_Duration'image);
          return Percent((The_Progress_Duration - Remaining_Duration) * Natural(Percent'last) / The_Progress_Duration);
        end if;
      end;
    else
      return Percent'first;
    end if;
  end In_Percent;


  function Is_Active return Boolean is
  begin
    return The_Progress_Duration /= 0.0;
  end Is_Active;

end Progress;
