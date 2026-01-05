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
pragma Style_White_Elephant;

with Ada.Real_Time;
with Traces;

package body Focus is

  package Log is new Traces ("Focus");

  package RT renames Ada.Real_Time;

  task type Control is

    entry Start_Autofocus;

    entry Await_Stop;

    entry Shutdown;

  end Control;

  The_Control : access Control;


  procedure Start is
  begin
    The_Control := new Control;
  end Start;


  function Actual_Information return Information is
  begin
    return Focus_Data.Actual;
  end Actual_Information;


  function Focuser_Image return String is
  begin
    case Actual_Information.Focuser is
    when Unknown =>
      return "";
    when Celestron =>
      return "Celestron";
    end case;
  end Focuser_Image;


  procedure Evaluate is
  begin
    Log.Write ("Evaluate");
    The_Control.Start_Autofocus;
  exception
  when Occurrence: others =>
    Focus_Data.Set_Fatal (Occurrence);
  end Evaluate;


  procedure Stop is
  begin
    The_Control.Await_Stop;
  end Stop;


  function Error_Message return String is
  begin
    Focus_Data.Reset_Error;
    return Focus_Data.Last_Error;
  end Error_Message;


  procedure Finish is
  begin
    The_Control.Shutdown;
  end Finish;


  -------------
  -- Control --
  -------------

  task body Control is

    Delta_Time : constant RT.Time_Span := RT.To_Time_Span (1.0 / 2);

    use type RT.Time;

    The_Wakeup_Time : RT.Time := RT.Clock + Delta_Time;

    The_Count : Natural := 0;

  begin -- Control
    Log.Write ("start");
    Focus_Data.Set (Unknown);
    loop
      select
        accept Start_Autofocus;
        Focus_Data.Set (Evaluating);
        The_Count := 10;
      or
        accept Await_Stop;
        Log.Write ("Stopping");
        The_Count := 0;
      or
        accept Shutdown;
        exit;
      or
        delay until The_Wakeup_Time;
        The_Wakeup_Time := RT.Clock + Delta_Time;
        if The_Count = 0 then
          Focus_Data.Set (Undefined);
        elsif The_Count = 1 then
          Focus_Data.Set (Positioned);
        else
          The_Count := @ - 1;
        end if;
      end select;
    end loop;
    Log.Write ("finish");
  exception
  when Occurrance: others =>
    Log.Termination (Occurrance);
  end Control;


-------------
-- Private --
-------------

  -----------
  -- Error --
  -----------

  procedure Raise_Error (Message : String) is
  begin
    Log.Error (Message);
    Focus_Data.Set_Error (Message);
    raise Focus_Error;
  end Raise_Error;


  ----------
  -- Data --
  ----------
  protected body Focus_Data is

    procedure Set (State : Status) is
    begin
      if The_Information.State /= Error then
        The_Information.State := State;
      end if;
    end Set;


    procedure Set (Item : Focuser_Model) is
    begin
      The_Information.Focuser := Item;
    end Set;


    procedure Set (Start_Position : Distance) is
    begin
      The_Information.Position := Start_Position;
    end Set;


    procedure Set (Backlash : Lash) is
    begin
      The_Information.Backlash := Backlash;
    end Set;


    function Actual return Information is
    begin
      return The_Information;
    end Actual;


    procedure Check (Item : Status) is
    begin
      if The_Information.State /= Item then
        The_Last_Error := ["Sequence Error - State must be " & Item'image];
        The_Information.State := Error;
        raise Focus_Error;
      end if;
    end Check;


    procedure Set_Error (Message : String) is
    begin
      The_Last_Error := [Message];
      The_Information.State := Error;
    end Set_Error;


    procedure Set_Fatal (Item : Exceptions.Occurrence) is
    begin
      Set_Error ("Internal_Error - " & Exceptions.Name_Of (Item));
    end Set_Fatal;


    function Last_Error return String is
    begin
      return The_Last_Error.To_String;
    end Last_Error;


    procedure Reset_Error is
    begin
      The_Information.State := Undefined;
    end Reset_Error;

  end Focus_Data;

end Focus;
