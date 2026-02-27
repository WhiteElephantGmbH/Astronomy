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
with Focal;
with Focus.HFD;
with Focus.Evaluation;

package body Focus is

  package RT renames Ada.Real_Time;

  task type Control is

    entry Start_Autofocus;

    entry Await_Stop;

    entry Shutdown;

  end Control;

  The_Control : access Control;
  The_Focuser : Focuser.Object_Access;

  procedure Start (Device       : Focuser.Object_Access;
                   Is_Simulated : Boolean := False) is
  begin
    The_Focuser := Device;
    Is_Simulation := Is_Simulated;
    The_Control := new Control;
  end Start;


  function Actual_State return Status is
  begin
    return Focus_Data.State;
  end Actual_State;


  function Focuser_Image return String is
  begin
    case The_Focuser.State is
    when Focuser.Disconnected =>
      return "";
    when others =>
      return The_Focuser.Name;
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


  function Evaluation_Result return Result is
  begin
    return Focus_Data.Evaluation;
  end Evaluation_Result;


  procedure Stop is
  begin
    Camera.Stop;
    The_Focuser.Stop;
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

  The_Index : Positive; -- used in simulation;

  task body Control is

    HFD_Samples   : constant HFD_Sample_Count := The_HFD_Samples;
    HFD_Spread    : constant Positive := HFD_Samples / 2;
    Trigger_Level : constant Diameter := The_Trigger_Level;
    Minimum_Delta : constant Diameter := The_Minimum_Delta;

    The_HFD   : Evaluation.Vektor(1 .. HFD_Samples * Sample_Factor); -- stop after samples * 2

    Start_Position : constant Distance := The_Start_Position;
    Tolerance      : constant Distance := The_Tolerance;
    Position_Step  : constant Step := The_Position_Step;
    Max_Backlash   : constant Step := (Position_Step / abs Position_Step) * Step(Focal.Backlash'last);

    Grid_Size : constant Camera.Square_Size := The_Grid_Size;

    First_Index   : Positive;
    Minimum_Index : Positive;


    function Found_Minimum return Boolean is
      The_Minimum   : Diameter := Diameter'last;
      First_Maximum : Diameter := Diameter'first;
      Last_Maximum  : Diameter := Diameter'first;
      Last_Index    : Natural;
    begin
      Log.Write ("First_Index:" & First_Index'image);
      Log.Write ("The_Index:" & The_Index'image);
      for Index in First_Index .. The_Index loop
        if First_Maximum < The_HFD(Index) then
          First_Maximum := The_HFD(Index);
          First_Index := Index;
        elsif First_Maximum > Trigger_Level then
          exit;
        end if;
      end loop;
      Log.Write ("First_Maximum:" & First_Maximum'image);
      Log.Write ("First_Index:" & First_Index'image);
      if First_Maximum <= Trigger_Level then
        return False;
      end if;
      for Index in First_Index .. The_Index loop
        if The_Minimum > The_HFD(Index) then
          The_Minimum := The_HFD(Index);
          Minimum_Index := Index;
        end if;
      end loop;
      Log.Write ("The_Minimum:" & The_Minimum'image);
      Log.Write ("Minimum_Index:" & Minimum_Index'image);
      for Index in Minimum_Index .. The_Index loop
        if Last_Maximum < The_HFD(Index) then
          Last_Maximum := The_HFD(Index);
          Last_Index := Index;
        end if;
      end loop;
      Log.Write ("Last_Maximum:" & Last_Maximum'image);
      Log.Write ("Last_Index:" & Last_Index'image);
      return (Minimum_Index - First_Index) >= HFD_Spread and (Last_Index - Minimum_Index) >= HFD_Spread and
             (First_Maximum - The_Minimum) > Minimum_Delta and (Last_Maximum - The_Minimum) > Minimum_Delta;
    end Found_Minimum;


    function Focus_Position return Distance is
      First         : constant Positive := Minimum_Index - HFD_Spread;
      Last          : constant Positive := Minimum_Index + HFD_Spread;
      From_Position : constant Distance := Start_Position + (First - The_HFD'first) * Position_Step;
    begin
      return Evaluation.Best_For (Start_Position => From_Position,
                                  Position_Step  => Position_Step,
                                  HFD_Array      => The_HFD(First .. Last));
    exception
    when others =>
      Raise_Error ("Focus calculation failed");
    end Focus_Position;


    use type RT.Time;
    use type Focuser.Status;

    Delta_Time : constant RT.Time_Span := RT.To_Time_Span (1.0 / 2);

    The_Wakeup_Time : RT.Time := RT.Clock + Delta_Time;

    The_Position : Distance;

    procedure Start_Evaluation is
    begin
      The_Index := The_HFD'first;
      First_Index := The_HFD'first;
      The_Position := Start_Position;
      if The_Position = Start_From_Actual then
        The_Position := The_Focuser.Actual_Position;
      end if;
      The_Focuser.Move_To (The_Position - Max_Backlash);
      The_Wakeup_Time := RT.Clock + Delta_Time;
      Focus_Data.Set (Starting);
    end Start_Evaluation;


    procedure Evaluate_Position is
      Actual_Position : constant Distance := The_Position;
      Actual_HFD      : constant Diameter := Focus_Data.Evaluation.HFD;
    begin
      if Actual_HFD = Focus.HFD_Not_Found then
        Log.Warning ("HFD not found at" & Actual_Position'image);
        Focus_Data.Set (Positioning);
        return;
      end if;
      Log.Write ("Evaluate_Position:" & Actual_Position'image);
      The_HFD(The_Index) := Actual_HFD;
      if The_Index >= HFD_Samples and then Found_Minimum then
        The_Position := Focus_Position - Max_Backlash;
        The_Focuser.Move_To (The_Position);
        Focus_Data.Set (Evaluated);
      else
        if The_Index = The_HFD'last then
          Raise_Error ("Focus position not found");
        end if;
        The_Index := @ + 1;
        The_Position := Actual_Position + Position_Step;
        The_Focuser.Move_To (The_Position);
        Focus_Data.Set (Positioning);
      end if;
    exception
    when Focus_Error =>
      null;
    end Evaluate_Position;


    function At_Position (Focuser_Position : Distance) return Boolean is
    begin
      return abs (Integer(The_Position) - Integer(Focuser_Position)) <= Tolerance;
    end At_Position;

  begin -- Control
    Log.Write ("start");
    loop
      select
        accept Start_Autofocus;
        case The_Focuser.State is
        when Focuser.Stopped | Focuser.Moving =>
          Start_Evaluation;
        when Focuser.Disconnected =>
          Set_Error ("Focuser not connected");
        end case;
      or
        accept Await_Stop do
          Log.Write ("Stopping");
          case The_Focuser.State is
          when Focuser.Moving =>
            The_Focuser.Stop;
          when Focuser.Stopped =>
            Focus_Data.Set (Undefined);
          when Focuser.Disconnected =>
            Focus_Data.Set (No_Focuser);
          end case;
          Camera.Stop;
          Log.Write ("Stopped");
        end Await_Stop;
      or
        accept Shutdown;
        exit;
      or
        delay until The_Wakeup_Time;
        begin
          case Focus_Data.State is
          when No_Focuser =>
            case The_Focuser.State is
            when Focuser.Stopped | Focuser.Moving =>
              Focus_Data.Set (Undefined);
            when Focuser.Disconnected =>
              null;
            end case;
          when Starting =>
            case The_Focuser.State is
            when Focuser.Stopped =>
              The_Focuser.Move_To (The_Position);
              Focus_Data.Set (Positioning);
            when Focuser.Moving =>
              null;
            when Focuser.Disconnected =>
              Set_Error ("Focuser Disconnected");
            end case;
          when Positioning =>
            case The_Focuser.State is
            when Focuser.Stopped =>
              declare
                Actual_Position : constant Distance := The_Focuser.Actual_Position;
              begin
                if At_Position (Actual_Position) then
                  Camera.Capture (Grid_Size);
                  if Camera.Has_Error then
                    Set_Error (Camera.Error_Message);
                  else
                    Focus_Data.Set (Capturing);
                  end if;
                else
                  Set_Error ("Positioning inaccurate - expected:" & The_Position'image &
                                                   " - actual:" & Actual_Position'image);
                end if;
              end;
            when Focuser.Moving =>
              null;
            when Focuser.Disconnected =>
              Set_Error ("Focuser Disconnected");
            end case;
          when Capturing =>
            case Camera.Actual_Information.State is
            when Camera.Cropped =>
              HFD.Evaluate (Camera.Captured);
              declare
                Actual_Position : constant Distance := The_Focuser.Actual_Position;
              begin
                if At_Position (Actual_Position) then
                  Evaluate_Position;
                else
                  Set_Error ("Positioning moved - expected:" & The_Position'image &
                                              " - actual:" & Actual_Position'image);
                end if;
              end;
            when Camera.Failed =>
              Set_Error (Camera.Error_Message);
            when others =>
              null;
            end case;
          when Evaluated =>
            case The_Focuser.State is
            when Focuser.Stopped =>
              declare
                Actual_Position : constant Distance := The_Focuser.Actual_Position;
              begin
                if At_Position (Actual_Position) then
                  The_Position := @ + Max_Backlash;
                  The_Focuser.Move_To (The_Position);
                  Focus_Data.Set (Focused);
                end if;
              end;
            when Focuser.Moving =>
              null;
            when Focuser.Disconnected =>
              Focus_Data.Set (No_Focuser);
            end case;
          when Focused =>
            case The_Focuser.State is
            when Focuser.Stopped =>
              if not At_Position (The_Focuser.Actual_Position) then
                Focus_Data.Set (Undefined);
              end if;
            when Focuser.Moving =>
              null;
            when Focuser.Disconnected =>
              Focus_Data.Set (No_Focuser);
            end case;
          when Undefined =>
            case The_Focuser.State is
            when Focuser.Stopped | Focuser.Moving =>
              null;
            when Focuser.Disconnected =>
              Focus_Data.Set (No_Focuser);
            end case;
          when Failed =>
            null;
          end case;
        exception
        when Focus_Error =>
          null;
        when Occurrence: others =>
          Focus_Data.Set_Fatal (Occurrence);
        end;
        The_Wakeup_Time := RT.Clock + Delta_Time;
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

  --------------------
  -- Error Handling --
  --------------------

  procedure Set_Error (Message : String) is
  begin
    Focus_Data.Set_Failed (Message);
  end Set_Error;


  procedure Raise_Error (Message : String) is
  begin
    Set_Error (Message);
    raise Focus_Error;
  end Raise_Error;


  ----------
  -- Data --
  ----------

  function Minimum_Start_Position return Distance is
    Minimum_Position : constant Distance := 500;
  begin
    pragma Assert (Minimum_Position > Distance(Focal.Backlash'last));
    if The_Position_Step < 0 then
      return Minimum_Position + (abs The_Position_Step * The_HFD_Samples * Sample_Factor);
    else
      return Minimum_Position;
    end if;
  end Minimum_Start_Position;


  function Simulated_HFD return Diameter is
    Data : constant Evaluation.Vektor
         := [130, 122, 121, 116, 114, 111, 107, 105, 104, 118, 109, 108, 107, 108, 105, 115, 125, 128, 130, 138, 143];
  begin
    if not Is_Simulation then
      raise Program_Error;
    elsif The_Index <= Data'last then
      return Data(The_Index);
    end if;
    return Data(Data'last);
  end Simulated_HFD;


  protected body Focus_Data is

    procedure Set (Item : Status) is
    begin
      if The_State /= Failed then
        The_State := Item;
      end if;
    end Set;


    function State return Status is
    begin
      return The_State;
    end State;


    procedure Set (Half_Flux : Camera.Pixel) is
    begin
      The_Result.Half_Flux := Half_Flux;
    end Set;


    procedure Set (Half_Flux_Diameter : Diameter) is
    begin
      The_Result.HFD := Half_Flux_Diameter;
    end Set;


    procedure Set (Position : Distance) is
    begin
      The_Result.Position := Position;
    end Set;


    function Evaluation return Result is
    begin
      return The_Result;
    end Evaluation;


    procedure Set_Failed (Message : String) is
    begin
      The_Result := (others => <>);
      The_Last_Error := [Message];
      The_State := Failed;
    end Set_Failed;


    procedure Check (Item : Status) is
    begin
      if The_State /= Item then
        Set_Error ("Sequence Error - State must be " & Item'image);
        raise Focus_Error;
      end if;
    end Check;


    procedure Set_Fatal (Item : Exceptions.Occurrence) is
      Message : constant String := Exceptions.Name_Of (Item);
    begin
      Log.Error (Exceptions.Information_Of (Item));
      Set_Error ("Internal_Error - " & Message);
    end Set_Fatal;


    function Last_Error return String is
    begin
      return The_Last_Error.S;
    end Last_Error;


    procedure Reset_Error is
    begin
      The_State := No_Focuser;
    end Reset_Error;

  end Focus_Data;

end Focus;
