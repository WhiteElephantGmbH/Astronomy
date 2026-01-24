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
with Focus.HFD;

package body Focus is

  package RT renames Ada.Real_Time;

  task type Control is

    entry Start_Autofocus;

    entry Await_Stop;

    entry Shutdown;

  end Control;

  The_Control : access Control;
  The_Focuser : Focuser.Object_Access;

  procedure Start (Device : Focuser.Object_Access) is
  begin
    The_Focuser := Device;
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

  task body Control is

    use type RT.Time;
    use type Focuser.Status;

    Delta_Time : constant RT.Time_Span := RT.To_Time_Span (1.0 / 2);

    The_Wakeup_Time : RT.Time := RT.Clock + Delta_Time;

    Minimum_Start_HFD : constant Diameter := 50;

    type Data is record
      Position : Distance;
      HFD      : Diameter;
    end record;

    type Index is (First, Second, Third, Last);

    The_Data     : array (Index) of Data;
    The_Index    : Index;
    The_Position : Distance;


    function Inside_Position (Fa, Fb : Distance;
                              Da, Db : Diameter) return Distance is
      P : Focus.Distance;
    begin
      Log.Write ("Inside - Fa:" & Fa'image & " - Fb:" & Fb'image);
      Log.Write ("       - Da:" & Da'image & " - Db:" & Db'image);
      P := Fa + Distance (Natural(Fb - Fa) * Natural(Da) / (Natural(Da + Db)));
      Log.Write ("P:" & P'image);
      return P;
    exception
    when others =>
      Raise_Error ("Inside position calculation failed");
    end Inside_Position;


    function Outside_Position (Fa, Fb : Focus.Distance;
                               Da, Db : Diameter) return Focus.Distance is
      P : Distance;
    begin
      Log.Write ("Outside - Fa:" & Fa'image & " - Fb:" & Fb'image);
      Log.Write ("        - Da:" & Da'image & " - Db:" & Db'image);
      if Da > Db then
        P := Fb + Distance (Natural(Fb - Fa) * Natural(Da) / (Natural(Da - Db)));
        P := Fa + 2 * (P - Fa);
      else
        Raise_Error ("Start position too high");
      end if;
      Log.Write ("P:" & P'image);
      return P;
    exception
    when Focus_Error =>
      raise;
    when others =>
      Raise_Error ("Outside position calculation failed");
    end Outside_Position;


    procedure Start_Evaluation is
    begin
      The_Index := First;
      The_Position := Focus_Data.Start_Position;
      if The_Position = Start_From_Actual then
        The_Position := The_Focuser.Actual_Position;
      else
        The_Focuser.Move_To (The_Position);
        The_Wakeup_Time := RT.Clock + Delta_Time;
      end if;
      Focus_Data.Set (Positioning);
    end Start_Evaluation;


    procedure Evaluate_Position is
      Actual_HFD      : constant Diameter := Focus_Data.Evaluation.HFD;
      Actual_Position : constant Distance := The_Position;
    begin
      The_Data(The_Index).HFD := Actual_HFD;
      The_Data(The_Index).Position := Actual_Position;
      case The_Index is
      when First =>
        if Actual_HFD > Minimum_Start_HFD then -- first star is visible
          Log.Write ("First HFD:" & Actual_HFD'image);
          The_Index := Second;
        end if;
        The_Position := Actual_Position + Focus_Data.Start_Increment;
        The_Focuser.Move_To (The_Position);
        Focus_Data.Set (Positioning);
      when Second =>
        The_Position := Outside_Position (Fa => The_Data(First).Position,
                                          Fb => The_Data(Second).Position,
                                          Da => The_Data(First).HFD,
                                          Db => The_Data(Second).HFD);
        The_Index := Third;
        The_Focuser.Move_To (The_Position);
        Focus_Data.Set (Positioning);
      when Third =>
        The_Position := Inside_Position (Fa => The_Data(First).Position,
                                         Fb => The_Data(Third).Position,
                                         Da => The_Data(First).HFD,
                                         Db => The_Data(Third).HFD);
        The_Focuser.Move_To (The_Position);
        Focus_Data.Set (Positioning);
        The_Index := Last;
      when Last =>
        Focus_Data.Set (The_Position);
        Focus_Data.Set (Evaluated);
      end case;
    exception
    when Focus_Error =>
      null;
    end Evaluate_Position;


    function At_Position (Focuser_Position : Distance) return Boolean is
    begin
      return abs (Integer(The_Position) - Integer(Focuser_Position)) <= Focus_Data.Position_Tolerance;
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
          when Positioning =>
            case The_Focuser.State is
            when Focuser.Stopped =>
              declare
                Actual_Position : constant Distance := The_Focuser.Actual_Position;
              begin
                if At_Position (Actual_Position) then
                  Camera.Capture (Focus_Data.Grid_Size);
                  if Camera.Has_Error then
                    Set_Error (Camera.Error_Message);
                  else
                    Focus_Data.Set (Capturing);
                  end if;
                else
                  Set_Error ("Focuser positioning inaccurate - expected:" & The_Position'image &
                                                       " - actual:" & Actual_Position'image);
                end if;
              end;
            when Focuser.Moving =>
              null;
            when Focuser.Disconnected =>
              Set_Error ("Focuser lost connection");
            end case;
          when Capturing =>
            case Camera.Actual_Information.State is
            when Camera.Cropped =>
              HFD.Evaluate (Camera.Captured);
              if The_Position /= The_Focuser.Actual_Position then
                Set_Error ("Focuser position moved");
              else
                Evaluate_Position;

              end if;
            when Camera.Failed =>
              Set_Error (Camera.Error_Message);
            when others =>
              null;
            end case;
          when Evaluated =>
            case The_Focuser.State is
            when Focuser.Stopped =>
              if not At_Position (The_Focuser.Actual_Position) then
                Focus_Data.Set (Undefined);
              end if;
            when Focuser.Moving =>
              Focus_Data.Set (Undefined);
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

  protected body Focus_Data is

    procedure Set (Item : Status) is
    begin
      if The_State /= Failed then
        The_State := Item;
      end if;
    end Set;


    procedure Set (First_Position  : Distance;
                   First_Increment : Distance;
                   Tolerance       : Distance;
                   Square_Size     : Camera.Square_Size) is
    begin
      The_Start_Position := First_Position;
      The_Start_Increment := First_Increment;
      The_Tolerance := Tolerance;
      The_Grid_Size := Square_Size;
    end Set;


    function State return Status is
    begin
      return The_State;
    end State;


    function Start_Position return Distance is
    begin
      return The_Start_Position;
    end Start_Position;


    function Start_Increment return Distance is
    begin
      return The_Start_Increment;
    end Start_Increment;


    function Position_Tolerance return Distance is
    begin
      return The_Tolerance;
    end Position_Tolerance;


    function Grid_Size return Camera.Square_Size is
    begin
      return The_Grid_Size;
    end Grid_Size;


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
      Log.Error (Message);
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
