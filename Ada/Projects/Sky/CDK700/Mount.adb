-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with PWI.Mount;
with Traces;

package body Mount is

  package Log is new Traces ("Mount");

  type Command is (Finish, Stop, Connect, Disconnect, Enable, Disable, Find_Home, Set_Pointing_Model);


  protected Action is

    procedure Put (Item : Command);

    entry Get (Item : out Command);

  private
    Is_Pending  : Boolean := False;
    The_Command : Command;
  end Action;


  protected body Action is

    procedure Put (Item : Command) is
    begin
      The_Command := Item;
      Is_Pending := True;
    end Put;

    entry Get (Item : out Command) when Is_Pending is
    begin
      Item := The_Command;
      Is_Pending := False;
    end Get;

  end Action;


  task type Control is

    entry Start (Item          : State_Handler_Access;
                 Is_Simulation : Boolean);

  end Control;

  The_Control : access Control;


  task body Control is

    The_State_Handler : State_Handler_Access;
    Is_Simulating     : Boolean;

    The_Command : Command;
    The_State   : State := Unknown;

    use type PWI.Mount.State;

  begin
    accept Start (Item : State_Handler_Access;
                  Is_Simulation : Boolean)
    do
      The_State_Handler := Item;
      Is_Simulating := Is_Simulation;
    end Start;
    Log.Write ("Control started" & (if Is_Simulating then " Simulation" else ""));
    loop
      begin
        select
          Action.Get (The_Command);
          case The_Command is
          when Finish =>
            exit;
          when Stop =>
            PWI.Mount.Stop;
          when Connect =>
            PWI.Mount.Connect;
            if Is_Simulating then
              The_State := Connected;
            end if;
          when Disconnect =>
            PWI.Mount.Disconnect;
            if Is_Simulating then
              The_State := Disconnected;
            end if;
          when Enable =>
            PWI.Mount.Enable;
            if Is_Simulating then
              The_State := Enabled;
            end if;
          when Disable =>
            PWI.Mount.Disable;
            if Is_Simulating then
              The_State := Connected;
            end if;
          when Find_Home =>
            PWI.Mount.Find_Home;
            if Is_Simulating then
              The_State := Synchronised;
            end if;
          when Set_Pointing_Model =>
            PWI.Mount.Set_Pointing_Model;
            if Is_Simulating then
              The_State := Stopped;
            end if;
          end case;
        or
          delay 1.0;
          PWI.Get_System;
          case PWI.Mount.Status is
          when PWI.Mount.Disconnected =>
            The_State := Disconnected;
          when PWI.Mount.Connected =>
            The_State := Connected;
          when PWI.Mount.Enabled =>
            The_State := Enabled;
          when PWI.Mount.Homing =>
            The_State := Homing;
          when PWI.Mount.Synchronised =>
            The_State := Synchronised;
          when PWI.Mount.Stopped =>
            The_State := Stopped;
          when PWI.Mount.Approaching =>
            The_State := Approaching;
          when PWI.Mount.Tracking =>
            The_State := Tracking;
          end case;
        end select;
      exception
      when PWI.No_Server =>
        The_State := Unknown;
      end ;
      The_State_Handler (The_State);
    end loop;
    Log.Write ("Control terminated");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Control;


  procedure Start (State_Handler : State_Handler_Access;
                   Is_Simulation : Boolean) is
  begin
    The_Control := new Control;
    The_Control.Start (State_Handler, Is_Simulation);
  end Start;


  function Image_Of (The_Direction : Space.Direction) return String is
  begin
    if Space.Direction_Is_Known (The_Direction) then
      return "RA: " & Space.Ra_Image_Of (The_Direction) & " - DEC: " & Space.Dec_Image_Of (The_Direction);
    end if;
    return "Unknown";
  end Image_Of;


  function Image_Of (The_Direction : Earth.Direction) return String is
  begin
    if Earth.Direction_Is_Known (The_Direction) then
      return "AZ: " & Earth.Az_Image_Of (The_Direction) & " - ALT: " & Earth.Alt_Image_Of (The_Direction);
    end if;
    return "Unknown";
  end Image_Of;


  procedure Connect is
  begin
    Log.Write ("Connect");
    Action.Put (Connect);
  end Connect;


  procedure Disconnect is
  begin
    Log.Write ("Disconnect");
    Action.Put (Disconnect);
  end Disconnect;


  procedure Enable is
  begin
    Log.Write ("Enable");
    Action.Put (Enable);
  end Enable;


  procedure Disable is
  begin
    Log.Write ("Disable");
    Action.Put (Disable);
  end Disable;


  procedure Find_Home is
  begin
    Log.Write ("Find_Home");
    Action.Put (Find_Home);
  end Find_Home;


  procedure Set_Pointing_Model is
  begin
    Log.Write ("Set_Pointing_Model");
    Action.Put (Set_Pointing_Model);
  end Set_Pointing_Model;


  procedure Goto_Target (Direction : Space.Direction) is
  begin
    Log.Write ("Goto_Target " & Image_Of (Direction));
  end Goto_Target;


  procedure Goto_Mark (Direction : Earth.Direction) is
  begin
    Log.Write ("Goto_Mark " & Image_Of (Direction));
  end Goto_Mark;


  procedure Direct (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed) is
    pragma Unreferenced (With_Speed);
  begin
    Log.Write ("Direct " & The_Drive'img);
  end Direct;


  procedure Adjust (The_Drive  : Device.Drive;
                    With_Speed : Angle.Signed) is
    pragma Unreferenced (With_Speed);
  begin
    Log.Write ("Adjust " & The_Drive'img);
  end Adjust;


  procedure Stop is
  begin
    Log.Write ("Stop");
    Action.Put (Stop);
  end Stop;


  function Actual_Direction return Space.Direction is
  begin
    Log.Write ("Actual_Direction Unknown");
    return Space.Unknown_Direction;
  end Actual_Direction;


  procedure Finish is
  begin
    Action.Put (Finish);
  end Finish;

end Mount;
