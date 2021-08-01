-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with Application;
with Data;
with Error;
with Gui;
with Handbox;
with Horizon;
with Lx200_Server;
with Moon;
with Name;
with Neo;
with Network.Tcp;
with Os.Application;
with Parameter;
with Os.Process;
with Sky_Line;
with Solar_System;
with Space;
with Sssb;
with Stellarium;
with Targets;
with Telescope;
with Time;
with Traces;
with User;

package body Control is

  package Log is new Traces ("Control");

  task type Manager is
  end Manager;

  type Command is (Define_Catalog,
                   Define_Target,
                   Startup,
                   Shutdown,
                   Stop,
                   Go_To,
                   Back,
                   Set_Orientation,
                   New_Goto_Direction,
                   Update,
                   New_Telescope_Information,
                   Close);

  protected Action_Handler is
    procedure Put_Goto (The_Direction : Space.Direction);
    procedure Put (The_Action : User.Action);
    procedure Signal_New_Telescope_Data;
    function New_Direction return Space.Direction;
    entry Get (The_Command : out Command);
    procedure Enable_Termination;
    entry Wait_For_Termination;
  private
    The_New_Direction       : Space.Direction;
    Next_Command            : Command;
    Has_New_Telescope_Data  : Boolean := False;
    Has_New_Goto_Direction  : Boolean := False;
    Define_Catalog_Pending  : Boolean := False;
    Define_Target_Pending   : Boolean := False;
    Update_Pending          : Boolean := False;
    Command_Is_Pending      : Boolean := False;
    Termination_Is_Enabled  : Boolean := False;
  end Action_Handler;


  procedure Goto_Handler (The_Direction : Space.Direction) is
    The_Target : Name.Id;
    use type Name.Id;
  begin
    Targets.Get_For (The_Direction, The_Target);
    if The_Target = Name.No_Id then
      Action_Handler.Put_Goto (The_Direction);
    else
      User.Set (The_Target);
    end if;
  end Goto_Handler;


  procedure User_Action_Handler (The_Action : User.Action) is
  begin
    Action_Handler.Put (The_Action);
  end User_Action_Handler;


  protected body Action_Handler is

    procedure Put (The_Action : User.Action) is
    begin
      if Next_Command = Close then
        return;
      end if;
      case The_Action is
      when User.Define_Catalog =>
        Define_Catalog_Pending := True;
      when User.Define_Target =>
        Define_Target_Pending := True;
      when User.Startup =>
        Next_Command := Startup;
        Command_Is_Pending := True;
      when User.Shutdown =>
        Next_Command := Shutdown;
        Command_Is_Pending := True;
      when User.Go_To =>
        Next_Command := Go_To;
        Command_Is_Pending := True;
      when User.Back =>
        Next_Command := Back;
        Command_Is_Pending := True;
      when User.Stop =>
        Next_Command := Stop;
        Command_Is_Pending := True;
      when User.Update =>
        Update_Pending := True;
      when User.Set_Orientation =>
        Next_Command := Set_Orientation;
        Command_Is_Pending := True;
      when User.Close =>
        Next_Command := Close;
        Command_Is_Pending := True;
      end case;
    end Put;

    procedure Put_Goto (The_Direction : Space.Direction) is
    begin
      The_New_Direction := The_Direction;
      if Next_Command /= Close then
        Has_New_Goto_Direction := True;
      end if;
    end Put_Goto;

    procedure Signal_New_Telescope_Data is
    begin
      if Next_Command /= Close then
        Has_New_Telescope_Data := True;
      end if;
    end Signal_New_Telescope_Data;

    entry Get (The_Command : out Command)
      when not Termination_Is_Enabled and then
        (Has_New_Goto_Direction or
         Command_Is_Pending or
         Define_Catalog_Pending or
         Update_Pending or
         Define_Target_Pending or
         Has_New_Telescope_Data)
    is
    begin
      if Next_Command = Close then
        The_Command := Next_Command;
        Command_Is_Pending := False;
      elsif Define_Catalog_Pending then
        The_Command := Define_Catalog;
        Define_Catalog_Pending := False;
      elsif Update_Pending then
        The_Command := Update;
        Update_Pending := False;
      elsif Define_Target_Pending then
        The_Command := Define_Target;
        Define_Target_Pending := False;
      elsif Has_New_Goto_Direction then
        The_Command := New_Goto_Direction;
        Has_New_Goto_Direction := False;
      elsif Has_New_Telescope_Data then
        The_Command := New_Telescope_Information;
        Has_New_Telescope_Data := False;
      else
        The_Command := Next_Command;
        Command_Is_Pending := False;
      end if;
    end Get;

    function New_Direction return Space.Direction is
    begin
      return The_New_Direction;
    end New_Direction;

    procedure Enable_Termination is
    begin
      Termination_Is_Enabled := True;
    end Enable_Termination;

    entry Wait_For_Termination when Termination_Is_Enabled is
    begin
      null;
    end Wait_For_Termination;

  end Action_Handler;


  New_Target_Direction : Space.Direction;

  function Target_Direction_Of (Unused_Id : Name.Id;
                                Unused_Ut : Time.Ut) return Space.Direction is
  begin
    return New_Target_Direction;
  end Target_Direction_Of;


  task body Manager is

    The_Command : Command;

    The_Data : Telescope.Data;

    The_Neo_Target : Name.Id;
    The_Landmark   : Name.Id;

    The_Completion_Time : Time.Ut := Time.In_The_Past;


    function Tracking_Period return Time.Period is
    begin
      if Name.Is_Known (The_Neo_Target) then
        return Neo.Tracking_Period_Of (The_Neo_Target);
      else
        return Time.Undefined;
      end if;
    end Tracking_Period;


    procedure Define_External_Target is
    begin
      User.Clear_Target;
      New_Target_Direction := Action_Handler.New_Direction;
      Telescope.Define_Space_Access (Target_Direction_Of'access, Name.No_Id);
      The_Landmark := Name.No_Id;
      The_Neo_Target := Name.No_Id;
    end Define_External_Target;


    procedure Handle_Goto is
    begin
      Define_External_Target;
      case The_Data.Status is
      when Telescope.Stopped
      | Telescope.Approaching
      | Telescope.Positioned
      | Telescope.Positioning
      | Telescope.Preparing
      | Telescope.Is_Tracking
      | Telescope.Waiting =>
        User.Perform_Goto;
      when others =>
        Log.Error ("goto not executed from " & The_Data.Status'img);
      end case;
    end Handle_Goto;


    Telescope_Information_Is_Handled : Boolean := False;

    procedure Handle_Telescope_Information is
      use type Telescope.State;
    begin
      The_Data := Telescope.Information;
      User.Show (The_Data);
      if The_Data.Target_Lost then
        The_Landmark := Name.No_Id;
        The_Neo_Target := Name.No_Id;
        User.Clear_Target;
      end if;
      if Name.Is_Known (The_Neo_Target) then
        declare
          Neo_Tracking_Period : constant Time.Period := Neo.Tracking_Period_Of (The_Neo_Target);
          Arriving_In     : Duration;
          use type Time.Period;
        begin
          if Neo_Tracking_Period /= Time.Undefined then
            Arriving_In := Neo_Tracking_Period.Arrival_Time - Time.Universal;
            if Arriving_In >= 0.0 then
              User.Show (Arriving_In);
            end if;
          end if;
        end;
      end if;
      case The_Data.Status is
      when Telescope.Homing | Telescope.Positioning | Telescope.Approaching =>
        declare
          Actual_Duration : Time.Ut := The_Data.Completion_Time - Time.Universal;
        begin
          if Actual_Duration < Time.In_The_Past then
            Actual_Duration := Time.In_The_Past;
          end if;
          if The_Completion_Time <= Actual_Duration then
            The_Completion_Time := Actual_Duration;
          elsif Actual_Duration /= Time.In_The_Past then
            User.Show (User.Percent(Float(Actual_Duration) * Float(User.Percent'last) / Float(The_Completion_Time)));
          else
            User.Show (The_Progress => 0);
          end if;
        end;
      when others =>
        The_Completion_Time := Time.In_The_Past;
        User.Show (The_Progress => 0);
      end case;
      case The_Data.Status is
      when Telescope.Stopped
      | Telescope.Positioning
      | Telescope.Preparing
      | Telescope.Approaching
      | Telescope.Is_Tracking
      | Telescope.Waiting =>
        Lx200_Server.Set (The_Data.Actual_Direction);
        Stellarium.Set (The_Data.Actual_Direction);
      when others =>
        null;
      end case;
    end Handle_Telescope_Information;


    function Synchronized_Clock return Ada.Real_Time.Time is
      The_Time     : Ada.Real_Time.Seconds_Count;
      The_Fraction : Ada.Real_Time.Time_Span;
      use type Ada.Real_Time.Seconds_Count;
    begin
      Ada.Real_Time.Split (Ada.Real_Time.Clock, The_Time, The_Fraction);
      return Ada.Real_Time.Time_Of (The_Time + 1, Ada.Real_Time.Time_Span_Zero);
    end Synchronized_Clock;

    The_Next_Time : Ada.Real_Time.Time := Synchronized_Clock;

    use type Ada.Real_Time.Time;
    use type Name.Id;

  begin -- Manager
    Log.Write ("manager start");
    Telescope.Set (User.Image_Orientation);
    Handbox.Start;
    loop
      select Action_Handler.Get (The_Command);
        case The_Command is
        when Define_Catalog =>
          Targets.Define_Catalog;
        when Update =>
          Targets.Update_List;
        when Define_Target =>
          declare
            The_Item : Name.Id;
          begin
            User.Show_Description ("");
            The_Landmark := Name.No_Id;
            The_Neo_Target := Name.No_Id;
            Targets.Get_For (User.Target_Name, The_Item);
            if Name.Is_Known (The_Item)  then
              case Name.Kind_Of (The_Item) is
              when Name.Sky_Object =>
                Telescope.Define_Space_Access (Name.Direction_Of'access, The_Item);
                User.Show_Description (Data.Descriptor_Of (Name.Object_Of (The_Item)));
              when Name.Moon =>
                Telescope.Define_Space_Access (Moon.Direction_Of'access, Name.No_Id);
              when Name.Sun =>
                Telescope.Define_Space_Access (Solar_System.Direction_Of'access, The_Item);
              when Name.Planet =>
                Telescope.Define_Space_Access (Solar_System.Direction_Of'access, The_Item);
                if Name.Image_Of (The_Item) = "Pluto" then
                  User.Show_Description ("Dwarf Planet");
                else
                  User.Show_Description ("Planet");
                end if;
              when Name.Small_Solar_System_Body =>
                Telescope.Define_Space_Access (Sssb.Direction_Of'access, The_Item);
              when Name.Near_Earth_Object =>
                Telescope.Define_Space_Access (Neo.Direction_Of'access, The_Item);
                The_Neo_Target := The_Item;
              when Name.Landmark =>
                The_Landmark := The_Item;
              end case;
            end if;
          end;
        when Startup =>
          Telescope.Startup;
        when Shutdown =>
          Telescope.Shutdown;
        when Stop =>
          Telescope.Halt;
        when Go_To =>
          if The_Landmark = Name.No_Id then
            Telescope.Follow (Tracking_Period);
          else
            Telescope.Position_To (The_Landmark);
          end if;
        when Back =>
          Telescope.Back;
        when Set_Orientation =>
          Telescope.Set (User.Image_Orientation);
        when New_Goto_Direction =>
          Handle_Goto;
        when New_Telescope_Information =>
          Handle_Telescope_Information;
          Telescope_Information_Is_Handled := True;
        when Close =>
          Handbox.Close;
          Targets.Stop;
          Telescope.Close;
          Stellarium.Close;
          Lx200_Server.Close;
          exit;
        end case;
      or
        delay until The_Next_Time;
        The_Next_Time := The_Next_Time + Ada.Real_Time.To_Time_Span (Time.One_Second / 2.0);
        if not Telescope_Information_Is_Handled then
          Handle_Telescope_Information;
        end if;
        Telescope_Information_Is_Handled := False;
      end select;
    end loop;
    Log.Write ("manager end");
    Action_Handler.Enable_Termination;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
    Handbox.Close;
    Targets.Stop;
    Gui.Close;
    Telescope.Close;
    Stellarium.Close;
    Lx200_Server.Close;
    Action_Handler.Enable_Termination;
  end Manager;


  procedure Read_Data is
  begin
    if not Sky_Line.Is_Defined then
      Horizon.Generate;
    end if;
    Sky_Line.Read;
    Neo.Add_Objects;
    Name.Read_Favorites;
  end Read_Data;


  procedure Information_Update_Handler is
  begin
    Action_Handler.Signal_New_Telescope_Data;
  end Information_Update_Handler;

  procedure Start is

    The_Manager : access Manager with Unreferenced;

    procedure Startup is
    begin
      Lx200_Server.Define_Handler (Goto_Handler'access);
      Stellarium.Define_Handler (Goto_Handler'access);
      The_Manager := new Manager;
    end Startup;

    procedure Termination is
    begin
      Action_Handler.Wait_For_Termination;
    end Termination;

    procedure Start_Stellarium_Server is
      Used_Port : constant Network.Port_Number := Parameter.Stellarium_Port;
    begin
      Stellarium.Start (Used_Port);
    exception
    when Network.Tcp.Port_In_Use =>
      Error.Raise_With (Application.Name & " - TCP port" & Used_Port'img & " for Stellarium in use");
    when others =>
      Error.Raise_With (Application.Name & " - could not start stellarium server");
    end Start_Stellarium_Server;

    procedure Start_Lx200_Server is
      Used_Port : constant Network.Port_Number := Parameter.Lx200_Port;
    begin
      Lx200_Server.Start (Used_Port);
    exception
    when Network.Tcp.Port_In_Use =>
      Error.Raise_With (Application.Name & " - TCP port" & Used_Port'img & " for Lx200 in use");
    when others =>
      Error.Raise_With (Application.Name & " - could not start Lx200 server");
    end Start_Lx200_Server;

  begin -- Start
    if (not Os.Is_Osx) and then (not Os.Application.Is_First_Instance) then
      --
      -- Note: This test is to prevent this application from being run more than once concurrently.
      --       If we mandate that this application is always run from within an OSX .app bundle then
      --       OSX will enforce this and therefore this test is not required.
      --       In this case it is better not to attempt detecting first instance because if the application
      --       is terminated by force quit the mutex is not released but remains until the host is rebooted.
      --
      Error.Raise_With (Application.Name & " already running");
    end if;
    Os.Process.Set_Priority_Class (Os.Process.Realtime);
    Parameter.Read;
    begin
      Read_Data;
      Start_Stellarium_Server;
      begin
        Start_Lx200_Server;
      exception
      when others =>
        Stellarium.Close;
        raise;
      end;
      Telescope.Start (Information_Update_Handler'access);
      Targets.Start (Clear  => User.Clear_Targets'access,
                     Define => User.Define'access,
                     Update => User.Update_Targets'access);
      User.Execute (Startup'access,
                    User_Action_Handler'access,
                    Termination'access);
      Parameter.Shutdown;
    exception
    when others =>
      Parameter.Shutdown;
      raise;
    end;
  exception
  when Error.Occurred =>
    User.Show_Error;
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Control;
