-- *********************************************************************************************************************
-- *                               (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Application;
with Data;
with Error;
with Gui;
with Horizon;
with Moon;
with Name;
with Network.Tcp;
with Os.Application;
with Os.Process;
with Parameter;
with Remote;
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
                   Go_To,
                   Park,
                   Stop,
                   Unpark,
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
    The_New_Direction        : Space.Direction;
    Next_Command             : User.Command_Action;
    Has_New_Telescope_Data   : Boolean := False;
    Has_New_Goto_Direction   : Boolean := False;
    Define_Catalog_Pending   : Boolean := False;
    Define_Target_Pending    : Boolean := False;
    Update_Pending           : Boolean := False;
    Command_Is_Pending       : Boolean := False;
    Termination_Is_Enabled   : Boolean := False;
  end Action_Handler;


  procedure Goto_Handler (The_Direction : Space.Direction) is
    The_Target : Name.Id;
    use type Name.Id;
  begin
    Targets.Get_For (The_Direction, Stellarium.Search_Tolerance, The_Target);
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
      use type User.Action;
    begin
      if Next_Command = User.Close then
        return;
      end if;
      case The_Action is
      when User.Define_Catalog =>
        Define_Catalog_Pending := True;
      when User.Define_Target =>
        Define_Target_Pending := True;
      when User.Update =>
        Update_Pending := True;
      when User.Command_Action =>
        Next_Command := The_Action;
        Command_Is_Pending := True;
      end case;
    end Put;

    procedure Put_Goto (The_Direction : Space.Direction) is
      use type User.Action;
    begin
      The_New_Direction := The_Direction;
      if Next_Command /= User.Close then
        Has_New_Goto_Direction := True;
      end if;
    end Put_Goto;

    procedure Signal_New_Telescope_Data is
      use type User.Action;
    begin
      if Next_Command /= User.Close then
        Has_New_Telescope_Data := True;
      end if;
    end Signal_New_Telescope_Data;

    entry Get (The_Command : out Command)
      when Has_New_Goto_Direction
        or Command_Is_Pending
        or Define_Catalog_Pending
        or Update_Pending
        or Define_Target_Pending
        or Has_New_Telescope_Data
    is
      use type User.Action;

    begin -- Get
      if Next_Command = User.Close then
        The_Command := Close;
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
        case Next_Command is
        when User.Go_To =>
          The_Command := Go_To;
        when User.Park =>
          The_Command := Park;
        when User.Stop =>
          The_Command := Stop;
        when User.Unpark =>
          The_Command := Unpark;
        when User.Close =>
          raise Program_Error; -- already handled
        end case;
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


  function Axis_Direction_Of (Axis_Position : Name.Id;
                              Unused_Ut     : Time.Ut) return Space.Direction is
  begin
    return Name.Direction_Of (Axis_Position);
  end Axis_Direction_Of;


  task body Manager is

    The_Command : Command;

    The_Data : Telescope.Data;

    use all type Telescope.State;

    procedure Define_External_Target is
    begin
      User.Clear_Target;
      New_Target_Direction := Action_Handler.New_Direction;
      Telescope.Define_Space_Access (Target_Direction_Of'access, Name.No_Id);
    end Define_External_Target;

    procedure Handle_Goto is
    begin
      Define_External_Target;
      case The_Data.Status is
      when Failure
         | Inconsistent
         | Inhibited
         | Homing
         | Parked
         | Parking
         | Unparking
         | Unknown
         | Disconnected
      =>
        Log.Warning ("goto not executed");
      when Slewing
         | Following
         | Positioned
         | Outside
         | Stopped
         | Tracking
      =>
        User.Perform_Goto;
      end case;
    end Handle_Goto;


    procedure Handle_Telescope_Information is
    begin
      The_Data := Telescope.Information;
      User.Show (The_Data);
      case The_Data.Status is
      when Failure
         | Inconsistent
         | Inhibited
         | Outside
         | Unknown
         | Disconnected
      =>
        null;
      when Slewing
         | Following
         | Positioned
         | Stopped
         | Parked
         | Tracking
         | Homing
         | Parking
         | Unparking
      =>
        Stellarium.Set (The_Data.Actual_Direction);
      end case;
    end Handle_Telescope_Information;

  begin -- Manager
    Log.Write ("manager start");
    if Parameter.Remote_Configured then
      Remote.Start (Parameter.Telescope_Name, Parameter.Remote_Address, Parameter.Remote_Port);
    end if;
    loop
      Action_Handler.Get (The_Command);
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
          Targets.Get_For (User.Target_Name, The_Item);
          if Name.Is_Known (The_Item)  then
            case Name.Kind_Of (The_Item) is
            when Name.Sky_Object =>
              Telescope.Define_Space_Access (Name.Direction_Of'access, The_Item);
              User.Show_Description (Data.Descriptor_Of (Name.Object_Of (The_Item)));
            when Name.Moon =>
              Telescope.Define_Space_Access (Moon.Direction_Of'access, The_Item);
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
            when Name.Axis_Position =>
              Telescope.Define_Space_Access (Axis_Direction_Of'access, The_Item);
            when Name.Landmark =>
              null; -- no Landmark
            when Name.Near_Earth_Object =>
              null; -- no NEOs
            end case;
          else
            null;
          end if;
        end;
      when Go_To =>
        Telescope.Go_To;
      when Park =>
        Telescope.Park;
      when Stop =>
        Telescope.Stop;
      when Unpark =>
        Telescope.Unpark;
      when New_Goto_Direction =>
        Handle_Goto;
      when New_Telescope_Information =>
        Handle_Telescope_Information;
      when Close =>
        Targets.Stop;
        Telescope.Close;
        Remote.Close;
        Stellarium.Close;
        exit;
      end case;
    end loop;
    Log.Write ("manager end");
    Action_Handler.Enable_Termination;
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
    Targets.Stop;
    Gui.Close;
    Telescope.Close;
    Remote.Close;
    Stellarium.Close;
    Action_Handler.Enable_Termination;
  end Manager;


  procedure Read_Data is
  begin
    if not Sky_Line.Is_Defined then
      Horizon.Generate;
    end if;
    Sky_Line.Read;
    Name.Read_Favorites (Enable_Axis_Positions => True,
                         Enable_Land_Marks     => False,
                         Enable_Neos           => False);
  end Read_Data;


  procedure Information_Update_Handler is
  begin
    Action_Handler.Signal_New_Telescope_Data;
  end Information_Update_Handler;

  procedure Start is

    The_Manager : access Manager with Unreferenced;

    procedure Startup is
    begin
      Stellarium.Define_Handler (Goto_Handler'access);
      The_Manager := new Manager;
    end Startup;

    procedure Termination is
    begin
      Action_Handler.Wait_For_Termination;
    end Termination;

    function Started_Stellarium_Server return Boolean is
      Used_Port : constant Network.Port_Number := Parameter.Stellarium_Port;
    begin
      Stellarium.Start (Used_Port);
      return True;
    exception
    when Network.Tcp.Port_In_Use =>
      User.Show_Error ("TCP port" & Used_Port'img & " for Stellarium in use");
      return False;
    when others =>
      User.Show_Error ("could not start stellarium server");
      return False;
    end Started_Stellarium_Server;

  begin -- Start
    if (not Os.Is_Osx) and then (not Os.Application.Is_First_Instance) then
    --
    -- Note: This test is to prevent this application from being run more than once concurrently.
    --       If we mandate that this application is always run from within an OSX .app bundle then
    --       OSX will enforce this and therefore this test is not required.
    --       In this case it is better not to attempt detecting first instance because if the application
    --       is terminated by force quit the mutex is not released but remains until the host is rebooted.
    --
      User.Show_Error (Application.Name & " already running");
      return;
    end if;
    Os.Process.Set_Priority_Class (Os.Process.Realtime);
    Parameter.Read;
    Read_Data;
    if Started_Stellarium_Server then
      Telescope.Start (Information_Update_Handler'access);
      Targets.Start (Clear  => User.Clear_Targets'access,
                     Define => User.Define'access,
                     Update => User.Update_Targets'access);
      User.Execute (Startup'access,
                    User_Action_Handler'access,
                    Termination'access);
      Stellarium.Shutdown;
    end if;
  exception
  when Error.Occurred =>
    Stellarium.Shutdown;
    User.Show_Error (Error.Message);
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Control;