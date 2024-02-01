-- *********************************************************************************************************************
-- *                           (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                      *
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
with Clock;
with Data;
with Error;
with Gui;
with Horizon;
with Name;
with Neo;
with Network.Tcp;
with Os.Application;
with Os.Process;
with Parameter;
with Sky_Line;
with Space;
with Sssb;
with Stellarium;
with Sun;
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
                   Align,
                   Go_To,
                   Go_To_Left,
                   Go_To_Right,
                   Go_To_Top,
                   Go_To_Next,
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
    The_New_Direction      : Space.Direction;
    Next_Command           : User.Command_Action := User.Command_Action'pred(User.Close); -- not Close
    Has_New_Telescope_Data : Boolean := False;
    Has_New_Goto_Direction : Boolean := False;
    Define_Catalog_Pending : Boolean := False;
    Define_Target_Pending  : Boolean := False;
    Update_Pending         : Boolean := False;
    Command_Is_Pending     : Boolean := False;
    Termination_Is_Enabled : Boolean := False;
  end Action_Handler;


  procedure Goto_Handler (The_Direction : Space.Direction) is
    The_Target : Name.Id;
    use type Name.Id;
  begin
    Targets.Get_For (The_Direction, Parameter.Search_Tolerance, The_Target);
    if The_Target = Name.No_Id then
      if not Sun.Is_Visible or else Sun.Is_In_Safe_Distance (To_Target => The_Direction) then
        Action_Handler.Put_Goto (The_Direction);
      end if;
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
      elsif Command_Is_Pending then
        case Next_Command is
        when User.Align =>
          The_Command := Align;
        when User.Go_To =>
          The_Command := Go_To;
        when User.Go_To_Left =>
          The_Command := Go_To_Left;
        when User.Go_To_Right =>
          The_Command := Go_To_Right;
        when User.Go_To_Top =>
          The_Command := Go_To_Top;
        when User.Go_To_Next =>
          The_Command := Go_To_Next;
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
      elsif Has_New_Telescope_Data then
        The_Command := New_Telescope_Information;
        Has_New_Telescope_Data := False;
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

    The_Neo_Target : Name.Id;

    use all type Telescope.State;

    subtype Error_State is Telescope.Error_State;

    subtype Transit_State is Telescope.Transit_State;

    procedure Define_External_Target is
    begin
      User.Clear_Target;
      New_Target_Direction := Action_Handler.New_Direction;
      Telescope.Define_Space_Access (Target_Direction_Of'access, Name.No_Id);
      The_Neo_Target := Name.No_Id;
    end Define_External_Target;

    procedure Handle_Goto is
    begin
      Define_External_Target;
      case The_Data.Status is
      when Error_State
         | Inhibited
         | Homing
         | Parked
         | Parking
         | Unparking
         | Disconnected
         | Capturing
         | Solving
      =>
        Log.Warning ("goto not executed");
      when Slewing
         | Following
         | Positioned
         | Outside
         | Stopped
         | Tracking
         | Transit_State
      =>
        User.Perform_Goto;
      end case;
    end Handle_Goto;


    procedure Handle_Telescope_Information is
    begin
      The_Data := Telescope.Information;
      User.Show (The_Data);
      if Name.Is_Known (The_Neo_Target) then
        declare
          Neo_Tracking_Period : constant Time.Period := Neo.Tracking_Period_Of (The_Neo_Target);
          Arriving_In         : Duration;
          use type Time.Ut;
          use type Time.Period;
        begin
          if Neo_Tracking_Period /= Time.Undefined then
            Arriving_In := Duration(Neo_Tracking_Period.Arrival_Time - Time.Universal);
            if Arriving_In < 0.0 then
              Arriving_In := 0.0;
            end if;
            User.Show (Arriving_In);
          end if;
        end;
      end if;
      case The_Data.Status is
      when Error_State
         | Inhibited
         | Outside
         | Disconnected
      =>
        null;
      when Capturing
         | Slewing
         | Following
         | Positioned
         | Stopped
         | Parked
         | Tracking
         | Transit_State
         | Solving
         | Homing
         | Parking
         | Unparking
      =>
        Stellarium.Set (The_Data.Actual_Direction);
      end case;
    end Handle_Telescope_Information;

  begin -- Manager
    Log.Write ("manager start");
    loop
      Action_Handler.Get (The_Command);
      case The_Command is
      when Define_Catalog =>
        Targets.Define_Catalog;
      when Update =>
        Targets.Update_List;
      when Define_Target =>
        declare
          Target_Name : constant String := User.Target_Name;
          The_Item    : Name.Id;
        begin
          User.Show_Description ("");
          The_Neo_Target := Name.No_Id;
          Targets.Get_For (Target_Name, The_Item);
          if Name.Is_Known (The_Item)  then
            case Name.Kind_Of (The_Item) is
            when Name.Sky_Object =>
              Telescope.Define_Space_Access (Name.Direction_Of'access, The_Item);
              User.Show_Description (Data.Descriptor_Of (Name.Object_Of (The_Item)));
            when Name.Moon =>
              Telescope.Define_Space_Access (Targets.Moon_Direction_Of'access, The_Item);
            when Name.Sun =>
              Telescope.Define_Space_Access (Targets.Solar_System_Direction_Of'access, The_Item);
            when Name.Planet =>
              Telescope.Define_Space_Access (Targets.Solar_System_Direction_Of'access, The_Item);
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
              Telescope.Define_Space_Access (null, The_Item);
              The_Neo_Target := The_Item;
              Telescope.Prepare_Tle;
            end case;
          else
            null;
          end if;
        end;
      when Align =>
        Telescope.Align;
      when Go_To =>
        Telescope.Go_To;
      when Go_To_Left =>
        Telescope.Go_To_Left;
      when Go_To_Right =>
        Telescope.Go_To_Right;
      when Go_To_Top =>
        Telescope.Go_To_Top;
      when Go_To_Next =>
        Telescope.Go_To_Next;
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
    Stellarium.Close;
    Action_Handler.Enable_Termination;
  end Manager;


  procedure Read_Data is
  begin
    if not Sky_Line.Is_Defined then
      Horizon.Generate;
    end if;
    Sky_Line.Read;
    Neo.Add_Objects;
    Name.Read_Favorites (Enable_Axis_Positions => True,
                         Enable_Land_Marks     => False,
                         Neo_Existing          => Neo.Exists'access);
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
      begin
        Clock.Start;
        Telescope.Start (Information_Update_Handler'access);
        Targets.Start (Clear    => User.Clear_Targets'access,
                       Define   => User.Define'access,
                       Update   => User.Update_Targets'access,
                       Arriving => Neo.Is_Arriving'access);
        User.Execute (Startup'access,
                      User_Action_Handler'access,
                      Termination'access);
        Stellarium.Shutdown;
        Clock.Finish;
      exception
      when others =>
        Clock.Finish;
        raise;
      end;
    end if;
  exception
  when Error.Occurred =>
    Stellarium.Shutdown;
    User.Show_Error (Error.Message);
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Control;
