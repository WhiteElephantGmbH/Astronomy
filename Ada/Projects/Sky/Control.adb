-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar;
with Ada.Real_Time;
with Alignment;
with Application;
with Data;
with Earth;
with Error;
with Gui;
with Lx200;
with Moon;
with Motor;
with Name;
with Neo;
with Network.Tcp;
with Numerics;
with Os.Application;
with Os.Ascom;
with Parameter;
with Os.Process;
with Sky_Line;
with Solar_System;
with Space;
with Sssb;
with Stellarium;
with Telescope;
with Time;
with Traces;
with User;

package body Control is

  package Log is new Traces ("Control");

  task type Target_Handler is

    entry Define_Catalog;

    entry Update;

    entry Get_For (Target_Name : String;
                   Target_Id   : out Name.Id);
    entry Stop;

  end Target_Handler;


  task body Target_Handler is

    The_Targets : aliased Name.Id_List;


    procedure Define_Targets (New_List : Boolean := False) is

      function Is_Visible (Direction : Space.Direction) return Boolean is
      begin
        return Sky_Line.Is_Above (Direction => Direction,
                                  Lmst      => Time.Lmst);
      end Is_Visible;

      function Is_To_Add (Item      : User.Selection;
                          Direction : Space.Direction) return Boolean is
      begin
        return Space.Direction_Is_Known (Direction) and then User.Is_Selected (Item) and then Is_Visible (Direction);
      end Is_To_Add;

      The_Changes : Natural := 0;

      Ut : constant Time.Ut := Time.Universal;

    begin -- Define_Targets
      for Index in The_Targets.Ids'first .. The_Targets.Last loop
        declare

          Item : Name.Id renames The_Targets.Ids(Index);

          function Is_To_Add return Boolean is
          begin
            case Name.Kind_Of (Item) is
            when Name.Moon =>
              return Is_To_Add (User.Solar_System, Moon.Direction_Of (Item, Ut));
            when Name.Sun | Name.Planet =>
              return Is_To_Add (User.Solar_System, Solar_System.Direction_Of (Item, Ut));
            when Name.Small_Solar_System_Body =>
              return Is_To_Add (User.Solar_System, Sssb.Direction_Of (Item, Ut));
            when Name.Landmark =>
              return True;
            when Name.Near_Earth_Object =>
              return User.Is_Selected (User.Near_Earth_Objects) and then Neo.Is_Arriving (Item);
            when Name.Sky_Object =>
              declare
                Object : constant Data.Object := Name.Object_Of (Item);
              begin
                case Data.Type_Of (Object) is
                when Data.Landmark  =>
                  raise Program_Error;
                when Data.Quasar  =>
                  return Is_To_Add (User.Galaxies, Data.Direction_Of (Object, Ut));
                when Data.Galaxy  =>
                  return Is_To_Add (User.Galaxies, Data.Direction_Of (Object, Ut));
                when Data.Nebula  =>
                  return Is_To_Add (User.Nebulas, Data.Direction_Of (Object, Ut));
                when Data.Cluster =>
                  return Is_To_Add (User.Clusters, Data.Direction_Of (Object, Ut));
                when Data.Stars =>
                  return Is_To_Add (User.Open_Clusters, Data.Direction_Of (Object, Ut));
                when Data.Double =>
                  return Is_To_Add (User.Multiple_Stars, Data.Direction_Of (Object, Ut));
                when Data.Star =>
                  return Is_To_Add (User.Stars, Data.Direction_Of (Object, Ut));
                when Data.Satellite =>
                  raise Program_Error;
                when Data.Unknown =>
                  return False;
                end case;
              end;
            end case;
          end Is_To_Add;

        begin
          if Name.Visibility_Changed_For (Item, Is_To_Add) then
            The_Changes := The_Changes + 1;
          end if;
        end;
      end loop;
      if New_List or (The_Changes > 10) then
        User.Clear_Targets;
        Name.Clear_History_For (The_Targets);
        User.Define (The_Targets'unrestricted_access);
      else
        User.Update_Targets;
      end if;
    end Define_Targets;

  begin -- Target_Handler
    loop
      select
        accept Define_Catalog;
        User.Clear_Targets;
        The_Targets := Name.Actual_List;
        Define_Targets (New_List => True);
      or
        accept Update;
        Define_Targets;
      or
        accept Get_For (Target_Name : String;
                        Target_Id   : out Name.Id)
        do
          Target_Id := Name.Item_Of (The_Targets, Target_Name);
        end Get_For;
      or
        accept Stop;
        exit;
      or delay 10.0;
        Define_Targets;
      end select;
    end loop;
    Log.Write ("target handler end");
  exception
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Target_Handler;


  task type Manager is
  end Manager;


  type Command is (Define_Catalog,
                   Define_Target,
                   Stop,
                   Park,
                   Align,
                   Synch,
                   Go_To,
                   Set_Orientation,
                   New_Goto_Direction,
                   New_Synch_Direction,
                   Update,
                   New_Telescope_Information,
                   Close);

  protected Action_Handler is
    procedure Put_Goto (The_Direction  : Space.Direction);
    procedure Put_Synch (The_Direction : Space.Direction);
    procedure Put (The_Action       : User.Action);
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
    Has_New_Synch_Direction : Boolean := False;
    Define_Catalog_Pending  : Boolean := False;
    Define_Target_Pending   : Boolean := False;
    Update_Pending          : Boolean := False;
    Command_Is_Pending      : Boolean := False;
    Termination_Is_Enabled  : Boolean := False;
  end Action_Handler;


  procedure Goto_Handler (The_Direction : Space.Direction) is
  begin
    Action_Handler.Put_Goto (The_Direction);
  end Goto_Handler;


  procedure Synch_Handler (The_Direction : Space.Direction) is
  begin
    Action_Handler.Put_Synch (The_Direction);
  end Synch_Handler;


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
      when User.Park =>
        Next_Command := Park;
        Command_Is_Pending := True;
      when User.Go_To =>
        Next_Command := Go_To;
        Command_Is_Pending := True;
      when User.Stop =>
        Next_Command := Stop;
        Command_Is_Pending := True;
      when User.Align =>
        Next_Command := Align;
        Command_Is_Pending := True;
      when User.Synch =>
        Next_Command := Synch;
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

    procedure Put_Synch (The_Direction : Space.Direction) is
    begin
      The_New_Direction := The_Direction;
      if Next_Command /= Close then
        Has_New_Synch_Direction := True;
      end if;
    end Put_Synch;

    procedure Signal_New_Telescope_Data is
    begin
      if Next_Command /= Close then
        Has_New_Telescope_Data := True;
      end if;
    end Signal_New_Telescope_Data;

    entry Get (The_Command : out Command)
      when Has_New_Goto_Direction
        or Has_New_Synch_Direction
        or Command_Is_Pending
        or Define_Catalog_Pending
        or Update_Pending
        or Define_Target_Pending
        or Has_New_Telescope_Data
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
      elsif Has_New_Synch_Direction then
        The_Command := New_Synch_Direction;
        Has_New_Synch_Direction := False;
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

    type Target_Access is access Target_Handler;

    Targets : constant Target_Access := new Target_Handler;

    The_Command : Command;

    The_Data : Telescope.Data;

    Is_Zero_Target : Boolean := True;

    The_Neo_Target : Name.Id;
    The_Landmark   : Name.Id;

    The_Travelling_Time : Time.Ut := 0.0;


    function Arrival_Time return Time.Ut is
    begin
      if Name.Is_Known (The_Neo_Target) then
        return Neo.Arrival_Time_Of (The_Neo_Target);
      else
        return Time.In_The_Past;
      end if;
    end Arrival_Time;


    procedure Define_External_Target is
    begin
      User.Clear_Target;
      New_Target_Direction := Action_Handler.New_Direction;
      Telescope.Define_Space_Access (Target_Direction_Of'access, Name.No_Id);
      The_Landmark := Name.No_Id;
      The_Neo_Target := Name.No_Id;
      Is_Zero_Target := False;
    end Define_External_Target;


    procedure Handle_Goto is
    begin
      Define_External_Target;
      case The_Data.Status is
      when Telescope.Disconnected | Telescope.Ready | Telescope.Startup =>
        Log.Error ("goto not executed");
      when others =>
        User.Perform_Goto;
      end case;
    end Handle_Goto;


    procedure Handle_Synch is
      use type Name.Id;
    begin
      Define_External_Target;
      case The_Data.Status is
      when Telescope.Disconnected | Telescope.Startup =>
        Log.Error ("synch not executed");
      when others =>
        User.Perform_Synch;
      end case;
    end Handle_Synch;


    Telescope_Information_Is_Handled : Boolean := False;

    procedure Handle_Telescope_Information is
      use type Telescope.State;
    begin -- Handle_Telescope_Information
      The_Data := Telescope.Information;
      User.Show (The_Data);
      if The_Data.Target_Lost then
        The_Landmark := Name.No_Id;
        The_Neo_Target := Name.No_Id;
        User.Clear_Target;
      end if;
      if Name.Is_Known (The_Neo_Target) then
        declare
          Arriving_In : Time.Ut := Neo.Arrival_Time_Of (The_Neo_Target);
        begin
          if Arriving_In /= Time.In_The_Past then
            Arriving_In := Arriving_In - Time.Universal;
            if Arriving_In >= 0.0 then
              User.Show (Arriving_In);
            end if;
          end if;
        end;
      end if;
      case The_Data.Status is
      when Telescope.Startup =>
        return;
      when Telescope.Ready =>
        Is_Zero_Target := True;
        User.Define_Park_Position;
        return;
      when Telescope.Disconnected =>
        return;
      when Telescope.Approaching | Telescope.Parking | Telescope.Positioning | Telescope.Preparing =>
        declare
          Actual_Duration : Time.Ut := The_Data.Arriving_Time - Time.Universal;
        begin
          if Actual_Duration < 0.0 then
            Actual_Duration := 0.0;
          end if;
          if The_Travelling_Time <= Actual_Duration then
            The_Travelling_Time := Actual_Duration;
          elsif Actual_Duration /= 0.0 then
            User.Show (User.Percent(Float(Actual_Duration) * Float(User.Percent'last) / Float(The_Travelling_Time)));
          end if;
        end;
      when others =>
        The_Travelling_Time := 0.0;
        User.Show (The_Progress => 0);
      end case;
      Os.Ascom.Set (The_Data.Actual_Direction);
      Os.Ascom.Set (Is_Approaching => The_Data.Status = Telescope.Approaching);
      Lx200.Set (The_Data.Actual_Direction);
      Stellarium.Set (The_Data.Actual_Direction);
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
    loop
      select Action_Handler.Get (The_Command);
        case The_Command is
        when Define_Catalog =>
          Targets.Define_Catalog;
        when Update =>
          Targets.Update;
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
              Is_Zero_Target := False;
            else
              User.Define_Park_Position;
              Is_Zero_Target := True;
            end if;
          end;
        when Stop =>
          Telescope.Halt;
        when Align =>
          Telescope.Align;
        when Synch =>
          if Is_Zero_Target then
            Telescope.Synch_Park_Position;
          else
            Telescope.Synch_On_Target;
          end if;
        when Park =>
          Telescope.Park;
        when Go_To =>
          if The_Landmark = Name.No_Id then
            Telescope.Follow (Arrival_Time);
          else
            Telescope.Position_To (The_Landmark);
          end if;
        when Set_Orientation =>
          Telescope.Set (User.Image_Orientation);
        when New_Goto_Direction =>
          Handle_Goto;
        when New_Synch_Direction =>
          Handle_Synch;
        when New_Telescope_Information =>
          Handle_Telescope_Information;
          Telescope_Information_Is_Handled := True;
        when Close =>
          Targets.Stop;
          Telescope.Close;
          Stellarium.Close;
          Lx200.Close;
          Os.Ascom.Close;
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
    Targets.Stop;
    Gui.Close;
    Telescope.Close;
    Stellarium.Close;
    Lx200.Close;
    Os.Ascom.Close;
    Action_Handler.Enable_Termination;
  end Manager;


  procedure Read_Data is
  begin
    Alignment.Read;
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
      Os.Ascom.Define_Handlers (Goto_Handler'access,
                                Synch_Handler'access);
      Lx200.Define_Handler (Goto_Handler'access);
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
      Lx200.Start (Used_Port);
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
    elsif not Motor.Is_Stepper and then
      Ada.Calendar.Year(Ada.Calendar.Clock) > 2016 and then Ada.Calendar.Month (Ada.Calendar.Clock) > 6
    then
      Error.Raise_With (Application.Name & " licence timeout");
    end if;
    Os.Process.Set_Priority_Class (Os.Process.Realtime);
    Parameter.Read (Motor.Is_Stepper);
    Time.Set (Parameter.Longitude);
    Read_Data;
    Motor.Define (First_Acceleration  => Parameter.First_Acceleration,
                  Second_Acceleration => Parameter.Second_Acceleration,
                  First_Lower_Limit   => Parameter.First_Lower_Limit,
                  First_Upper_Limit   => Parameter.First_Upper_Limit,
                  Second_Lower_Limit  => Parameter.Second_Lower_Limit,
                  Second_Upper_Limit  => Parameter.Second_Upper_Limit,
                  Maximum_Speed       => Parameter.Maximum_Speed,
                  Park_Position       => Numerics.Position_Of (Earth.Direction_Of (Az  => Parameter.Park_Azimuth,
                                                                                   Alt => Parameter.Park_Altitude)));
    Start_Stellarium_Server;
    Os.Ascom.Start;
    begin
      Start_Lx200_Server;
    exception
    when others =>
      Os.Ascom.Close;
      Stellarium.Close;
      raise;
    end;
    Telescope.Start (Information_Update_Handler'access);
    User.Execute (Startup'access,
                  User_Action_Handler'access,
                  Termination'access);
  exception
  when Error.Occurred =>
    User.Show_Error;
  when Occurrence: others =>
    Log.Termination (Occurrence);
  end Start;

end Control;
