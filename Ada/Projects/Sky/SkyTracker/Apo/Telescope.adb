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

with Alignment;
with Parameter;
with Picture;
with Pole_Axis;
with Remote;
with Traces;
with User;

package body Telescope is

  package Log is new Traces ("Telescope");


  task type Control_Task is

    entry Define (The_Air_Pressure : Refraction.Hectopascal);

    entry Define (The_Temperature : Refraction.Celsius);

    entry Align;

    entry Go_To;

    entry Go_To_Left;

    entry Go_To_Right;

    entry Go_To_Top;

    entry Park;

    entry Stop;

    entry Unpark;

    entry Get (The_Data : out Data);

    entry Close;

  end Control_Task;


  type Control_Access is access Control_Task;

  Control : Control_Access;


  Signal_Information_Update : Information_Update_Handler;

  procedure Start (Update_Handler : Information_Update_Handler) is
  begin
    Signal_Information_Update := Update_Handler;
    Control := new Control_Task;
  end Start;


  procedure Define (The_Air_Pressure : Refraction.Hectopascal) is
  begin
    Control.Define (The_Air_Pressure);
  end Define;


  procedure Define (The_Temperature : Refraction.Celsius) is
  begin
    Control.Define (The_Temperature);
  end Define;


  procedure Align is
  begin
    Control.Align;
  end Align;


  procedure Go_To is
  begin
    Control.Go_To;
  end Go_To;


  procedure Go_To_Left is
  begin
    Control.Go_To_Left;
  end Go_To_Left;


  procedure Go_To_Right is
  begin
    Control.Go_To_Right;
  end Go_To_Right;


  procedure Go_To_Top is
  begin
    Control.Go_To_Top;
  end Go_To_Top;


  procedure Park is
  begin
    Control.Park;
  end Park;


  procedure Stop is
  begin
    Control.Stop;
  end Stop;


  procedure Unpark is
  begin
    Control.Unpark;
  end Unpark;


  Next_Id            : Name.Id;
  Next_Get_Direction : Get_Space_Access := null;


  function Target_Kind return Ten_Micron.Target_Kind is
  begin
    if Name.Is_Known (Next_Id) then
      case Name.Kind_Of (Next_Id) is
      when Name.Axis_Position =>
        return Ten_Micron.Axis_Position;
      when others =>
        return Ten_Micron.Other_Targets;
      end case;
    else
      return Ten_Micron.Other_Targets;
    end if;
  end Target_Kind;


  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id) is
  begin
    Next_Id := The_Id;
    Next_Get_Direction := Get_Direction;
  end Define_Space_Access;


  function Actual_Target_Direction return Space.Direction is
  begin
    if User.In_Setup_Mode then
      return Space.North_Pole;
    elsif Next_Get_Direction = null then
      return Space.Unknown_Direction;
    else
      return Next_Get_Direction (Next_Id, Time.Universal);
    end if;
  end Actual_Target_Direction;


  function Information return Data is
    The_Data : Data;
  begin
    Control.Get (The_Data);
    return The_Data;
  end Information;


  procedure Close is
  begin
    Control.Close;
  end Close;


  task body Control_Task is

    Update_Delay : constant Duration := 0.5;

    Aligning_Enabled : Boolean := False;

    The_Information : Ten_Micron.Information;

    The_Picture_Direction : Space.Direction;

    Evatuate_Pole_Axis : access procedure;


    procedure Get_Information is
    begin
      if The_Information.Status = Disconnected then
        Ten_Micron.Startup (Parameter.Ten_Micron_Ip_Address, Parameter.Ten_Micron_Port);
      end if;
      The_Information := Ten_Micron.Get;
    end Get_Information;


    function Solve_Picture return Boolean is
      Actual_Direction : constant Space.Direction := Actual_Target_Direction;
    begin
      if not Picture.Solve (Actual_Direction) then
        Log.Warning ("Picture not solved");
        return False;
      end if;
      return True;
    exception
    when Picture.Not_Solved =>
      Log.Warning ("Picture solving not started");
      return False;
    when Item: others =>
      Log.Error ("Picture solving failed");
      Log.Termination (Item);
      return False;
    end Solve_Picture;


    function Picture_Solved return Boolean is
    begin
      return Picture.Solved;
    exception
    when Picture.Not_Solved =>
      Log.Warning ("Picture not solved");
      return False;
    when Item: others =>
      Log.Error ("Picture solving failed");
      Log.Termination (Item);
      return False;
    end Picture_Solved;


    procedure Update_Handling is
    begin
      Get_Information;
      case The_Information.Status is
      when Positioned =>
        if User.In_Setup_Mode and then Picture.Exists and then Solve_Picture then
          Ten_Micron.Start_Solving;
        end if;
      when Tracking =>
        if not User.In_Setup_Mode
          and then not Aligning_Enabled
          and then Picture.Exists
          and then Solve_Picture
        then
          Ten_Micron.Start_Solving;
        end if;
      when Solving =>
        if Picture_Solved then
          Ten_Micron.End_Solving;
          if User.In_Setup_Mode then
            Evatuate_Pole_Axis.all;
          else
            The_Picture_Direction := Picture.Actual_Direction;
            User.Enable_Align_On_Picture;
            Aligning_Enabled := True;
          end if;
        end if;
      when Disconnected =>
        Aligning_Enabled := False;
        Picture.Stop_Solving;
      when others =>
        Aligning_Enabled := False;
      end case;
      Signal_Information_Update.all;
    end Update_Handling;


    procedure Synch_On_Picture is
    begin
      Aligning_Enabled := False;
      Ten_Micron.Synch_To (The_Picture_Direction);
      The_Picture_Direction := Space.Unknown_Direction;
    end Synch_On_Picture;


    procedure Position_To (The_Direction : Space.Direction) is
    begin
      Ten_Micron.Slew_To (The_Direction, Ten_Micron.Axis_Position);
      Remote.Define (Target => "");
    end Position_To;

  begin -- Control_Task
    loop
      begin
        select
          accept Close;
          exit;
        or
          accept Define (The_Air_Pressure : Refraction.Hectopascal) do
            Ten_Micron.Define (The_Air_Pressure);
          end Define;
        or
          accept Define (The_Temperature : Refraction.Celsius) do
            Ten_Micron.Define (The_Temperature);
          end Define;
        or
          accept Align do
            Synch_On_Picture;
            Ten_Micron.Slew_To (Actual_Target_Direction, Target_Kind);
          end Align;
        or
          accept Go_To do
            Evatuate_Pole_Axis := null;
            Ten_Micron.Slew_To (Actual_Target_Direction, Target_Kind);
            if Name.Is_Known (Next_Id) then
              case Name.Kind_Of (Next_Id) is
              when Name.Axis_Position =>
                Remote.Define (Target => "");
              when others =>
                Remote.Define (Target => Name.Image_Of (Next_Id));
              end case;
            else
              Remote.Define (Target => "");
            end if;
          end Go_To;
        or
          accept Go_To_Left do
            Evatuate_Pole_Axis := Pole_Axis.Evaluate_Left'access;
            Position_To (Space.Axis_Pole_Left);
          end Go_To_Left;
        or
          accept Go_To_Right do
            Evatuate_Pole_Axis := Pole_Axis.Evaluate_Right'access;
            Position_To (Space.Axis_Pole_Right);
          end Go_To_Right;
        or
          accept Go_To_Top do
            Evatuate_Pole_Axis := Pole_Axis.Evaluate_Top'access;
            Position_To (Space.Axis_Pole_Top);
          end Go_To_Top;
        or
          accept Park do
            Remote.Define (Target => "");
            Ten_Micron.Park;
          end Park;
        or
          accept Stop do
            Remote.Define (Target => "");
            Ten_Micron.Stop;
          end Stop;
        or
          accept Unpark do
            Remote.Define (Target => "");
            Ten_Micron.Unpark;
          end Unpark;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := Telescope.State(The_Information.Status);
            The_Data.Target_Direction := Actual_Target_Direction;
            The_Data.Actual_Direction := The_Information.Direction;
            The_Data.Actual_Position := The_Information.Position;
            The_Data.Universal_Time := Time.Universal;
            The_Data.Cone_Error := Alignment.Cone_Error;
            The_Data.Pole_Offsets := Alignment.Pole_Offsets;
          end Get;
        or delay Update_Delay;
          Signal_Information_Update.all;
          Update_Handling;
        end select;
        Remote.Define (The_Information.Status in Tracking | Following);
      exception
      when Item: others =>
        Log.Termination (Item);
      end;
    end loop;
    Log.Write ("end");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control_Task;

end Telescope;
