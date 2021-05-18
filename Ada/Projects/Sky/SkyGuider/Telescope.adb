-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
with Site;
with System;
with Traces;
with User;

package body Telescope is

  package Log is new Traces ("Telescope");


  task type Control_Task with Priority => System.Max_Priority is

    entry Start;

    entry Increase_Moving_Rate;

    entry Decrease_Moving_Rate;

    entry Start_Moving (Direction : Moving_Direction);

    entry Stop_Moving (Direction : Moving_Direction);

    entry Stop_Moving;

    entry Initialize;

    entry Go_To;

    entry Synch;

    entry Align;

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
    Control.Start;
  end Start;


  procedure Start is
  begin
    Control.Start;
  end Start;


  procedure Increase_Moving_Rate is
  begin
    Control.Increase_Moving_Rate;
  end Increase_Moving_Rate;


  procedure Decrease_Moving_Rate is
  begin
    Control.Decrease_Moving_Rate;
  end Decrease_Moving_Rate;


  procedure Start_Moving (Direction : Moving_Direction) is
  begin
    Control.Start_Moving (Direction);
  end Start_Moving;


  procedure Stop_Moving (Direction : Moving_Direction) is
  begin
    Control.Stop_Moving (Direction);
  end Stop_Moving;


  procedure Stop_Moving is
  begin
    Control.Stop_Moving;
  end Stop_Moving;


  procedure Initialize is
  begin
    Control.Initialize;
  end Initialize;


  procedure Go_To is
  begin
    Control.Go_To;
  end Go_To;


  procedure Synch is
  begin
    Control.Synch;
  end Synch;


  procedure Align is
  begin
    Control.Align;
  end Align;


  Next_Id            : Name.Id;
  Next_Get_Direction : Get_Space_Access := null;

  procedure Define_Space_Access (Get_Direction : Get_Space_Access;
                                 The_Id        : Name.Id) is
  begin
    Next_Id := The_Id;
    Next_Get_Direction := Get_Direction;
  end Define_Space_Access;


  function Actual_Target_Direction return Space.Direction is
  begin
    if Next_Get_Direction = null then
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

    Update_Delay : constant Duration := 0.25;

    The_Information : M_Zero.Information;
    The_Status       : State       := Disconnected;
    Aligning_Enabled : Boolean     := False;
    The_Moving_Rate  : Moving_Rate := Finding;

    The_Picture_Direction : Space.Direction;


    procedure Set (Item : Moving_Rate) is
    begin
      M_Zero.Set_Rate (M_Zero.Moving_Rate(Item));
    end Set;


    procedure Get_Information is
    begin
      The_Information := M_Zero.Get;
      The_Status := Telescope.State(The_Information.Status);
    end Get_Information;


    procedure Startup is
    begin
      Get_Information;
      if The_Status = Disconnected then
        M_Zero.Connect (Parameter.M_Zero_Ip_Address, Parameter.M_Zero_Port);
        Set (The_Moving_Rate);
      end if;
      Get_Information;
      if The_Status = Connected then
        if Site.Is_Defined then
          M_Zero.Initialize;
        end if;
      end if;
    end Startup;


    function Solve_Picture return Boolean is
      Actual_Direction : constant Space.Direction := Actual_Target_Direction;
    begin
      if not Picture.Solve (Actual_Direction) then
        M_Zero.Set_Error ("Picture not solved");
        return False;
      end if;
      return True;
    exception
    when Picture.Not_Solved =>
      M_Zero.Set_Error ("Picture solving not started");
      return False;
    when Item: others =>
      M_Zero.Set_Error ("Picture solving failed");
      Log.Termination (Item);
      return False;
    end Solve_Picture;


    function Picture_Solved return Boolean is
    begin
      return Picture.Solved;
    exception
    when Picture.Not_Solved =>
      M_Zero.Set_Error ("Picture not solved");
      return False;
    when Item: others =>
      M_Zero.Set_Error ("Picture solving failed");
      Log.Termination (Item);
      return False;
    end Picture_Solved;


    procedure Update_Handling is
      Last_Status : constant State := The_Status;
    begin
      Get_Information;
      if Last_Status = Approaching and then The_Status = Tracking then -- start tracking
        Set (The_Moving_Rate);
      end if;
      case The_Status is
      when Tracking =>
        if not User.In_Setup_Mode
          and then not Aligning_Enabled
          and then Picture.Exists
          and then Solve_Picture
        then
          M_Zero.Start_Solving;
        end if;
      when Solving =>
        if Picture_Solved then
          The_Picture_Direction := Picture.Actual_Direction;
          User.Enable_Align_On_Picture;
          Aligning_Enabled := True;
          M_Zero.End_Solving;
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
      M_Zero.Synch_To (The_Picture_Direction);
      The_Picture_Direction := Space.Unknown_Direction;
    end Synch_On_Picture;

  begin -- Control_Task
    accept Start;
    Startup;
    loop
      begin
        select
          accept Close;
          exit;
        or
          accept Start do
            Startup;
          end Start;
        or
          accept Initialize do
            M_Zero.Initialize;
          end Initialize;
        or
          accept Go_To do
            M_Zero.Slew_To (Actual_Target_Direction);
          end Go_To;
        or
          accept Align do
            Synch_On_Picture;
            M_Zero.Slew_To (Actual_Target_Direction);
         end Align;
        or
          accept Synch do
            if Aligning_Enabled then
              Synch_On_Picture;
            else
              M_Zero.Synch_To (Actual_Target_Direction);
            end if;
          end Synch;
        or
          accept Start_Moving (Direction : Moving_Direction) do
            if not (The_Status in Approaching | Solving) then
              M_Zero.Start_Moving (M_Zero.Moving_Direction(Direction));
            end if;
          end Start_Moving;
        or
          accept Stop_Moving (Direction : Moving_Direction) do
            M_Zero.Stop_Moving (M_Zero.Moving_Direction(Direction));
          end Stop_Moving;
        or
          accept Stop_Moving do
            M_Zero.Stop_Moving;
          end Stop_Moving;
        or
          accept Increase_Moving_Rate do
            if The_Status /= Approaching and then The_Moving_Rate < Moving_Rate'last then
              The_Moving_Rate := Moving_Rate'succ(The_Moving_Rate);
              Set (The_Moving_Rate);
            end if;
          end Increase_Moving_Rate;
        or
          accept Decrease_Moving_Rate do
            if The_Status /= Approaching and then The_Moving_Rate > Moving_Rate'first then
              The_Moving_Rate := Moving_Rate'pred(The_Moving_Rate);
              Set (The_Moving_Rate);
            end if;
          end Decrease_Moving_Rate;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := Telescope.State(The_Information.Status);
            The_Data.Target_Direction := Actual_Target_Direction;
            The_Data.Actual_Direction := The_Information.Direction;
            The_Data.Picture_Direction := The_Picture_Direction;
            The_Data.Universal_Time := Time.Universal;
            The_Data.Cone_Error := Alignment.Cone_Error;
            The_Data.Pole_Offsets := Alignment.Pole_Offsets;
            The_Data.Actual_Moving_Rate := The_Moving_Rate;
          end Get;
        or delay Update_Delay;
          Signal_Information_Update.all;
          Update_Handling;
        end select;
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
