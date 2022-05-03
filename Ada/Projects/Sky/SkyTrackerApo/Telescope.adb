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

with Parameter;
with Traces;

package body Telescope is

  package Log is new Traces ("Telescope");


  task type Control_Task is

    entry Go_To;

    entry Park;

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


  procedure Go_To is
  begin
    Control.Go_To;
  end Go_To;


  procedure Park is
  begin
    Control.Park;
  end Park;


  procedure Unpark is
  begin
    Control.Unpark;
  end Unpark;


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

    The_Information  : Ten_Micron.Information;
    The_Status       : State       := Disconnected;


    procedure Get_Information is
    begin
      if The_Status = Disconnected then
        Ten_Micron.Startup (Parameter.Ten_Micron_Ip_Address, Parameter.Ten_Micron_Port);
      end if;
      The_Information := Ten_Micron.Get;
      The_Status := Telescope.State(The_Information.Status);
    end Get_Information;


    procedure Update_Handling is
    begin
      Get_Information;
      Signal_Information_Update.all;
    end Update_Handling;

  begin -- Control_Task
    Get_Information;
    loop
      begin
        select
          accept Close;
          exit;
        or
          accept Go_To do
            Ten_Micron.Slew_To (Actual_Target_Direction);
          end Go_To;
        or
          accept Park do
            Ten_Micron.Park;
          end Park;
        or
          accept Unpark do
            Ten_Micron.Unpark;
          end Unpark;
        or
          accept Get (The_Data : out Data) do
            The_Data.Status := Telescope.State(The_Information.Status);
            The_Data.Target_Direction := Actual_Target_Direction;
            The_Data.Actual_Direction := The_Information.Direction;
            The_Data.Universal_Time := Time.Universal;
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
