-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Traces;

package body User.Input is

  package Log is new Traces ("Input");

  type Command is (Close,
                   Move_Up,
                   Move_Down,
                   Move_Left,
                   Move_Right,
                   End_Move,
                   Decrease_Time,
                   Increase_Time,
                   End_Change,
                   Decrease_Speed,
                   Increase_Speed,
                   Enter,
                   Stop);

  subtype Move is Command range Move_Up .. Move_Right;

  subtype Change_Speed is Command range Decrease_Speed .. Increase_Speed;

  subtype Change_Time is Command range Decrease_Time .. Increase_Time;

  protected Manager is

    procedure Set (The_Command : Device.Command;
                   From        : Source);

    function In_Action return Boolean;

    procedure Finish;

    entry Take (The_Command : out Command);

  private
    New_Command    : Boolean := False;
    Is_Active      : Boolean := False;
    Active_Command : Command;
    From_Source    : Source;
  end Manager;


  protected body Manager is

    procedure Set (The_Command : Device.Command;
                   From        : Source) is
      use type Device.Command;
    begin
      if Is_Active then
        case Active_Command is
        when Close | Stop =>
          return;
        when Move =>
          if From_Source = From and The_Command = Device.No_Command then
            Active_Command := End_Move;
            New_Command := True;
          end if;
        when Change_Time =>
          if From_Source = From and The_Command = Device.No_Command then
            Active_Command := End_Change;
            New_Command := True;
          end if;
        when End_Move | End_Change | Enter | Change_Speed =>
          null;
        end case;
        if The_Command = Device.Stop then
          Active_Command := Stop;
          From_Source := From;
          New_Command := True;
        end if;
      else
        case The_Command is
        when Device.Stop =>
          Active_Command := Stop;
        when Device.Enter =>
          Active_Command := Enter;
        when Device.Move_Up =>
          Active_Command := Move_Up;
        when Device.Move_Down =>
          Active_Command := Move_Down;
        when Device.Move_Left =>
          Active_Command := Move_Left;
        when Device.Move_Right =>
          Active_Command := Move_Right;
        when Device.Decrease_Time =>
          Active_Command := Decrease_Time;
        when Device.Increase_Time =>
          Active_Command := Increase_Time;
        when Device.Decrease_Speed =>
          Active_Command := Decrease_Speed;
        when Device.Increase_Speed =>
          Active_Command := Increase_Speed;
        when Device.No_Command =>
          return;
        end case;
        New_Command := True;
        From_Source := From;
        Is_Active := True;
      end if;
    end Set;


    function In_Action return Boolean is
    begin
      return Active_Command in Move | Change_Time;
    end In_Action;


    procedure Finish is
    begin
      Active_Command := Close;
      Is_Active := True;
      New_Command := True;
    end Finish;


    entry Take (The_Command : out Command) when New_Command is
    begin
      The_Command := Active_Command;
      case The_Command is
      when Move | Change_Time =>
        null;
      when others =>
        Is_Active := False;
      end case;
      New_Command := False;
    end Take;

  end Manager;


  procedure Put (The_Command : Device.Command;
                 From        : Source) is
  begin
    Log.Write ("Command => " & The_Command'img & " from " & From'img);
    Manager.Set (The_Command, From);
  end Put;


  function Is_Active return Boolean is
  begin
    return Manager.In_Action;
  end Is_Active;


  procedure Close is
  begin
    Manager.Finish;
  end Close;


  task type Handler;

  task body Handler is
    The_Command : Command;
    Is_Moving   : Boolean := False;
  begin
    Log.Write ("Started");
    loop
      select
        Manager.Take (The_Command);
        Log.Write ("Manager.Execute " & The_Command'img);
        case The_Command is
        when Close =>
          exit;
        when Stop =>
          User.Perform_Stop;
        when Enter =>
          User.Enter_Handling;
        when Move_Up =>
          Telescope.Execute (Telescope.Move_Up);
          Is_Moving := True;
        when Move_Down =>
          Telescope.Execute (Telescope.Move_Down);
          Is_Moving := True;
        when Move_Left =>
          Telescope.Execute (Telescope.Move_Left);
          Is_Moving := True;
        when Move_Right =>
          Telescope.Execute (Telescope.Move_Right);
          Is_Moving := True;
        when End_Move =>
          Telescope.Execute (Telescope.End_Move);
          Is_Moving := False;
        when Decrease_Speed =>
          Telescope.Execute (Telescope.Decrease_Speed);
        when Increase_Speed =>
          Telescope.Execute (Telescope.Increase_Speed);
        when Decrease_Time =>
          Telescope.Execute (Telescope.Decrease_Time);
        when Increase_Time =>
          Telescope.Execute (Telescope.Increase_Time);
        when End_Change =>
          Telescope.Execute (Telescope.End_Change);
        end case;
      or
        delay 20.0;
        if Is_Moving then
          Telescope.Execute (Telescope.End_Move);
          Is_Moving := False;
        end if;
      end select;
    end loop;
    Log.Write ("Terminated");
  exception
  when Occurence: others =>
    Log.Termination (Occurence);
  end Handler;


  The_Handler : access Handler with Unreferenced;

  procedure Open is
  begin
    The_Handler := new Handler;
  end Open;

end User.Input;
