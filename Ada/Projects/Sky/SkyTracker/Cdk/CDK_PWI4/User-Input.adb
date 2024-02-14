-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
                   End_Command,
                   Next_Speed,
                   Previous_Speed,
                   Spiral_Offset_Center,
                   Spiral_Offset_Next,
                   Spiral_Offset_Previous,
                   Go_Back,
                   Rotate,
                   Add_Point,
                   Stop);

  subtype Move is Command range Move_Up .. Move_Right;

  subtype Change_Speed is Command range Next_Speed .. Previous_Speed;

  subtype Spiral_Offset is Command range Spiral_Offset_Center .. Spiral_Offset_Previous;

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
          if From_Source = From and The_Command = Device.End_Command then
            Active_Command := End_Command;
            New_Command := True;
          end if;
        when End_Command | Go_Back | Change_Speed | Spiral_Offset | Add_Point | Rotate =>
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
        when Device.Go_Back =>
          Active_Command := Go_Back;
        when Device.Move_Up =>
          Active_Command := Move_Up;
        when Device.Move_Down =>
          Active_Command := Move_Down;
        when Device.Move_Left =>
          Active_Command := Move_Left;
        when Device.Move_Right =>
          Active_Command := Move_Right;
        when Device.Spiral_Offset_Center =>
          Active_Command := Spiral_Offset_Center;
        when Device.Spiral_Offset_Next =>
          Active_Command := Spiral_Offset_Next;
        when Device.Spiral_Offset_Previous =>
          Active_Command := Spiral_Offset_Previous;
        when Device.Previous_Speed =>
          Active_Command := Previous_Speed;
        when Device.Next_Speed =>
          Active_Command := Next_Speed;
        when Device.Rotate =>
          Active_Command := Rotate;
        when Device.Add_Point =>
          Active_Command := Add_Point;
        when Device.End_Command =>
          return;
        end case;
        New_Command := True;
        From_Source := From;
        Is_Active := True;
      end if;
    end Set;


    function In_Action return Boolean is
    begin
      return Active_Command in Move;
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
      when Move =>
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
    Is_Changing : Boolean := False;
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
        when Go_Back =>
          User.Back_Handling;
        when Move_Up =>
          Telescope.Execute (Telescope.Move_Up);
          Is_Changing := True;
        when Move_Down =>
          Telescope.Execute (Telescope.Move_Down);
          Is_Changing := True;
        when Move_Left =>
          Telescope.Execute (Telescope.Move_Left);
          Is_Changing := True;
        when Move_Right =>
          Telescope.Execute (Telescope.Move_Right);
          Is_Changing := True;
        when End_Command =>
          Is_Changing := False;
          Telescope.Execute (Telescope.End_Command);
        when Previous_Speed =>
          Telescope.Execute (Telescope.Previous_Speed);
        when Next_Speed =>
          Telescope.Execute (Telescope.Next_Speed);
        when Spiral_Offset_Center =>
          Telescope.Execute (Telescope.Spiral_Offset_Center);
        when Spiral_Offset_Next =>
          Telescope.Execute (Telescope.Spiral_Offset_Next);
        when Spiral_Offset_Previous =>
          Telescope.Execute (Telescope.Spiral_Offset_Previous);
        when Add_Point =>
          Telescope.Execute (Telescope.Add_Point);
        when Rotate =>
          Telescope.Execute (Telescope.Rotate);
        end case;
      or
        delay 20.0;
        if Is_Changing then
          Telescope.Execute (Telescope.End_Command);
          Is_Changing := False;
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
