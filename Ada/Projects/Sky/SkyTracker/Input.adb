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

package body Input is

  package Log is new Traces ("Input");

  subtype Move is Command range Move_Left .. Move_Down;

  subtype Change_Speed is Command range Next_Speed .. Previous_Speed;

  subtype Spiral_Offset is Command range Spiral_Offset_Center .. Spiral_Offset_Previous;

  protected Manager is

    procedure Set (The_Command : Command;
                   From        : Source);

    procedure Finish;

    entry Take (The_Command : out Command;
                Is_Closing  : out Boolean);

  private
    Finishing      : Boolean := False;
    New_Command    : Boolean := False;
    Is_Active      : Boolean := False;
    Active_Command : Command;
    From_Source    : Source;
  end Manager;


  protected body Manager is

    procedure Set (The_Command : Command;
                   From        : Source) is
    begin
      if Finishing then
        return;
      elsif Is_Active then
        case Active_Command is
        when Stop =>
          return;
        when Move =>
          if From_Source = From and The_Command = End_Command then
            Active_Command := End_Command;
            New_Command := True;
          end if;
        when End_Command | Go_Back | Change_Speed | Spiral_Offset | Time_Change | Add_Point | Rotate =>
          null;
        end case;
        if The_Command = Stop then
          Active_Command := Stop;
          From_Source := From;
          New_Command := True;
        end if;
      else
        case The_Command is
        when End_Command =>
          return;
        when others =>
          Active_Command := The_Command;
        end case;
        New_Command := True;
        From_Source := From;
        Is_Active := True;
      end if;
    end Set;


    procedure Finish is
    begin
      Finishing := True;
    end Finish;


    entry Take (The_Command : out Command;
                Is_Closing  : out Boolean) when New_Command or Finishing is
    begin
      The_Command := Active_Command;
      case The_Command is
      when Move =>
        null;
      when others =>
        Is_Active := False;
      end case;
      New_Command := False;
      Is_Closing := Finishing;
      Finishing := False;
    end Take;

  end Manager;


  procedure Put (The_Command : Command;
                 From        : Source) is
  begin
    Log.Write ("Command => " & The_Command'img & " from " & From'img);
    Manager.Set (The_Command, From);
  end Put;


  procedure Close is
  begin
    Manager.Finish;
  end Close;


  task type Handler (Execute : access procedure (Item : Command));

  task body Handler is
    The_Command : Command;
    Is_Closing  : Boolean;
    Is_Changing : Boolean := False;
  begin
    Log.Write ("Started");
    loop
      select
        Manager.Take (The_Command, Is_Closing);
        exit when Is_Closing;
        Log.Write ("Manager.Execute " & The_Command'img);
        case The_Command is
        when Move =>
          Execute (The_Command);
          Is_Changing := True;
        when End_Command =>
          Is_Changing := False;
          Execute (End_Command);
        when others =>
          Execute (The_Command);
        end case;
      or
        delay 20.0;
        if Is_Changing then
          Execute (End_Command);
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

  procedure Open (Execute : access procedure (Item : Command)) is
  begin
    The_Handler := new Handler (Execute);
  end Open;

end Input;
