-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Device;
with User.Input;

package body Handbox.PWI2 is

  Arrow_Was_Pressed : Boolean := False;
  Center_Is_Pressed : Boolean := False;
  Is_Changing       : Boolean := False;


  procedure Handle (The_Command : Command) is

    procedure Execute (Item : Device.Command) is
    begin
      User.Input.Put (Item, User.Input.Handbox);
    end Execute;

  begin
    case The_Command is
    when Up_Pressed =>
      if Center_Is_Pressed then
        Execute (Device.Next_Speed);
        Arrow_Was_Pressed := True;
      else
        Is_Changing := True;
        Execute (Device.Move_Up);
      end if;
    when Down_Pressed =>
      if Center_Is_Pressed then
       Execute (Device.Previous_Speed);
        Arrow_Was_Pressed := True;
      else
        Is_Changing := True;
        Execute (Device.Move_Down);
      end if;
    when Left_Pressed =>
      Is_Changing := True;
      if Center_Is_Pressed then
        Execute (Device.Decrease_Time);
        Arrow_Was_Pressed := True;
      else
        Execute (Device.Move_Left);
      end if;
    when Right_Pressed =>
      Is_Changing := True;
      if Center_Is_Pressed then
        Execute (Device.Increase_Time);
        Arrow_Was_Pressed := True;
      else
        Execute (Device.Move_Right);
      end if;
    when Center_Pressed =>
      Center_Is_Pressed := True;
      Arrow_Was_Pressed := False;
    when Up_Released | Down_Released | Left_Released | Right_Released =>
      if Is_Changing then
        Is_Changing := False;
        Execute (Device.End_Command);
      end if;
    when Center_Released =>
      Center_Is_Pressed := False;
      if not Arrow_Was_Pressed then
        Execute (Device.Go_Back);
      end if;
    when Stop =>
      if Is_Changing then
        Is_Changing := False;
        Execute (Device.End_Command);
      end if;
      Arrow_Was_Pressed := False;
      Center_Is_Pressed := False;
    end case;
  end Handle;

end Handbox.PWI2;
