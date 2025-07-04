-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Input;

package body Handbox.PWI4 is

  Is_Moving           : Boolean := False;
  Center_Is_Pressed   : Boolean := False;
  Left_Action_Pending : Boolean := False;
  Is_Active           : Boolean := False;


  procedure Handle (The_Command : Command) is

    procedure Execute (Item : Input.Command) is
    begin
      Input.Put (Item, Input.Handbox);
    end Execute;

  begin -- Handle
    case The_Command is
    when Up_Pressed =>
      if Center_Is_Pressed then
        Execute (Input.Next_Speed);
        Is_Active := True;
      elsif not Is_Moving then
        Execute (Input.Move_Up);
        Is_Moving := True;
      end if;
    when Down_Pressed =>
      if Center_Is_Pressed then
        Execute (Input.Previous_Speed);
        Is_Active := True;
      elsif not Is_Moving then
        Execute (Input.Move_Down);
        Is_Moving := True;
      end if;
    when Left_Pressed =>
      if Center_Is_Pressed then
        Left_Action_Pending := True;
      elsif not Is_Moving then
        Execute (Input.Move_Left);
        Is_Moving := True;
      end if;
    when Right_Pressed =>
      if Center_Is_Pressed then
        Execute (Input.Spiral_Offset_Next);
        Is_Active := True;
      elsif not Is_Moving then
        Execute (Input.Move_Right);
        Is_Moving := True;
      end if;
    when Center_Pressed =>
      if not Is_Moving then
        Center_Is_Pressed := True;
        Is_Active := False;
      end if;
    when Up_Released | Down_Released | Left_Released | Right_Released =>
      if Is_Moving then
        Is_Moving := False;
        Execute (Input.End_Command);
      elsif Left_Action_Pending then
        Execute (Input.Spiral_Offset_Previous);
        Is_Active := True;
      end if;
      Left_Action_Pending := False;
    when Center_Released =>
      if not Is_Moving then
        if Left_Action_Pending then
          Execute (Input.Spiral_Offset_Center);
          Left_Action_Pending := False;
        elsif not Is_Active then
          Execute (Input.Go_Back);
        end if;
      end if;
      Center_Is_Pressed := False;
      Is_Active := False;
    when Stop =>
      if Is_Moving then
        Is_Moving := False;
        Execute (Input.End_Command);
      end if;
      Center_Is_Pressed := False;
      Left_Action_Pending := False;
      Is_Active := False;
    end case;
  end Handle;

end Handbox.PWI4;
