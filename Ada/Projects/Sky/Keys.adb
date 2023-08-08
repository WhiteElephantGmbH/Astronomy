-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Gui.Key_Codes;
with Traces;

package body Keys is

  package Log is new Traces ("User");

  Moving_Left      : Boolean := False;
  Moving_Right     : Boolean := False;
  Moving_Up        : Boolean := False;
  Moving_Down      : Boolean := False;
  Time_Changing    : Boolean := False;
  Enter_Is_Pressed : Boolean := False;
  Ignore_Next      : Boolean := False;

  procedure Handler (The_Event    : Gui.Key_Event;
                     The_Key_Code : Gui.Key_Code) is
  begin
    case The_Event is
    when Gui.Key_Pressed =>
      Log.Write ("Key pressed: " & The_Key_Code'img);
      if Ignore_Next then
        return;
      end if;
      case The_Key_Code is
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down | Gui.Key_Codes.K_Down =>
        if Enter_Is_Pressed then
          Put (Decrease_Speed);
        else
          Moving_Down := True;
          Put (Move_Down);
        end if;
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up | Gui.Key_Codes.K_Up =>
        if Enter_Is_Pressed then
          Put (Increase_Speed);
        else
          Moving_Up := True;
          Put (Move_Up);
        end if;
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left | Gui.Key_Codes.K_Left =>
        if Enter_Is_Pressed then
          if not Time_Changing then
            Put (Decrease_Time);
            Time_Changing := True;
          end if;
        else
          Put (Move_Left);
          Moving_Left := True;
        end if;
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right | Gui.Key_Codes.K_Right =>
        if Enter_Is_Pressed then
          if not Time_Changing then
            Put (Increase_Time);
            Time_Changing := True;
          end if;
        else
          Put (Move_Right);
          Moving_Right := True;
        end if;
      when Gui.Key_Codes.KP_Add | Gui.Key_Codes.K_Page_Up =>
        Put (Increase_Speed);
      when Gui.Key_Codes.KP_Subtract | Gui.Key_Codes.K_Page_Down =>
        Put (Decrease_Speed);
      when Gui.Key_Codes.KP_Enter | Gui.Key_Codes.K_Return =>
        Enter_Is_Pressed := True;
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := True;
      when others =>
        null;
      end case;
    when Gui.Key_Released =>
      Log.Write ("Key released: " & The_Key_Code'img);
      case The_Key_Code is
      when Gui.Key_Codes.K_Menu =>
        Ignore_Next := False;
      when Gui.Key_Codes.KP_2 | Gui.Key_Codes.KP_Down  | Gui.Key_Codes.K_Down =>
        if Moving_Down then
          Put (Move_Down_End);
          Moving_Down := False;
        end if;
      when Gui.Key_Codes.KP_8 | Gui.Key_Codes.KP_Up | Gui.Key_Codes.K_Up =>
        if Moving_Up then
          Put (Move_Up_End);
          Moving_Up := False;
        end if;
      when Gui.Key_Codes.KP_4 | Gui.Key_Codes.KP_Left | Gui.Key_Codes.K_Left =>
        if Moving_Left then
          Put (Move_Left_End);
          Moving_Left := False;
        end if;
        if Time_Changing then
          Put (Change_Time_End);
          Time_Changing := False;
        end if;
      when Gui.Key_Codes.KP_6 | Gui.Key_Codes.KP_Right | Gui.Key_Codes.K_Right =>
        if Moving_Right then
          Put (Move_Right_End);
          Moving_Right := False;
        end if;
        if Time_Changing then
          Put (Change_Time_End);
          Time_Changing := False;
        end if;
      when Gui.Key_Codes.KP_Enter | Gui.Key_Codes.K_Return =>
        Enter_Is_Pressed := False;
        if not (Moving_Left or Moving_Right or Moving_Up or Moving_Down or Time_Changing) then
          Put (Enter);
        end if;
      when others =>
        null;
      end case;
    end case;
  exception
  when others =>
    Log.Error ("Key_Handler failed");
  end Handler;

end Keys;
