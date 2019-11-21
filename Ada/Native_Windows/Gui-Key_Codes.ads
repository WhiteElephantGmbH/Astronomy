-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Win32.Winuser;

package Gui.Key_Codes is

  KP_Add      : constant Key_Code := Win32.Winuser.VK_ADD;
  KP_Divide   : constant Key_Code := Win32.Winuser.VK_DIVIDE;
  KP_Multiply : constant Key_Code := Win32.Winuser.VK_MULTIPLY;
  KP_Subtract : constant Key_Code := Win32.Winuser.VK_SUBTRACT;

  KP_Decimal : constant Key_Code := Win32.Winuser.VK_DECIMAL;
  KP_Enter   : constant Key_Code := Win32.Winuser.VK_RETURN;

  KP_Down  : constant Key_Code := Win32.Winuser.VK_DOWN;
  KP_Left  : constant Key_Code := Win32.Winuser.VK_LEFT;
  KP_Right : constant Key_Code := Win32.Winuser.VK_RIGHT;
  KP_Up    : constant Key_Code := Win32.Winuser.VK_UP;

  KP_0 : constant Key_Code := Win32.Winuser.VK_NUMPAD0;
  KP_1 : constant Key_Code := Win32.Winuser.VK_NUMPAD1;
  KP_2 : constant Key_Code := Win32.Winuser.VK_NUMPAD2;
  KP_3 : constant Key_Code := Win32.Winuser.VK_NUMPAD3;
  KP_4 : constant Key_Code := Win32.Winuser.VK_NUMPAD4;
  KP_5 : constant Key_Code := Win32.Winuser.VK_NUMPAD5;
  KP_6 : constant Key_Code := Win32.Winuser.VK_NUMPAD6;
  KP_7 : constant Key_Code := Win32.Winuser.VK_NUMPAD7;
  KP_8 : constant Key_Code := Win32.Winuser.VK_NUMPAD8;
  KP_9 : constant Key_Code := Win32.Winuser.VK_NUMPAD9;

  K_Back   : constant Key_Code := Win32.Winuser.VK_BACK;
  K_Delete : constant Key_Code := Win32.Winuser.VK_DELETE;
  K_Menu   : constant Key_Code := Win32.Winuser.VK_MENU;
  K_Return : constant Key_Code := -1; -- not used
  K_Space  : constant Key_Code := Win32.Winuser.VK_SPACE;
  K_Tab    : constant Key_Code := Win32.Winuser.VK_TAB;

  K_Page_Down : constant Key_Code := Win32.Winuser.VK_NEXT;
  K_Page_Up   : constant Key_Code := Win32.Winuser.VK_PRIOR;

  K_Down  : constant Key_Code := -2; -- not used
  K_Left  : constant Key_Code := -3; -- not used
  K_Right : constant Key_Code := -4; -- not used
  K_Up    : constant Key_Code := -5; -- not used

end Gui.Key_Codes;
