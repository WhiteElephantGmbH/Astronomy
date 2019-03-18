-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Gdk.Types.Keysyms;

package Gui.Key_Codes is

  KP_Add      : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Add);
  KP_Divide   : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Divide);
  KP_Multiply : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Multiply);
  KP_Subtract : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Subtract);

  KP_Decimal : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Decimal);
  KP_Enter   : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Enter);

  KP_Down  : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Down);
  KP_Left  : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Left);
  KP_Right : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Right);
  KP_Up    : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_Up);

  KP_0 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_0);
  KP_1 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_1);
  KP_2 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_2);
  KP_3 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_3);
  KP_4 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_4);
  KP_5 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_5);
  KP_6 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_6);
  KP_7 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_7);
  KP_8 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_8);
  KP_9 : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_KP_9);

  K_Back   : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_BackSpace);
  K_Delete : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Delete);
  K_Menu   : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Menu);
  K_Return : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Return);
  K_Tab    : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Tab);

  K_Page_Down : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Next);
  K_Page_Up   : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Prior);

  K_Down  : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Down);
  K_Left  : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Left);
  K_Right : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Right);
  K_Up    : constant Key_Code := Key_Code(Gdk.Types.Keysyms.GDK_Up);

end Gui.Key_Codes;
