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

with Gtk.Window;

private package Gui.Router is

  type Request_Data is abstract tagged null record;

  procedure Request (Data : in out Request_Data'class);

  procedure Synchronous_Service (Data : in out Request_Data) is abstract;

  procedure Execute (Application_Name    : String;
                     Startup_Routine     : access procedure (Window : Gtk.Window.Gtk_Window);
                     Termination_Routine : Action_Routine;
                     Period              : Duration);

  function Get_Window_Metrics (For_Window : Gtk.Window.Gtk_Window) return Window_Metrics;

  procedure Close (The_Window : Gtk.Window.Gtk_Window);

  procedure Execute (The_Action : Action_Routine);

  procedure Execute (The_Action      : Click_Routine;
                     The_Information : Information);

  procedure Execute (The_Action  : Key_Handler;
                     The_Event   : Key_Event;
                     The_Keycode : Key_Code);

  type Message_Data is abstract tagged null record;

  procedure Asynchronous_Service (The_Message : Message_Data) is abstract;

  procedure Send (Message : Message_Data'class);


end Gui.Router;
