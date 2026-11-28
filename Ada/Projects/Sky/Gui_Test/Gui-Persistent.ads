-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

private package Gui.Persistent is

  procedure Create (Title               : String;
                    Termination_Routine : Action_Routine;
                    Initial_Metrics     : Window_Metrics;
                    Always_Topmost      : Boolean);

  function Create (Title           : String;
                   Initial_Metrics : Window_Metrics := Default_Metrics) return Detached_Window;

  function Stored_Color_Mode return Color_Mode;

  procedure Save_Color_Mode (Mode : Color_Mode);

  function Stored_Detached_Fullscreen return Boolean;

  procedure Save_Detached_Fullscreen (Fullscreen : Boolean);

  type Column_Widths is array (Positive range <>) of Natural;

  function Widths_Of (Id : String) return Column_Widths;

  procedure Save_Widths (Id     : String;
                         Widths : Column_Widths);

end Gui.Persistent;
