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

pragma Build (Description => "GUI Handbox Test",
              Version     => (0, 0, 0, 0),
              Kind        => Windows,
              Icon        => False,
              Compiler    => "GNAT\14.2");

with Ada.Text_IO;
with Gui;

procedure Gui_Handbox_Test is
  Page : Gui.Page;
  Handbox, Top, Middle, Bottom : Gui.Group;
  Up, Left, Center, Right, Down : Gui.Button with Unreferenced;

  procedure Pressed is
  begin
    Ada.Text_IO.Put_Line ("button pressed");
  end Pressed;

  procedure Startup is
  begin
    Page := Gui.Add_Page ("Handbox");
    Handbox := Gui.Create (Page, Gui.Vertical, Equal_Cells => True, Aspect_Ratio => 1.0);
    Top := Gui.Create (Handbox, Gui.Horizontal, Equal_Cells => True);
    Gui.Space (Top);
    Up := Gui.Create (Top, "Up", Pressed'unrestricted_access, Expand => True);
    Gui.Space (Top);
    Middle := Gui.Create (Handbox, Gui.Horizontal, Equal_Cells => True);
    Left := Gui.Create (Middle, "Left", Pressed'unrestricted_access, Expand => True);
    Center := Gui.Create (Middle, "Stop", Pressed'unrestricted_access, Expand => True);
    Right := Gui.Create (Middle, "Right", Pressed'unrestricted_access, Expand => True);
    Bottom := Gui.Create (Handbox, Gui.Horizontal, Equal_Cells => True);
    Gui.Space (Bottom);
    Down := Gui.Create (Bottom, "Down", Pressed'unrestricted_access, Expand => True);
    Gui.Space (Bottom);
  end Startup;

begin
  Gui.Execute ("Gui Handbox Test", Startup'access,
               Initial_Metrics => (100, 100, 420, 420));
end Gui_Handbox_Test;
