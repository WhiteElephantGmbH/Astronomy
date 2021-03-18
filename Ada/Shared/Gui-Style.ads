-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Soudronic;

private package Gui.Style is

  procedure Load_Css_File;
  --
  -- Loads the CSS rules contained in the file Soudronic.css
  -- Under Windows from the folder %LocalAppData%\Gtk-3.0
  -- otherise from $HOME.local/Gtk-3.0
  --
  -- If the file is missing or the contents contain syntax errors the load fails without error.
  --

  procedure Apply_Rule_To (The_Widget : not null access Gtk.Widget.Gtk_Widget_Record'class;
                           Css_Rule   : String);
  --
  -- Applies the specified CSS rule to the specified widget
  --

  procedure Apply_Rule (Css_Rule : String);
  --
  -- Applies the specified CSS rule (for the application from now on)
  --

  procedure Change_Color (The_Progress_Bar : Gtk.Progress_Bar.Gtk_Progress_Bar;
                          The_Color        : String);
  -- Change the progress color of the specified progress bar
  -- The_Color should either be the name of an X11 color or
  -- the symbol # immediately followed the 6 hexadecimal digits denoting the RBG value
  --
end Gui.Style;
