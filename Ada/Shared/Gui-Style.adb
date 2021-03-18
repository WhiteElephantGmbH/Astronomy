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

with Ada.Environment_Variables;
with File;
with Glib.Error;
with Gtk.Css_Provider;
with Gdk.Screen;
with Gtk.Style_Context;
with Gtk.Style_Provider;
with Os;

package body Gui.Style is

  function Local_App_Data return File.Folder is
    use type File.Folder;
  begin
    if Os.Is_Windows then
      return File.Folder(Ada.Environment_Variables.Value ("LocalAppData"));
    else
      return Ada.Environment_Variables.Value ("HOME") + ".local";
    end if;
  end Local_App_Data;


  procedure Load_Css_File is
  begin
    declare
      use type File.Folder;
      The_Folder : constant File.Folder := Local_App_Data + "Gtk-3.0";
      The_File   : constant String := File.Composure (Directory => The_Folder,
                                                    Filename  => "Soudronic",
                                                    Extension => "css");
    begin
      if File.Exists (The_File) then
        declare
          Provider   : Gtk.Css_Provider.Gtk_Css_Provider;
          Any_Errors : aliased Glib.Error.GError;
          use Gtk.Css_Provider;
        begin
          Gtk.Css_Provider.Gtk_New (Provider);
          if Provider.Load_From_Path (The_File, Any_Errors'access) then
            Gtk.Style_Context.Add_Provider_For_Screen (Gdk.Screen.Get_Default,
                                                       +Provider,
                                                       Gtk.Style_Provider.Priority_Application);
          end if;
          Provider.Unref;
        end;
      end if;
    end;
  exception
  when others =>
    null;  -- In case File.Exists raises an exception due to malformed file name
  end Load_Css_File;


  procedure Apply_Rule (Css_Rule : String) is
    Provider   : Gtk.Css_Provider.Gtk_Css_Provider;
    Any_Errors : aliased Glib.Error.GError;
    use Gtk.Css_Provider;
  begin
    Gtk.Css_Provider.Gtk_New (Provider);
    if Provider.Load_From_Data (Css_Rule, Any_Errors'access) then
      Gtk.Style_Context.Add_Provider_For_Screen (Gdk.Screen.Get_Default,
                                                 +Provider,
                                                 Gtk.Style_Provider.Priority_Application);
    end if;
    Provider.Unref;
  end Apply_Rule;


  procedure Apply_Rule_To (The_Widget : not null access Gtk.Widget.Gtk_Widget_Record'class;
                           Css_Rule   : String) is
    Provider   : Gtk.Css_Provider.Gtk_Css_Provider;
    Any_Errors : aliased Glib.Error.GError;
  begin
    Gtk.Css_Provider.Gtk_New (Provider);
    if Provider.Load_From_Data (Css_Rule, Any_Errors'access) then
      declare
        The_Context : Gtk.Style_Context.Gtk_Style_Context;
        use Gtk.Css_Provider;
      begin
        The_Context := Gtk.Style_Context.Get_Style_Context (The_Widget);
        The_Context.Add_Provider (+Provider, Gtk.Style_Provider.Priority_Application);
      end;
    end if;
    Provider.Unref;
  end Apply_Rule_To;


  procedure Change_Color (The_Progress_Bar : Gtk.Progress_Bar.Gtk_Progress_Bar;
                          The_Color        : String) is
  begin
    Apply_Rule_To (The_Progress_Bar, ".progressbar {background-image: none; background-color: " & The_Color & ";}");
  end Change_Color;

end Gui.Style;
