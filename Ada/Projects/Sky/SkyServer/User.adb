-- *********************************************************************************************************************
-- *                            (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                             *
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

with Application;
with Gui.Registered;
with Traces;

package body User is

  package Log is new Traces ("User");

  Main_Page     : Gui.Page;
  Progress_Bar : Gui.Progress_Bar;
  Text_View     : Gui.Text_View;


  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Exit_Handler        : not null access procedure;
                     The_Termination_Handler : not null access procedure) is

    procedure Create_Interface is
    begin
      Main_Page := Gui.Add_Page ("Main");
      Progress_Bar := Gui.Create (Main_Page);
      Gui.Define_Range (Progress_Bar, 100);
      Text_View := Gui.Create (Main_Page, Word_Wrapping => True);
      The_Startup_Handler.all;
    exception
    when Item: others =>
      Log.Termination (Item);
    end Create_Interface;

    procedure Termination is
    begin
      The_Exit_Handler.all;
      Log.Write ("terminating");
      The_Termination_Handler.all;
    exception
    when others =>
      Log.Error ("Termination failed");
    end Termination;

  begin -- Execute
    Gui.Registered.Execute (The_Application_Name    => Application.Title,
                            The_Startup_Routine     => Create_Interface'access,
                            The_Termination_Routine => Termination'access);
    Log.Write ("terminated");
  end Execute;


  procedure Show (Message : String) is
  begin
    Gui.Append_Line_To (Text_View, Message);
  end Show;


  procedure Show_Error (Message : String) is
  begin
    Gui.Message_Box (Message);
  end Show_Error;


  procedure Show_Progress (Item    : String;
                           Percent : Siril_Progress.Percent) is
    use type Siril_Progress.Percent;
  begin
    if Percent = Siril_Progress.Percent'first then
      Show ("Siril: " & Item);
      Gui.Report_Progress (Progress_Bar, 0);
    elsif Percent = Siril_Progress.Percent'last then
      Gui.Report_Progress (Progress_Bar, 0);
    else
      Gui.Report_Progress (Progress_Bar, Natural(Percent));
    end if;
  end Show_Progress;

end User;
