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

with Ada.Containers.Indefinite_Ordered_Maps;
with Persistent;

package body Gui.Persistent is

  package Column_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, Column_Widths);

  package Persistent_Columns is new Standard.Persistent (Column_Maps.Map, "List_Columns");

  The_Column_Data : Persistent_Columns.Data;
  The_Columns     : Column_Maps.Map renames The_Column_Data.Storage;


  function Widths_Of (Id : String) return Column_Widths is
  begin
    if The_Columns.Contains (Id) then
      return The_Columns.Element (Id);
    end if;
    return [];
  end Widths_Of;


  procedure Save_Widths (Id     : String;
                         Widths : Column_Widths) is
  begin
    The_Columns.Include (Id, Widths);
  end Save_Widths;


  package Persistent_Metrics is new Standard.Persistent (Window_Metrics, "Metrics");

  The_Metrics_Data : Persistent_Metrics.Data;
  The_Metrics      : Window_Metrics renames The_Metrics_Data.Storage;

  The_Termination_Routine : Action_Routine;


  procedure Termination is
  begin
    if not (Application_Is_Minimized or Application_Is_Maximized) then
      The_Metrics := Get_Persistent_Window_Metrics;
    end if;
    if The_Termination_Routine /= null then
      The_Termination_Routine.all;
    end if;
  end Termination;


  procedure Create (Title               : String;
                    Termination_Routine : Action_Routine;
                    Initial_Metrics     : Window_Metrics;
                    Always_Topmost      : Boolean) is

  begin
    if Persistent_Metrics.Storage_Is_Empty then
      The_Metrics := Initial_Metrics;
    end if;
    The_Termination_Routine := Termination_Routine;
    Gui.Create (Title,
                Termination'access,
                The_Metrics,
                Always_Topmost);
  end Create;


  package Persistent_Detached_Metrics is new Standard.Persistent (Window_Metrics, "Detached_Metrics");

  The_Detached_Metrics_Data : Persistent_Detached_Metrics.Data;
  The_Detached_Metrics      : Window_Metrics renames The_Detached_Metrics_Data.Storage;


  procedure Save_Detached_Metrics (Metrics : Window_Metrics) is
  begin
    The_Detached_Metrics := Metrics;
  end Save_Detached_Metrics;


  function Create (Title           : String;
                   Initial_Metrics : Window_Metrics := Default_Metrics) return Detached_Window is
  begin
    if The_Detached_Metrics = Default_Metrics then
      The_Detached_Metrics := Initial_Metrics;
    end if;
    return Create (Title, The_Detached_Metrics, Save_Detached_Metrics'access);
  end Create;

begin
  if Persistent_Detached_Metrics.Storage_Is_Empty then
    The_Detached_Metrics := Default_Metrics;
  end if;
end Gui.Persistent;
