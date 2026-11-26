-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Persistent;

package body Gui.Registered is

  package Persistent_Metrics is new Persistent (Window_Metrics, "Metrics");

  procedure Execute (The_Application_Name    : String;
                     The_Version             : String := "";
                     The_Startup_Routine     : access procedure;
                     The_Termination_Routine : access procedure := null;
                     Initial_Metrics         : Window_Metrics := Default_Metrics;
                     Always_Topmost          : Boolean := False) is

    The_Metrics_Data : Persistent_Metrics.Data;
    The_Metrics      : Window_Metrics renames The_Metrics_Data.Storage;

    procedure Termination is
    begin
      if not (Application_Is_Minimized or Application_Is_Maximized) then
        The_Metrics := Get_Window_Metrics;
      end if;
      if The_Termination_Routine /= null then
        The_Termination_Routine.all;
      end if;
    end Termination;

  begin -- Execute;
    if Persistent_Metrics.Storage_Is_Empty then
      The_Metrics := Initial_Metrics;
    end if;
    Execute (The_Application_Name & ' ' & The_Version,
             The_Startup_Routine,
             Termination'access,
             The_Metrics,
             Always_Topmost);
  end Execute;


  package Persistent_Detached_Metrics is new Persistent (Window_Metrics, "Detached_Metrics");

  The_Metrics_Data : Persistent_Detached_Metrics.Data;
  The_Metrics      : Window_Metrics renames The_Metrics_Data.Storage;


  procedure Save_Detached_Metrics (Metrics : Window_Metrics) is
  begin
    The_Metrics := Metrics;
  end Save_Detached_Metrics;


  function Create (Title           : String;
                   Initial_Metrics : Window_Metrics := Default_Metrics) return Detached_Window is
  begin
    if Persistent_Detached_Metrics.Storage_Is_Empty then
      The_Metrics := Initial_Metrics;
    end if;
    return Create (Title, The_Metrics, Save_Detached_Metrics'access);
  end Create;

end Gui.Registered;
