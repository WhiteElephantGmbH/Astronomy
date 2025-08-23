-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Traces;

package body Command is

  package Log is new Traces ("Command");

  procedure Execute (Item : Focuser.Command) is
  begin
    Log.Write ("Execute:" & Item'image);
    Focuser.Execute (Item);
  end Execute;


  procedure Move_To (Position : Focuser.Distance) is
  begin
    Log.Write ("Move to" & Position'image);
    Focuser.Move_To (Position);
  end Move_To;


  procedure Set (Backlash : Focuser.Lash) is
  begin
    Log.Write ("Set backlash" & Backlash'image);
    Focuser.Set (Backlash);
  end Set;

begin
  Focuser.Start;
end Command;
