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

private with Ada.Finalization;
with Standard_C_Interface;

package Terminator is

  package SI renames Standard_C_Interface;

  type Trigger is tagged limited private;

  function Read_Fd (Handle : Trigger) return SI.File_Descriptor;

  procedure Signal (Handle : Trigger);

  procedure Clear (Handle : Trigger);

private

  type Trigger is new Ada.Finalization.Limited_Controlled with record
    Fds : SI.Pipe_Fds := (Read_Fd  => SI.Not_Opened,
                          Write_Fd => SI.Not_Opened);
  end record;

  overriding
  procedure Initialize (Handle : in out Trigger);

  overriding
  procedure Finalize (Handle : in out Trigger);

end Terminator;

