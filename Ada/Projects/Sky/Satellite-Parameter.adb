-- *********************************************************************************************************************
-- *                               (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Section;

package body Satellite.Parameter is

  Groups_Key : constant String := "Groups";

  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Id));
    Set_Groups (Section.String_Of (Groups_Key, Id));
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put ("[" & Id & "]");
    Put (Groups_Key & " = " & Image_Of ([Stations, Visual]));
  end Defaults;

end Satellite.Parameter;
