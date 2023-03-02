-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

private with Ada.Text_IO;

package Generator is

  procedure Execute;

private

  package IO renames Ada.Text_IO;

  procedure Put_Line (Text : String) renames IO.Put_Line;

  Simbad_Folder : constant String := "H:\Source\Astronomy\Simbad\";
  Data_Folder   : constant String := "H:\Source\Astronomy\Ada\SkyData\";

  The_Line_Number : Natural;

  Error_Occured : exception;

  procedure Error (Text : String) with No_Return;

end Generator;
