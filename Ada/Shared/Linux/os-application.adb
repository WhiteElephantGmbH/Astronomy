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

package body Os.Application is


  function Is_First_Instance return Boolean is
  begin
    return True;
  end Is_First_Instance;


  function Origin_Folder return String is
  begin
    return "";
  end Origin_Folder;


  function Name return String is
  begin
    return "";
  end Name;


  function Main_Version return String is
  begin
    return "";
  end Main_Version;


  function Version return String is
  begin
    return "";
  end Version;


  function Version return Unsigned.Quadword is
  begin
    return 0;
  end Version;

end Os.Application;
