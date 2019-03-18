-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Environment_Variables;
with GNAT.Sockets;

package body Os is

  function Computer_Name return String is
  begin
    return GNAT.Sockets.Host_Name;
  exception
  when others =>
    return "HostName";
  end Computer_Name;


  function User_Name return String is
  begin
    return Ada.Environment_Variables.Value ("USER");
  exception
  when others =>
    return "UserName";
  end User_Name;


  function Is_Shutting_Down return Boolean is
  begin
    return False;
  end Is_Shutting_Down;


  function Family return Family_Name is
  begin
    return Linux;
  end Family;


  function Is_Linux return Boolean is
  begin
    return True;
  end Is_Linux;  


  function Is_Osx return Boolean is
  begin
    return False;
  end Is_Osx;  


  function Is_Windows return Boolean is
  begin
    return False;
  end Is_Windows;

end Os;
