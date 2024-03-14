-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Network;
with Text;

package Remote is

  function Configured return Boolean;

  procedure Start;

  procedure Define (Target : String);

  procedure Define (Is_On_Target : Boolean);

  type Command is (Start_Session, Generate_Qr_Code, End_Session);

  procedure Execute (The_Command : Command);

  procedure Close;

private

  Id : constant String := "Remote";

  The_Telescope_Name : Text.String;
  The_Remote_Address : Network.Ip_Address;
  The_Remote_Port    : Network.Port_Number;

end Remote;
