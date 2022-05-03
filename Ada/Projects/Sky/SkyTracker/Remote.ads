-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package Remote is

  procedure Start (Telescope_Name : String;
                   Ip_Address     : Network.Ip_Address;
                   Port           : Network.Port_Number);

  procedure Define (Target : String);

  procedure Define (Is_On_Target : Boolean);

  type Command is (Start_Session, Generate_Qr_Code, End_Session);

  procedure Execute (The_Command : Command);

  procedure Close;

end Remote;
