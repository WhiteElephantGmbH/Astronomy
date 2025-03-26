-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package PWI4.Focuser is

  function Exists return Boolean;

  function Connected return Boolean;

  function Moving return Boolean;

  function Actual_Position return Microns;

  procedure Connect (Device : Device_Index := Default_Device);

  procedure Disconnect (Device : Device_Index := Default_Device);

  procedure Find_Home (Device : Device_Index := Default_Device);

  procedure Go_To (Position : Microns;
                   Device   : Device_Index := Default_Device);

  procedure Stop (Device : Device_Index := Default_Device);

end PWI4.Focuser;
