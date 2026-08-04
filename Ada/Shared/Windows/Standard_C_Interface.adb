-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
-- *                              Interface to the Standard C library  (Linux simulation)                              *
-- *********************************************************************************************************************
pragma Style_Astronomy;

package body Standard_C_Interface is

  function Wait_Select (Nfds       : Fd_Number;
                        Read_Fds   : access Fd_Set;
                        Write_Fds  : access Fd_Set := null;
                        Except_Fds : access Fd_Set := null;
                        Timeout    : access Timeval := null) return Return_Count is
    pragma Unreferenced (Nfds, Read_Fds, Write_Fds, Except_Fds, Timeout);
  begin
    return Failed;
  end Wait_Select;

end Standard_C_Interface;
