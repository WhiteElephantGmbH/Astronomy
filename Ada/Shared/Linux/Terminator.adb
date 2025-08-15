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

with Interfaces.C;

package body Terminator is

  package C renames Interfaces.C;

  use type SI.Return_Code;
  use type SI.File_Descriptor;


  procedure Initialize  (Handle : in out Trigger) is
    Fds : aliased SI.Pipe_Fds;
  begin
    if SI.Pipe (Fds'access) /= SI.Success then
      raise Program_Error with "pipe() failed";
    end if;
    Handle.Fds := Fds;
  end Initialize;


  function Read_Fd (Handle : Trigger) return SI.File_Descriptor is
  begin
    return Handle.Fds.Read_Fd;
  end Read_Fd;


  procedure Signal (Handle : Trigger) is
    B     : aliased C.char := 'X';
    Dummy : SI.Return_Count;
  begin
    if Handle.Fds.Write_Fd /= SI.Not_Opened then
      Dummy := SI.Write (Handle.Fds.Write_Fd, B'address, 1);
    end if;
  end Signal;


  procedure Clear (Handle : Trigger) is
    B     : aliased C.char;
    Dummy : SI.Return_Count;
  begin
    if Handle.Fds.Read_Fd /= SI.Not_Opened then
      Dummy := SI.Read (Handle.Fds.Read_Fd, B'address, 1);
    end if;
  end Clear;


  procedure Finalize (Handle : in out Trigger) is
    Dummy : SI.Return_Code;
  begin
    if Handle.Fds.Read_Fd /= SI.Not_Opened then
      Dummy := SI.Close (Handle.Fds.Read_Fd);
    end if;
    if Handle.Fds.Write_Fd >= 0 then
      Dummy := SI.Close (Handle.Fds.Write_Fd);
    end if;
    Handle.Fds.Read_Fd  := SI.Not_Opened;
    Handle.Fds.Write_Fd := SI.Not_Opened;
  end Finalize;

end Terminator;

