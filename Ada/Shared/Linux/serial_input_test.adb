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
with Ada.Text_IO;
with Interfaces.C.Strings;
with Standard_C_Interface;
with Termios_Interface;

package body Serial_Input_Test is

  package C renames Interfaces.C;
  package I renames Standard_C_Interface;
  package T renames Termios_Interface;

  package IO renames Ada.Text_IO;

  Serial_FD : I.File_Descriptor;
  Tio       : aliased T.Termios;

  use type I.File_Descriptor;
  use type I.Return_Code;
  use type T.Tcflag_T;
  
  procedure execute is
  begin
    IO.Put_Line ("Opening /dev/ttyACM0...");
    Serial_FD := I.Open (C.Strings.New_String("/dev/ttyACM0"), I.Read_Write, 0);

    if Serial_FD = I.Not_Opened then
      IO.Put_Line ("Failed to open serial port.");
      return;
    end if;

    if T.Tcgetattr (Serial_FD, Tio'access) /= I.Success then
      IO.Put_Line ("tcgetattr failed");
      return;
    end if;

    Tio.C_Iflag := 0;
    Tio.C_Oflag := 0;
    Tio.C_Lflag := 0;
    Tio.C_Cflag := T.CS8 + T.CREAD + T.CLOCAL;

    if T.Cfsetspeed (Tio'access, T.B19200) /= I.Success then
      IO.Put_Line ("Setting baud rate failed");
      return;
    end if;

    if T.Tcsetattr (Serial_FD, T.TCSANOW, Tio'access) /= 0 then
      IO.Put_Line ("tcsetattr failed");
      return;
    end if;

    IO.Put_Line ("Serial port configured.");

    -- Loop to read with timeout
    declare
      Timeout : aliased I.Timeval := (Sec => 1, Usec => 0);
      Set     : aliased I.Fd_Set := [others => False];
      Buffer  : aliased Character;
      Count   : I.Return_Count;
    begin
      loop
        declare
          The_Result   : I.Return_Count;
          Unused       : Character;
          Is_Available : Boolean;
        begin
          Set(Serial_Fd) := True;
          The_Result := I.Wait_Select (Serial_FD + I.Fd_Number(1),
                                       Read_Fds => Set'access,
                                       Timeout  => Timeout'access);
          if The_Result = I.Failed then
            IO.Put_Line ("select() failed.");
            exit;
          elsif The_Result = 0 then
            Ada.Text_IO.Get_Immediate (Unused, Is_Available);
            if Is_Available then
              IO.Put_Line (" pressed");
              exit;
            end if;
          else
            Count := I.Read (Serial_FD, Buffer'address, 1);
            if Count = 1 then
              IO.Put_Line ("Received: '" & Buffer & "'");
            end if;
          end if;
        end;
      end loop;

      IO.Put_Line ("Closing serial port...");
      if I.Close(Serial_FD) /= I.Success then
        IO.Put_Line("Failed to close serial port.");
      else
        IO.Put_Line("Serial port closed.");
      end if;
    end;
  end Execute;

end Serial_Input_Test;
