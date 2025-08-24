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

with Celestron.Focuser;
with Http_Server.HPS;

package body Handbox.HPS is

  package Focuser renames Celestron.Focuser;

  Is_Moving : Boolean;


  procedure Execute (The_Command : Focuser.Command) renames Http_Server.HPS.Execute;


  procedure Handle (The_Command : Command) is
  begin
    if Is_Moving then
      case The_Command is
      when Left_Released | Right_Released | Stop =>
        Execute (Focuser.Stop);
        Is_Moving := False;
      when others =>
        null;
      end case;
    else
      case The_Command is
      when Up_Pressed =>
        Execute (Focuser.Increase_Rate);
      when Down_Pressed =>
        Execute (Focuser.Decrease_Rate);
      when Left_Pressed =>
        Execute (Focuser.Move_In);
        Is_Moving := True;
      when Right_Pressed =>
        Execute (Focuser.Move_Out);
        Is_Moving := True;
      when others =>
        null;
      end case;
    end if;
  end Handle;

end Handbox.HPS;
