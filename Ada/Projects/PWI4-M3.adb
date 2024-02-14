-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with PWI4.Protocol;

package body PWI4.M3 is

  function Exists return Boolean is
  begin
    return Protocol.M3.Info.Exists;
  end Exists;


  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "m3",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  procedure Turn (To : Port) is
  begin
    Execute (Command_Name => "goto",
             Parameters   => "port=" & (case To is when Port_1 => "1", when Port_2 => "2"));
  end Turn;


  function Actual_Port return M3_Port is
    Data : constant Protocol.M3_Info := Protocol.M3.Info;
    use type Protocol.Port_Number;
  begin
    if Data.Exists then
      case Data.Port is
      when 0 | -1 =>
        return Between;
      when 1 =>
        return Port_1;
      when 2 =>
        return Port_2;
      end case;
    end if;
    return Unknown;
  end Actual_Port;

end PWI4.M3;
