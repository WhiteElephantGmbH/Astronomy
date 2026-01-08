-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Focus;

package Celestron.Focuser is

  type Command is (Decrease_Rate, Increase_Rate, Move_In, Move_Out, Home, Stop);

  procedure Start;

  subtype Distance is Focus.Distance;

  type Lash is new Distance range 0 .. 2**8 - 1;

  subtype Rate is Natural range 1 .. 4;

  Default_Port_Number   : constant := 12000;
  Default_Home_Position : constant := 20376;
  Default_Backlash      : constant := 25;

  Get_Data_Command : constant String := "get_data";
  Execute_Command  : constant String := "execute";
  Move_To_Command  : constant String := "move_to";
  Set_Home_Command : constant String := "set_home";
  Set_Lash_Command : constant String := "set_lash";
  Shutdown_Command : constant String := "shutdown";

  type Data is record
    Exists   : Boolean := False;
    Moving   : Boolean := False;
    Position : Distance := Distance'last;
    Home     : Distance := Distance'last;
    Backlash : Lash := Lash'last;
    Speed    : Rate := Rate'first;
  end record;

  No_Data : constant Data := (others => <>);

  function Exists return Boolean;

  function Moving return Boolean;

  function Home_Position return Distance;

  function Backlash return Lash;

  function Position return Distance;

  function Speed return Rate;

  procedure Execute (Item : Command);

  procedure Move_To (Item : Distance);

  procedure Set_Home (Item : Distance);

  procedure Set (Item : Lash);

  procedure Finish;

end Celestron.Focuser;
