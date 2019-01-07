-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package Device is

  type Extended_Drive is (D1, D2, D3);

  subtype Drive is Extended_Drive range D1 .. D2;

  type State is (Disconnected,
                 Startup,      -- power up or session timeout
                 Initialized,  -- processor initialized
                 Stopped,      -- telescope stopped (directions known)
                 Synchronised, -- telescope Synchronised (handling of updates)
                 Moving,       -- telescope moving
                 Fault);       -- telescope fault detected

  Stopping : constant State := Moving;

  type Time_Synch_State is (Idle,          -- no synchronization
                            Waiting,       -- waiting for low synch signal low
                            Started,       -- start (synch signal high)
                            Measuring,     -- synch signal low
                            Ready,         -- synch duration mesuared (signal High)
                            Synchronised); -- time synchronized (minutes addred)

  type Command is (No_Command,
                   Move_Left,
                   Move_Right,
                   Move_Up,
                   Move_Down,
                   Increase,
                   Decrease,
                   Enter,
                   Stop,
                   Set_Guiding_Rate,
                   Set_Centering_Rate,
                   Set_Finding_Rate,
                   Set_Slewing_Rate);

  type Steps_Per_Revolution is array (Drive) of Positive;

end Device;
