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
pragma Style_Astronomy;

package GPIO is

-----------
-- Types --
-----------

  type Line is (Line2,  Line3,  Line4,  Line5,  Line6,  Line7,  Line8,  Line9,  Line10, Line11, Line12, Line13, Line14,
                Line15, Line16, Line17, Line18, Line19, Line20, Line21, Line22, Line23, Line24, Line25, Line26, Line27);

  type Level is (Low, High);

----------------
-- Exceptions --
----------------

  Device_Error : exception;
  In_Use       : exception;
  Not_In_Use   : exception;
  Released     : exception;
  Usage_Error  : exception;

-----------------------------
-- Static Input Operations --
-----------------------------

  procedure Request_Static_Input (Item : Line);

  function Level_Of (Item : Line) return Level;

------------------------------
-- Dynamic Input Operations --
------------------------------

  procedure Request_Dynamic_Input (Item : Line);

  procedure Await_Change_To_High (On : Line);
  -- raises Released when released.

  procedure Await_Change_To_Low (On : Line);
  -- raises Released when released.

-----------------------
-- Output Operations --
-----------------------

  procedure Request_Output (Item          : Line;
                            Initial_Value : Level := Low);

  procedure Set (Item : Line;
                 To   : Level);

-----------------------
-- Release Operation --
-----------------------

  procedure Release (Item : Line);

end GPIO;
