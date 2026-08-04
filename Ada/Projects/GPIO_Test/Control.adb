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

with Ada.Text_IO;
with Exceptions;
with GPIO;
with Time;

package body Control is

  task Handler is
    entry Wait_For_End;
  end Handler;

  task body Handler is
    use type GPIO.Level;
  begin
    GPIO.Request_Output (GPIO.Line22);
    GPIO.Request_Dynamic_Input (GPIO.Line27);
    loop
      GPIO.Await_Change_To_High (On => GPIO.Line27);
      GPIO.Set (GPIO.Line22, GPIO.High);
      Time.Wait (0.1);
      GPIO.Await_Change_To_Low (On => GPIO.Line27);
      Time.Wait (0.1);
      GPIO.Set (GPIO.Line22, GPIO.Low);
    end loop;
  exception
  when GPIO.Released =>
    accept Wait_For_End;
  when Item: others =>
    Ada.Text_IO.Put_Line (Exceptions.Information_Of (Item));
    accept Wait_For_End;
  end Handler;

  procedure Start is
  begin
    GPIO.Request_Output (GPIO.Line17);
    for Unused in 1 .. 10 loop
      GPIO.Set (GPIO.Line17, GPIO.High);
      Time.Wait (0.5);
      GPIO.Set (GPIO.Line17, GPIO.Low);
      Time.Wait (0.5);
    end loop;
    GPIO.Release (GPIO.Line27);
    Handler.Wait_For_End;
  end Start;

end Control;
