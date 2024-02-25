-- *********************************************************************************************************************
-- *                       (c) 2022 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Network;
with Progress;

package Cdk_700 is

  procedure Startup;
    ENC_Not_Available : exception;
    Startup_Failed    : exception;

  function Startup_Progress return Progress.Percent;

  function Had_Powerup return Boolean;
  -- Precondition: Startup must have been called

  function Is_Started return Boolean;
  -- Precondition: Startup must have been called

  function Is_Simulated return Boolean;
  -- Precondition: Startup must have been called

private
  The_Ip_Address       : Network.Ip_Address;
  The_Restart_Duration : Duration;
end Cdk_700;
