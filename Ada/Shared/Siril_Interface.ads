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

with Siril_Progress;

generic
  Name : String;
package Siril_Interface is

  subtype Percent is Siril_Progress.Percent;

  subtype Progress_Handler is Siril_Progress.Handler;

  procedure Open (Progress : Progress_Handler := null);

  procedure Execute (Command : String);

  procedure Close;

  procedure Abort_And_Close;

  Aborted              : exception;
  Fatal_Error          : exception;
  Resource_Error       : exception;
  Not_Enough_Stars     : exception;
  Plate_Solving_Failed : exception;
  Status_Error         : exception;
  Protocol_Error       : exception;

end Siril_Interface;
