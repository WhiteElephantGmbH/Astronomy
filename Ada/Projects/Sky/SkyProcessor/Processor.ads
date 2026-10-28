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

with Image;
with Space;
with Text;

package Processor is

  type Dark_Parameters is record
    Id : Text.String;
  end record;

  type Light_Parameters is record
    Target   : Space.Direction;
    Darks_Id : Text.String;
  end record;

  procedure Start;

  procedure Evaluate_Darks (Filename : String);

  procedure New_Sequence (Parameters : Light_Parameters);

  procedure Set (Index : Image.Number);

  procedure Stop;

  procedure Shutdown;

end Processor;
