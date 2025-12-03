-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Exposure;
with Sensitivity;

package Camera is

  type Model is (Canon_Eos_6D, Canon_Eos_60D);

  type Status is (Disconnected, Connected, Capturing, Captured);

  type Information is record
    State  : Status;
    Camera : Model;
  end record;

  procedure Start;

  function Actual_Information return Information;

  procedure Capture (Filename : String;
                     Time     : Exposure.Item := Exposure.From_Camera;
                     Iso      : Sensitivity.Item := Sensitivity.From_Camera);

  procedure Stop;

  procedure Finish;

end Camera;
