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

with Camera.Canon;
with Traces;

package body Camera is

  package Log is new Traces (Id);

  procedure Start is
  begin
    Canon.Startup;
  end Start;

  procedure Define_Picture (Filename : String) is
  begin
    Canon.Define (Filename);
  end Define_Picture;


  procedure Capture (Time : Exposure.Item    := Exposure.From_Camera;
                     Iso  : Sensitivity.Item := Sensitivity.From_Camera) is
  begin
    Log.Write ("capture started");
    Canon.Capture_Picture (Time, Iso);
  end Capture;


  procedure Stop is
  begin
    --Canon.Stop_Capture; !!!
    Log.Write ("Capture stopped");
  end Stop;

  procedure Finish is
  begin
    Canon.Shutdown;
  end Finish;

end Camera;
