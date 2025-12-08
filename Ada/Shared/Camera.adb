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
with Camera.Raw;

with Traces;

package body Camera is

  package Log is new Traces ("Camera");

  procedure Start is
  begin
    Log.Write ("Start");
    Canon.Start_Control;
  end Start;


  function Actual_Information return Information is
  begin
    return Canon.Actual_Info;
  end Actual_Information;


  procedure Capture (Filename : String;
                     Time     : Exposure.Item    := Exposure.From_Camera;
                     Iso      : Sensitivity.Item := Sensitivity.From_Camera) is
  begin
    Log.Write ("Capture " & Filename & " - Time: " & Time'image & " - Iso: " & Iso'image);
    Canon.Capture_Picture (Filename, Time, Iso);
  end Capture;


  procedure Capture (Size : Square_Size;
                     Time : Exposure.Item := Exposure.From_Camera;
                     Iso  : Sensitivity.Item := Sensitivity.From_Camera) is
  begin
    Log.Write ("Capture - Size:" & Size'image & " - Time: " & Time'image & " - Iso: " & Iso'image);
    Canon.Capture_Grid (Size, Time, Iso);
  end Capture;


  function Captured return Green_Grid is
  begin
    return Canon.Captured_Grid;
  end Captured;


  function Image_Height return Rows is
  begin
    return Raw.Height;
  end Image_Height;


  function Image_Width return Columns is
  begin
    return Raw.Width;
  end Image_Width;


  procedure Stop is
  begin
    Log.Write ("Stop");
    Canon.Stop_Capture;
  end Stop;


  function Error_Message return String is
  begin
    return Canon.Last_Error_Message;
  end Error_Message;


  procedure Finish is
  begin
    Log.Write ("Finish");
    Canon.End_Control;
  end Finish;

end Camera;
